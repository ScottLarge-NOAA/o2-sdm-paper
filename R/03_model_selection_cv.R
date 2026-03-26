# ==============================================================================
# 03_model_selection_cv.R
# Purpose: Model selection via Leave-One-Year-Out CV matching Indivero et al.
#          Evaluates 12 candidate SDMs using a Gaussian (Identity) framework.
#          Hardened for parallel Unix environments with dynamic load-balancing.
# ==============================================================================

library(sdmTMB)
library(dplyr)
library(purrr)
library(recipes)
library(rsample)
library(furrr)
library(future)
source(here::here("R", "00_config_o2.R"))

message("--- Starting Model Selection (Gaussian Architecture) ---")

# --- 0. SETUP CACHE -----------------------------------------------------------
model_cache_dir <- file.path(out_dir, "model_cache")
if (!dir.exists(model_cache_dir)) dir.create(model_cache_dir, recursive = TRUE)

# --- 1. DATA PREP & RECIPES ---------------------------------------------------
dat_filtered <- readRDS(file.path(der_dir, "all_o2_dat_filtered.rds"))

dat_cleaned <- dat_filtered %>%
  # Drop NAs for all variables used in ANY phase of the modeling pipeline
  tidyr::drop_na(depth, o2, temp, sigma0, doy, X_km, Y_km, year) %>%  
  filter(depth > 0, o2 > 0, o2 < 1500) %>%  
  mutate(
    sigma0 = if_else(sigma0 <= 24, 24, sigma0),
    depth_ln = log(depth),
    o2_scaled = o2 / 100 # Gaussian Scaling
  ) %>% 
  # Restrict analysis to years with enough data to support a CV fold
  group_by(year) %>%  
  filter(n() > 50) %>%  
  ungroup()

# Generate CV Fold IDs (Leave-One-Year-Out)
year_map <- dat_cleaned %>% distinct(year) %>% arrange(year) %>% mutate(fold_id = row_number())
dat_cleaned <- dat_cleaned %>% left_join(year_map, by = "year")

# 80/20 Split
set.seed(123)
dat_split <- rsample::initial_split(dat_cleaned, prop = 0.8) 
training_data <- rsample::training(dat_split) 

# BULLETPROOF RECIPE:
# We explicitly update the role of metadata columns to "ID". 
# This prevents them from being dropped OR mathematically transformed.
my_recipe <- recipes::recipe(o2_scaled ~ temp + sigma0 + depth_ln + doy + year + X_km + Y_km + fold_id, 
                             data = training_data) %>%
  recipes::update_role(year, X_km, Y_km, fold_id, new_role = "id") %>%
  recipes::step_normalize(temp, sigma0, depth_ln)

rec_prepped <- recipes::prep(my_recipe, training = training_data)
baked_training_data <- recipes::bake(rec_prepped, new_data = training_data)
saveRDS(rec_prepped, file.path(der_dir, "o2_recipe.rds"))

# Build spatial mesh
spde <- sdmTMB::make_mesh(data = baked_training_data, xy_cols = c("X_km", "Y_km"), n_knots = 250)

# --- 2. CANDIDATE MODELS (Table 2) --------------------------------------------
model_frame <- data.frame(
  equation = paste0("m", 1:12),
  spatial = c(rep("off", 3), rep("on", 9)),
  annual = c(rep("off", 6), rep("on", 3), rep("off", 3)),
  spatiotemporal = c(rep("off", 9), rep("ar1", 3)),
  temp = rep(c("off", "on", "on"), 4),
  sal = rep(c("off", "off", "on"), 4)
) %>%
  mutate(
    formula = purrr::pmap(list(annual, temp, sal), function(a, t, s) {
      # Base terms used in every model
      terms <- c("s(depth_ln)", "s(doy, bs = 'cc')")
      # Optional terms based on matrix
      if (t == "on") terms <- c(terms, "s(temp)")
      if (s == "on") terms <- c(terms, "s(sigma0)")
      
      # Handle intercept: Remove global ("0") if time-varying ("annual") is active
      intercept <- if (a == "on") "0" else "1"
      
      # Construct final formula string
      rhs <- paste(c(intercept, terms), collapse = " + ")
      as.formula(paste("o2_scaled ~", rhs))
    })
  )

# --- 3. EXECUTION LOGIC -------------------------------------------------------
run_models <- function(formula, spatial, spatiotemporal, annual, data_input) {
  
  # Core SDM Arguments
  args <- list(
    formula = formula,
    data = data_input,
    mesh = spde,
    family = gaussian(), 
    spatial = spatial,
    spatiotemporal = spatiotemporal,
    k_folds = length(unique(data_input$year)),
    fold_ids = data_input$fold_id,
    priors = operational_priors,
    control = sdmTMBcontrol(newton_loops = 1), # Insurance for AR1 convergence
    parallel = FALSE # Internal parallelism turned off to avoid fighting furrr
  )
  
  # Time component logic
  if (annual == "on" || spatiotemporal != "off") {
    args$time <- "year"
    all_years <- tidyr::full_seq(data_input$year, period = 1)
    args$extra_time <- setdiff(all_years, unique(data_input$year))
  }
  
  # Re-inject random walk intercept if annual = 'on'
  if (annual == "on") {
    args$time_varying <- as.formula("~ 1")
  }
  
  do.call(sdmTMB::sdmTMB_cv, args)
}

# The Thread-Safe Wrapper
run_and_save_model <- function(formula, spatial, spatiotemporal, annual, model_id, data_input) {
  
  file_name <- file.path(model_cache_dir, paste0(model_id, ".rds"))
  
  if (file.exists(file_name)) return(readRDS(file_name))
  
  # Pin down math libraries to 1 thread per worker to prevent "S" Status lockups
  TMB::openmp(n = 1)
  RhpcBLASctl::blas_set_num_threads(1)
  RhpcBLASctl::omp_set_num_threads(1)
  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
  
  result <- tryCatch({
    run_models(formula, spatial, spatiotemporal, annual, data_input = data_input)
  }, error = function(e) {
    paste("Error:", e$message)
  })
  
  saveRDS(result, file = file_name)
  return(result)
}

# --- 4. PARALLEL EXECUTION ----------------------------------------------------
# 1. Check cache for completed models (e.g., m1 through m10)
finished_models <- gsub("\\.rds$", "", list.files(model_cache_dir, pattern = "\\.rds$"))

# 2. Filter matrix to ONLY run what is missing
model_frame_pending <- model_frame %>% filter(!equation %in% finished_models)

message(sprintf(" -> %d cached models found. %d queued for execution.", length(finished_models), nrow(model_frame_pending)))

if(nrow(model_frame_pending) > 0) {
  
  # Provide 10GB RAM overhead for object passing
  options(future.globals.maxSize = 10 * 1024^3) 
  
  # Spawn workers (max 4, or fewer if only 1-3 models are pending)
  plan(multisession, workers = min(4, nrow(model_frame_pending))) 
  
  cv_fits <- furrr::future_pmap(
    # EXPLICIT MAPPING: Map dataframe columns strictly to function arguments
    .l = list(
      formula = model_frame_pending$formula,
      spatial = model_frame_pending$spatial,
      spatiotemporal = model_frame_pending$spatiotemporal, 
      annual = model_frame_pending$annual,
      model_id = model_frame_pending$equation 
    ),
    .f = run_and_save_model, 
    data_input = baked_training_data,
    .options = furrr::furrr_options(
      seed = TRUE,
      scheduling = FALSE, # Enables dynamic load-balancing
      globals = c("run_models", "spde", "model_cache_dir", "operational_priors"),
      packages = c("sdmTMB", "dplyr")
    )
  )
  
  plan(sequential)
}
message("--- CV Complete ---")
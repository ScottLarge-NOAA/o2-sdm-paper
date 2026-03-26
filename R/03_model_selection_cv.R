# ==============================================================================
# 03_model_selection_cv.R
# Purpose: Model selection via Leave-One-Year-Out CV matching Indivero et al.
#          Evaluates 4 hypothesis-driven SDMs using a Gaussian framework.
#          Tests in situ vs reanalysis (GLORYS) stratification proxies.
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

message("--- Starting Model Selection (The Stratification Showdown) ---")

# --- 0. SETUP CACHE -----------------------------------------------------------
model_cache_dir <- file.path(out_dir, "model_cache")
if (!dir.exists(model_cache_dir)) dir.create(model_cache_dir, recursive = TRUE)

# --- 1. DATA PREP & RECIPES ---------------------------------------------------
dat_filtered <- readRDS(file.path(der_dir, "all_o2_dat_filtered.rds"))

dat_cleaned <- dat_filtered %>%
  # Drop NAs for ALL new GLORYS variables to protect the TMB optimizer
  tidyr::drop_na(depth, o2, temp_insitu, temp_glorys, delta_t_glorys, mlotst_glorys, doy, X_km, Y_km, year) %>%  
  filter(depth > 0, o2 > 0, o2 < 1500) %>%  
  mutate(
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
# Include all 4 competing physical variables in the step_normalize function.
my_recipe <- recipes::recipe(o2_scaled ~ temp_insitu + temp_glorys + delta_t_glorys + mlotst_glorys + depth_ln + doy + year + X_km + Y_km + fold_id, 
                             data = training_data) %>%
  recipes::update_role(year, X_km, Y_km, fold_id, new_role = "id") %>%
  recipes::step_normalize(temp_insitu, temp_glorys, delta_t_glorys, mlotst_glorys, depth_ln)

rec_prepped <- recipes::prep(my_recipe, training = training_data)
baked_training_data <- recipes::bake(rec_prepped, new_data = training_data)
saveRDS(rec_prepped, file.path(der_dir, "o2_recipe.rds"))

# Build spatial mesh (Using your pre-established knot settings)
spde <- sdmTMB::make_mesh(data = baked_training_data, xy_cols = c("X_km", "Y_km"), n_knots = 250)


# --- 2. CANDIDATE MODELS (The 4-Model Hypothesis Test) ------------------------
message("Building candidate formulas...")

formulas <- list(
  # Model 1: The "Gold Standard" (In situ baseline)
  m1_insitu = "o2_scaled ~ 1 + s(depth_ln) + s(doy, bs = 'cc') + s(temp_insitu)",
  
  # Model 2: The "Reanalysis Penalty" (Does GLORYS alone hurt performance?)
  m2_glorys = "o2_scaled ~ 1 + s(depth_ln) + s(doy, bs = 'cc') + s(temp_glorys)",
  
  # Model 3: The "Delta-T Fix" (GLORYS + Surface/Bottom Temp difference)
  m3_deltat = "o2_scaled ~ 1 + s(depth_ln) + s(doy, bs = 'cc') + s(temp_glorys) + s(delta_t_glorys)",
  
  # Model 4: The "Mixed Layer Fix" (GLORYS + Mixed Layer Thickness)
  m4_mlotst = "o2_scaled ~ 1 + s(depth_ln) + s(doy, bs = 'cc') + s(temp_glorys) + s(mlotst_glorys)"
)

# Build the execution frame
model_frame <- tibble(
  equation = names(formulas),
  formula_str = unname(formulas)
) %>%
  mutate(formula = purrr::map(formula_str, as.formula))

print(model_frame)


# --- 3. EXECUTION LOGIC -------------------------------------------------------

# Streamlined execution function: Architecture is locked to spatial="on", spatiotemporal="off"
run_models <- function(formula, data_input) {
  
  args <- list(
    formula = formula,
    data = data_input,
    mesh = spde,
    family = gaussian(), 
    spatial = "on",         
    spatiotemporal = "off", 
    k_folds = length(unique(data_input$year)),
    fold_ids = data_input$fold_id,
    priors = operational_priors,
    control = sdmTMBcontrol(newton_loops = 1), 
    parallel = FALSE # Internal parallelism turned off to avoid fighting furrr
  )
  
  do.call(sdmTMB::sdmTMB_cv, args)
}

# The Thread-Safe Wrapper
run_and_save_model <- function(formula, model_id, data_input) {
  
  file_name <- file.path(model_cache_dir, paste0(model_id, ".rds"))
  if (file.exists(file_name)) return(readRDS(file_name))
  
  # Pin down math libraries to 1 thread per worker to prevent "S" Status lockups
  TMB::openmp(n = 1)
  RhpcBLASctl::blas_set_num_threads(1)
  RhpcBLASctl::omp_set_num_threads(1)
  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
  
  result <- tryCatch({
    run_models(formula, data_input = data_input)
  }, error = function(e) {
    paste("Error:", e$message)
  })
  
  saveRDS(result, file = file_name)
  return(result)
}


# --- 4. PARALLEL EXECUTION ----------------------------------------------------
# 1. Check cache for completed models
finished_models <- gsub("\\.rds$", "", list.files(model_cache_dir, pattern = "\\.rds$"))

# 2. Filter matrix to ONLY run what is missing
model_frame_pending <- model_frame %>% filter(!equation %in% finished_models)

message(sprintf(" -> %d cached models found. %d queued for execution.", length(finished_models), nrow(model_frame_pending)))

if(nrow(model_frame_pending) > 0) {
  
  # Provide 10GB RAM overhead for object passing
  options(future.globals.maxSize = 10 * 1024^3) 
  
  # Spawn workers (max 4, since we only have 4 models)
  plan(multisession, workers = 4)
  
  cv_fits <- furrr::future_pmap(
    # EXPLICIT MAPPING: Much cleaner now
    .l = list(
      formula = model_frame_pending$formula,
      model_id = model_frame_pending$equation 
    ),
    .f = run_and_save_model, 
    data_input = baked_training_data,
    .options = furrr::furrr_options(
      seed = TRUE,
      scheduling = FALSE, 
      globals = c("run_models", "spde", "model_cache_dir", "operational_priors"),
      packages = c("sdmTMB", "dplyr")
    )
  )
  
  plan(sequential)
}
message("--- CV Complete ---")

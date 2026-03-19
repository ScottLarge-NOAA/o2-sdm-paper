# ==============================================================================
# 03_model_selection_cv.R
# Purpose: Model selection via Leave-One-Year-Out CV using the Dec 2025 logic.
#          Consistent with X_km/Y_km and hardened for Docker/Unix environments.
# ==============================================================================

library(sdmTMB)
library(dplyr)
library(purrr)
library(recipes)
library(rsample)
library(furrr)
library(future)
source(here::here("R", "00_config_o2.R"))

message("--- Starting Model Selection (Dec 2025 Architecture) ---")

# --- 0. SETUP CACHE -----------------------------------------------------------
model_cache_dir <- file.path(out_dir, "model_cache")
if (!dir.exists(model_cache_dir)) dir.create(model_cache_dir, recursive = TRUE)

# --- 1. DATA PREP -------------------------------------------------------------
dat_filtered <- readRDS(file.path(der_dir, "all_o2_dat_filtered.rds"))

dat_cleaned <- dat_filtered %>%
  tidyr::drop_na(depth, o2, temp, sigma0, doy, X_km, Y_km, year, survey) %>%  
  filter(depth > 0, o2 > 0, o2 < 1500) %>%  
  mutate(
    survey = as.factor(survey),
    sigma0 = if_else(sigma0 <= 24, 24, sigma0),
    depth_ln = log(depth),
    o2_scaled = o2 / 100
  ) %>% 
  group_by(year) %>%  
  filter(n() > 50) %>%  
  ungroup()

# Create Leave-One-Year-Out Fold IDs
year_map <- dat_cleaned %>%
  distinct(year) %>%
  arrange(year) %>%
  mutate(fold_id = row_number())

dat_cleaned <- dat_cleaned %>%
  left_join(year_map, by = "year")

# 80/20 Split for Validation
set.seed(123)
dat_split <- rsample::initial_split(dat_cleaned, prop = 0.8) 
training_data <- rsample::training(dat_split) 

# Recipe Scaling
my_recipe <- recipes::recipe(o2_scaled ~ temp + sigma0 + depth_ln + doy + year + X_km + Y_km + survey, 
                             data = training_data) %>%
  recipes::step_normalize(temp, sigma0, depth_ln)

rec_prepped <- recipes::prep(my_recipe, training = training_data)
baked_training_data <- recipes::bake(rec_prepped, new_data = training_data)
saveRDS(rec_prepped, file.path(der_dir, "o2_recipe.rds"))

# Mesh setup (n_knots = 250 as requested)
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
      terms <- c("s(depth_ln)", "s(doy, bs = 'cc')")
      if (t == "on") terms <- c(terms, "s(temp)")
      if (s == "on") terms <- c(terms, "s(sigma0)")
      intercept <- if (a == "on") "0" else "1"
      # Adding 'survey' as fixed effect for sensor calibration
      rhs <- paste(c(intercept, "survey", terms), collapse = " + ")
      as.formula(paste("o2_scaled ~", rhs))
    })
  )

# --- 3. EXECUTION FUNCTIONS ---------------------------------------------------
run_models <- function(formula, spatial, spatiotemporal, annual, data_input) {
  k_val <- length(unique(data_input$year))
  
  args <- list(
    formula = formula,
    data = data_input,
    mesh = spde,
    family = gaussian(),
    spatial = spatial,
    spatiotemporal = spatiotemporal,
    k_folds = k_val,
    fold_ids = data_input$fold_id,
    parallel = FALSE
  )
  
  if (annual == "on" || spatiotemporal != "off") {
    args$time <- "year"
    all_years <- tidyr::full_seq(data_input$year, period = 1)
    args$extra_time <- setdiff(all_years, unique(data_input$year))
  }
  
  do.call(sdmTMB::sdmTMB_cv, args)
}

run_and_save_model <- function(formula, spatial, spatiotemporal, annual, model_id, data_input) {
  file_name <- file.path(model_cache_dir, paste0(model_id, ".rds"))
  if (file.exists(file_name)) return(readRDS(file_name))
  
  TMB::openmp(n = 1)
  RhpcBLASctl::blas_set_num_threads(1)
  RhpcBLASctl::omp_set_num_threads(1)

  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
  
  
  result <- tryCatch({
    run_models(formula, spatial, spatiotemporal, annual, data_input = data_input)
  }, error = function(e) paste("Error:", e$message))
  
  saveRDS(result, file = file_name)
  return(result)
}

# --- 4. PARALLEL EXECUTION ----------------------------------------------------
message("Launching parallel CV fits...")

# Increase global size to prevent the 500MB socket hang
options(future.globals.maxSize = 10 * 1024^3) # 10GB

plan(multisession, workers = 4)

cv_fits <- furrr::future_pmap(
  .l = list(
    formula = model_frame$formula,
    spatial = model_frame$spatial,
    spatiotemporal = model_frame$spatiotemporal, 
    annual = model_frame$annual,
    model_id = model_frame$equation 
  ),
  .f = run_and_save_model, 
  data_input = baked_training_data,
  .options = furrr::furrr_options(
    seed = TRUE,
    scheduling = 1,
    globals = c("run_models", "spde", "model_cache_dir"),
    packages = c("sdmTMB", "dplyr")
  )
)

plan(sequential)
message("--- CV Complete ---")
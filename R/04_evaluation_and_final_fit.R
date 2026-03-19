# ==============================================================================
# 04_evaluation_and_final_fit.R
# Purpose: Extracts CV metrics, identifies the best predictive model, fits the 
#          final production SDM, and projects results onto the spatial grid.
# ==============================================================================

library(sdmTMB)
library(dplyr)
library(purrr)
library(recipes)
library(ggplot2)
library(sf)
source(here::here("R", "00_config_o2.R"))

message("--- Starting Model Evaluation & Final Fit ---")

model_cache_dir <- file.path(out_dir, "model_cache")

# --- 1. EXTRACT CROSS-VALIDATION METRICS --------------------------------------
message("Evaluating cached cross-validation models...")

# Define the 12 candidate models (Needed for the loop matching)
model_levels <- paste0("m", 1:12)

# Helper function to safely extract metrics from a cached model
get_cv_metrics <- function(model_id) {
  file_path <- file.path(model_cache_dir, paste0(model_id, ".rds"))
  
  if (!file.exists(file_path)) return(tibble(equation = model_id, status = "Missing"))
  
  cv_obj <- readRDS(file_path)
  if (is.character(cv_obj)) return(tibble(equation = model_id, status = "Failed", error = cv_obj))
  
  # Check if all folds converged and pass gradient sanity checks
  fold_fits <- cv_obj$models
  all_converged <- all(map_lgl(fold_fits, ~ .x$model$convergence == 0))
  all_sane <- all(map_lgl(fold_fits, function(x) {
    s <- try(sdmTMB::sanity(x, silent = TRUE), silent = TRUE)
    if (inherits(s, "try-error")) FALSE else s$all_ok
  }))
  
  # Calculate overall RMSE and MAE in scaled space
  df <- cv_obj$data
  errors <- df$o2_scaled - exp(df$cv_predicted) # exp() because of log-link
  
  tibble(
    equation = model_id,
    status = "Success",
    converged = all_converged,
    sane = all_sane,
    rmse = sqrt(mean(errors^2, na.rm = TRUE)),
    mae = mean(abs(errors), na.rm = TRUE)
  )
}

# Compile results table
results_frame <- map_dfr(model_levels, get_cv_metrics) %>% arrange(rmse)
print(results_frame)

# Identify the winner (Lowest RMSE that is mathematically sane)
best_model_row <- results_frame %>% 
  filter(status == "Success", sane == TRUE) %>% 
  slice(1)

if(nrow(best_model_row) == 0) stop(" [X] CRITICAL: No models successfully converged and passed sanity checks.")

winning_model_id <- best_model_row$equation
message(sprintf(" [+] WINNER: %s with an overall RMSE of %.4f", winning_model_id, best_model_row$rmse))

# --- 2. FIT THE FINAL PRODUCTION MODEL ----------------------------------------
message("Fitting the final production model using the winning structure...")

# Load Data and Recipe
dat <- readRDS(file.path(der_dir, "all_o2_dat_filtered.rds"))
rec_prepped <- readRDS(file.path(der_dir, "o2_recipe.rds"))

# Apply exact same filtering and scaling as the CV script
dat_cleaned <- dat %>%
  tidyr::drop_na(depth, o2, temp, sigma0, doy, X_km, Y_km, year) %>% 
  filter(depth > 0, o2 >= 0, o2 <= 1500) %>% 
  mutate(sigma0 = if_else(sigma0 <= 24, 24, sigma0), depth_ln = log(depth), o2_scaled = o2 / 100) %>% 
  group_by(year) %>% filter(n() > 50) %>% ungroup()

baked_full_data <- recipes::bake(rec_prepped, new_data = dat_cleaned)

# Build Final Mesh
spde_full <- sdmTMB::make_mesh(data = baked_full_data, xy_cols = c("X_km", "Y_km"), n_knots = 250)

# Extract the winning formula/settings from the best CV object
winning_cv_obj <- readRDS(file.path(model_cache_dir, paste0(winning_model_id, ".rds")))
winning_formula <- winning_cv_obj$models[[1]]$formula
has_spatial <- winning_cv_obj$models[[1]]$spatial
has_spatiotemporal <- winning_cv_obj$models[[1]]$spatiotemporal

# HPC Thread Locks for the final fit
TMB::openmp(n = 1) 
RhpcBLASctl::blas_set_num_threads(1)
RhpcBLASctl::omp_set_num_threads(1)

# Fit Model
final_fit <- sdmTMB(
  formula = winning_formula,
  data = baked_full_data,
  mesh = spde_full,
  time = ifelse(has_spatiotemporal != "off" || grepl("1 \\| year", deparse(winning_formula)), "year", NULL),
  family = tweedie(link = "log"), 
  spatial = has_spatial,
  spatiotemporal = has_spatiotemporal
)

sanity(final_fit)
saveRDS(final_fit, file.path(out_dir, "final_do_model.rds"))
message(" [+] Final model successfully optimized and saved!")

# --- 3. SPATIAL PREDICTION (PROJECTING THE FIELD) -----------------------------
# Optional: Let's project this onto your map for the most recent year of data
message("Projecting estimates onto the EPU prediction grid...")

target_year <- max(dat_cleaned$year)
grid_path <- file.path(der_dir, "epu_grid.rds") # From your thermal-niche pipeline!

if(file.exists(grid_path)) {
  pred_grid <- readRDS(grid_path) %>% 
    mutate(
      year = target_year,
      doy = 180, # Mid-summer projection (can adjust)
      survey = "ecomon", # Standardize to scientific equipment baseline
      depth_ln = log(depth),
      # Use regional averages for covariates if we don't have dynamic rasters
      temp = mean(dat_cleaned$temp, na.rm = TRUE),
      sigma0 = mean(dat_cleaned$sigma0, na.rm = TRUE)
    )
  
  # Normalize grid variables using the EXACT same recipe
  baked_grid <- recipes::bake(rec_prepped, new_data = pred_grid)
  
  # Predict
  predictions <- predict(final_fit, newdata = baked_grid)
  
  # Unscale the output back to real umol/kg
  predictions <- predictions %>% 
    mutate(o2_umol_kg = exp(est) * 100)
  
  # Save grid
  saveRDS(predictions, file.path(out_dir, paste0("o2_predictions_", target_year, ".rds")))
  message(" [+] Predictions complete and saved!")
} else {
  message(" [!] Prediction grid not found. Skipping spatial projection.")
}
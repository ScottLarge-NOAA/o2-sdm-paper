# ==============================================================================
# 04_evaluation_and_final_fit.R
# Purpose: Extracts CV metrics, identifies best Gaussian model, fits final SDM.
# ==============================================================================

source(here::here("R", "00_config_o2.R"))

message("--- Starting Model Evaluation & Final Fit ---")

model_cache_dir <- file.path(out_dir, "model_cache")

# --- 1. EXTRACT METRICS -------------------------------------------------------
get_cv_metrics <- function(model_id) {
  file_path <- file.path(model_cache_dir, paste0(model_id, ".rds"))
  if (!file.exists(file_path)) return(tibble(equation = model_id, status = "Missing"))
  
  cv_obj <- readRDS(file_path)
  if (is.character(cv_obj)) return(tibble(equation = model_id, status = "Failed", error = cv_obj))
  
  fold_fits <- cv_obj$models
  all_converged <- all(purrr::map_lgl(fold_fits, ~ .x$model$convergence == 0))
  all_sane <- all(purrr::map_lgl(fold_fits, function(x) {
    s <- try(sdmTMB::sanity(x, silent = TRUE), silent = TRUE)
    if (inherits(s, "try-error")) FALSE else s$all_ok
  }))
  
  # Gaussian calculation: Direct difference in scaled space
  errors <- cv_obj$data$o2_scaled - cv_obj$data$cv_predicted 
  
  tibble(
    equation = model_id, status = "Success",
    converged = all_converged, sane = all_sane,
    rmse = sqrt(mean(errors^2, na.rm = TRUE)),
    mae = mean(abs(errors), na.rm = TRUE)
  )
}

results_frame <- purrr::map_dfr(paste0("m", 1:12), get_cv_metrics) %>% arrange(rmse)
print(results_frame)

best_model_row <- results_frame %>% filter(status == "Success", sane == TRUE) %>% slice(1)
if(nrow(best_model_row) == 0) stop(" [X] CRITICAL: No models passed sanity checks.")
message(sprintf(" [+] WINNER: %s with an overall RMSE of %.4f", best_model_row$equation, best_model_row$rmse))

# --- 2. FIT FINAL PRODUCTION MODEL --------------------------------------------
dat <- readRDS(file.path(der_dir, "all_o2_dat_filtered.rds"))
rec_prepped <- readRDS(file.path(der_dir, "o2_recipe.rds"))

dat_cleaned <- dat %>%
  tidyr::drop_na(depth, o2, temp, sigma0, doy, X_km, Y_km, year) %>% 
  filter(depth > 0, o2 > 0, o2 < 1500) %>% 
  mutate(sigma0 = if_else(sigma0 <= 24, 24, sigma0), depth_ln = log(depth), 
         o2_scaled = o2 / 100,
         fold_id = 1) %>% 
  group_by(year) %>% filter(n() > 50) %>% ungroup()

# Generate full-data mesh
baked_full_data <- recipes::bake(rec_prepped, new_data = dat_cleaned)
spde_full <- sdmTMB::make_mesh(data = baked_full_data, xy_cols = c("X_km", "Y_km"), n_knots = 250)

# Extract specs from winner
winning_cv_obj <- readRDS(file.path(model_cache_dir, paste0(best_model_row$equation, ".rds")))
winning_formula <- winning_cv_obj$models[[1]]$formula
if (is.list(winning_formula)) winning_formula <- winning_formula[[1]] # Strips the list wrapper
has_spatial <- winning_cv_obj$models[[1]]$spatial
has_spatiotemporal <- winning_cv_obj$models[[1]]$spatiotemporal

# TMB::openmp(n = 1); RhpcBLASctl::blas_set_num_threads(1); RhpcBLASctl::omp_set_num_threads(1)

final_fit <- sdmTMB(
  formula = winning_formula,
  data = baked_full_data,
  mesh = spde_full,
  time = if (has_spatiotemporal != "off" || grepl("1 \\| year", deparse(winning_formula))) "year" else NULL,
  family = gaussian(), 
  spatial = has_spatial,
  spatiotemporal = has_spatiotemporal,
  priors = operational_priors,
  control = sdmTMBcontrol(newton_loops = 1)
)

if (grepl("1 \\| year", deparse(winning_formula))) final_fit <- update(final_fit, time_varying = ~ 1, time = "year")

sanity(final_fit)
saveRDS(final_fit, file.path(out_dir, "final_do_model.rds"))
message(" [+] Final model optimized and saved!")

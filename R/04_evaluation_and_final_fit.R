# ==============================================================================
# 04_evaluation_and_final_fit.R
# Purpose: Extracts CV metrics, identifies the best stratification model, 
#          and fits the final production SDM on 100% of the training data.
# ==============================================================================

library(furrr)
library(dplyr)
library(sdmTMB)
source(here::here("R", "00_config_o2.R"))

message("--- Starting Model Evaluation & Final Fit ---")

model_cache_dir <- file.path(out_dir, "model_cache")

# Set up parallel processing (max 4 since we only have 4 models)
plan(multisession, workers = min(4, availableCores() - 1))

# --- 1. EXTRACT METRICS (Optimized) -------------------------------------------
get_cv_metrics <- function(model_id) {
  
  # Force the background worker to load the package and its C++ DLLs
  library(sdmTMB)
  
  # Pin down math libraries to 1 thread per worker to prevent "S" Status lockups
  TMB::openmp(n = 1)
  RhpcBLASctl::blas_set_num_threads(1)
  RhpcBLASctl::omp_set_num_threads(1)
  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
  
  file_path <- file.path(model_cache_dir, paste0(model_id, ".rds"))
  if (!file.exists(file_path)) return(tibble(equation = model_id, status = "Missing", converged = FALSE, sane = FALSE, rmse = NA, mae = NA))
  
  cv_obj <- readRDS(file_path)
  if (is.character(cv_obj)) return(tibble(equation = model_id, status = "Failed", error = cv_obj, converged = FALSE, sane = FALSE, rmse = NA, mae = NA))
  
  fold_fits <- cv_obj$models
  
  # Fast convergence check
  all_converged <- all(purrr::map_lgl(fold_fits, ~ .x$model$convergence == 0))
  
  # SMART SANITY CHECK: Short-circuits the moment a single fold fails
  all_sane <- TRUE
  if (all_converged) {
    for (i in seq_along(fold_fits)) {
      s <- try(sdmTMB::sanity(fold_fits[[i]], silent = TRUE), silent = TRUE)
      is_ok <- if (inherits(s, "try-error")) FALSE else s$all_ok
      
      if (!is_ok) {
        all_sane <- FALSE
        break 
      }
    }
  } else {
    all_sane <- FALSE
  }
  
  # Gaussian calculation: Direct difference in scaled space
  # (No need to exponentiate like lognormal models)
  errors <- cv_obj$data$o2_scaled - cv_obj$data$cv_predicted 
  
  tibble(
    equation = model_id, status = "Success",
    converged = all_converged, sane = all_sane,
    rmse = sqrt(mean(errors^2, na.rm = TRUE)),
    mae = mean(abs(errors), na.rm = TRUE)
  )
}

# The 4 specific models from our Stratification Showdown
candidate_models <- c("m1_insitu", "m2_glorys", "m3_deltat", "m4_mlotst")

results_frame <- furrr::future_map_dfr(candidate_models, get_cv_metrics) %>% arrange(rmse)
print(results_frame)

plan(sequential)

# Save the results
saveRDS(results_frame, file.path(out_dir, "cv_model_tournament_results.rds"))
# Pick the winner
# best_model_row <- results_frame %>% filter(status == "Success", sane == TRUE) %>% slice(1)

# FORCE the winner to the best GLORYS model so we can project it across the un-sampled grid
best_model_row <- results_frame %>% filter(equation == "m3_deltat")
if(nrow(best_model_row) == 0) stop(" [X] CRITICAL: No models passed sanity checks.")
message(sprintf(" [+] WINNER: %s with an overall RMSE of %.4f", best_model_row$equation, best_model_row$rmse))


# --- 2. FIT FINAL PRODUCTION MODEL --------------------------------------------
message(sprintf("Fitting final production model using 100%% of data: %s", best_model_row$equation))

dat <- readRDS(file.path(der_dir, "all_o2_dat_filtered.rds"))
rec_prepped <- readRDS(file.path(der_dir, "o2_recipe.rds"))

# Must drop NAs for all competing variables, even if the winning model didn't use them, 
# so the baked grid matches perfectly.
dat_cleaned <- dat %>%
  tidyr::drop_na(depth, o2, temp_insitu, temp_glorys, delta_t_glorys, mlotst_glorys, doy, X_km, Y_km, year) %>% 
  filter(depth > 0, o2 > 0, o2 < 1500) %>% 
  mutate(
    depth_ln = log(depth), 
    o2_scaled = o2 / 100,
    fold_id = 1
  ) %>% 
  group_by(year) %>% filter(n() > 50) %>% ungroup()

# Generate full-data mesh
baked_full_data <- recipes::bake(rec_prepped, new_data = dat_cleaned)
spde_full <- sdmTMB::make_mesh(data = baked_full_data, xy_cols = c("X_km", "Y_km"), n_knots = 250)

# Extract winning formula from CV cache
winning_cv_obj <- readRDS(file.path(model_cache_dir, paste0(best_model_row$equation, ".rds")))
winning_formula <- winning_cv_obj$models[[1]]$formula
if (is.list(winning_formula)) winning_formula <- winning_formula[[1]] # Strips the list wrapper

# Single-threaded math for maximum stability on the full dataset
TMB::openmp(n = 1)
RhpcBLASctl::blas_set_num_threads(1)
RhpcBLASctl::omp_set_num_threads(1)

# Fit the Final Model (Architecture is locked to our winning spatial-only structure)
final_fit <- sdmTMB(
  formula = winning_formula,
  data = baked_full_data,
  mesh = spde_full,
  family = gaussian(), 
  spatial = "on",           
  spatiotemporal = "off",   
  priors = operational_priors,
  control = sdmTMBcontrol(newton_loops = 1)
)

sanity(final_fit)
saveRDS(final_fit, file.path(out_dir, "final_do_model.rds"))
message(" [+] Final model optimized and saved!")

# ==============================================================================
# 06_model_diagnostics.R
# Purpose: Evaluates out-of-sample predictive skill and generates diagnostic 
#          plots for the final production SDM (m3_deltat) using CV data.
#          Includes a targeted evaluation of extreme hypoxic performance.
# ==============================================================================

library(dplyr)
library(ggplot2)
library(patchwork) 
library(sf)
source(here::here("R", "00_config_o2.R"))

message("--- Starting Model Diagnostics (Out-of-Sample) ---")

plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE)

# --- 1. LOAD THE TARGET MODELS ------------------------------------------------
# We explicitly target the GLORYS Base model and the Delta-T final model
m2_path <- file.path(out_dir, "model_cache", "m2_glorys.rds")
m3_path <- file.path(out_dir, "model_cache", "m3_deltat.rds")

if (!file.exists(m2_path) || !file.exists(m3_path)) {
  stop(" [X] CRITICAL: Missing CV objects in model_cache. Please re-run 03_.")
}

m2_cv <- readRDS(m2_path)
m3_cv <- readRDS(m3_path)

message(" [+] Successfully loaded cross-validation data for final models.")

# Unscale the data back to biological reality (umol/kg)
diag_data_m3 <- m3_cv$data %>%
  mutate(
    obs_o2 = o2_scaled * 100,
    pred_o2 = cv_predicted * 100,
    residual = obs_o2 - pred_o2
  )

diag_data_m2 <- m2_cv$data %>%
  mutate(
    obs_o2 = o2_scaled * 100,
    pred_o2 = cv_predicted * 100,
    residual = obs_o2 - pred_o2
  )

# --- 2. GLOBAL PREDICTIVE METRICS (Final Model: m3_deltat) --------------------
metrics_m3 <- diag_data_m3 %>%
  summarize(
    RMSE = sqrt(mean(residual^2, na.rm = TRUE)),
    MAE = mean(abs(residual), na.rm = TRUE),
    Pearson_r = cor(obs_o2, pred_o2, use = "complete.obs"),
    R_squared = cor(obs_o2, pred_o2, use = "complete.obs")^2
  )

message("\n--- GLOBAL PERFORMANCE (m3_deltat) ---")
message(sprintf("     RMSE: %.2f umol/kg", metrics_m3$RMSE))
message(sprintf("     MAE:  %.2f umol/kg", metrics_m3$MAE))
message(sprintf("     R^2:  %.3f", metrics_m3$R_squared))

# --- 3. THE HYPOXIA SPOTLIGHT (Lowest 5% of Observations) ---------------------
# Calculate the threshold for the bottom 5% of oxygen in your dataset
hypoxia_threshold <- quantile(diag_data_m3$obs_o2, probs = 0.05, na.rm = TRUE)

m2_extreme_rmse <- diag_data_m2 %>%
  filter(obs_o2 <= hypoxia_threshold) %>%
  summarize(RMSE = sqrt(mean(residual^2, na.rm = TRUE))) %>%
  pull(RMSE)

m3_extreme_rmse <- diag_data_m3 %>%
  filter(obs_o2 <= hypoxia_threshold) %>%
  summarize(RMSE = sqrt(mean(residual^2, na.rm = TRUE))) %>%
  pull(RMSE)

message("\n--- THE HYPOXIA SPOTLIGHT ---")
message(sprintf("Evaluating strictly on observations below %.1f umol/kg (Bottom 5%%):", hypoxia_threshold))
message(sprintf("     Base GLORYS (m2) RMSE: %.2f umol/kg", m2_extreme_rmse))
message(sprintf("     Delta-T (m3) RMSE:     %.2f umol/kg", m3_extreme_rmse))
message(sprintf("     Improvement:          %.2f umol/kg", m2_extreme_rmse - m3_extreme_rmse))
message("--------------------------------------\n")

# --- 4. DIAGNOSTIC PLOTS (Final Model: m3_deltat) -----------------------------
message("Generating diagnostic panel...")

# A. Observed vs Predicted (Added hypoxia threshold lines)
p_obs_pred <- ggplot(diag_data_m3, aes(x = pred_o2, y = obs_o2)) +
  geom_hex(bins = 60) +
  scale_fill_viridis_c(trans = "log10", name = "Density") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = hypoxia_threshold, color = "white", linetype = "dotted") +
  geom_vline(xintercept = hypoxia_threshold, color = "white", linetype = "dotted") +
  labs(title = "A. Observed vs. Predicted", 
       subtitle = paste("Dotted line = 5th percentile (", round(hypoxia_threshold, 1), "umol/kg)"),
       x = "Predicted O2", y = "Observed O2") +
  theme_bw()

# B. Residuals vs. Fitted 
p_res_fit <- ggplot(diag_data_m3, aes(x = pred_o2, y = residual)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(alpha = 0.1, color = "midnightblue") +
  geom_smooth(method = "gam", color = "darkorange", se = FALSE) +
  labs(title = "B. Residuals vs. Fitted", x = "Predicted O2", y = "Residual (Obs - Pred)") +
  theme_bw()

# C. Q-Q Plot 
p_qq <- ggplot(diag_data_m3, aes(sample = residual)) +
  stat_qq(color = "midnightblue", alpha = 0.3) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "C. Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_bw()

# D. Spatial Residuals (Are we missing specific regions?)
spatial_res <- diag_data_m3 %>%
  group_by(X_km, Y_km) %>%
  summarize(mean_res = mean(residual, na.rm = TRUE), .groups = "drop")

p_spatial <- ggplot(spatial_res, aes(x = X_km, y = Y_km, color = mean_res)) +
  geom_point(size = 0.5, alpha = 0.8) +
  scale_color_gradient2(low = "#d73027", mid = "grey90", high = "#4575b4", midpoint = 0,
                        name = "Mean\nResidual") +
  labs(title = "D. Spatial Residuals", x = "Easting (km)", y = "Northing (km)") +
  theme_bw() +
  coord_equal()

# Combine panels using patchwork
final_diagnostic_plot <- (p_obs_pred | p_res_fit) / (p_qq | p_spatial)

ggsave(file.path(plot_dir, "model_diagnostics_panel.png"), final_diagnostic_plot, 
       width = 12, height = 10, dpi = 300)

message(" [+] Diagnostic panel saved to /plots/model_diagnostics_panel.png")
message(" [+] Script 06_ Complete!")
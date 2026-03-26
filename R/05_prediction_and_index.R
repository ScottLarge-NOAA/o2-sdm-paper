# ==============================================================================
# 05_prediction_and_index.R
# Purpose: Projects the final SDM onto a spatiotemporal grid using input-derived 
#          covariates. Calculates a standardized regional O2 index and generates 
#          seasonal empirical maps for the NEUS shelf (MAB, GOM, GB).
# ==============================================================================

library(sdmTMB)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(recipes)
source(here::here("R", "00_config_o2.R"))

message("--- Starting Spatial Prediction & Index Generation ---")

# --- 1. LOAD MODEL & DATA -----------------------------------------------------
final_fit <- readRDS(file.path(out_dir, "final_do_model.rds"))
dat <- readRDS(file.path(der_dir, "all_o2_dat_filtered.rds"))
rec_prepped <- readRDS(file.path(der_dir, "o2_recipe.rds"))
epu_grid_base <- readRDS(file.path(der_dir, "epu_grid.rds")) 

# Clean input data to extract covariate trends
dat_cleaned <- dat %>%
  tidyr::drop_na(depth, o2, temp, sigma0, doy, X_km, Y_km, year) %>% 
  mutate(
    month = as.numeric(format(as.Date(paste(year, doy, sep="-"), "%Y-%j"), "%m")),
    season = case_when(
      month %in% 3:5 ~ "Spring",
      month %in% 9:11 ~ "Fall",
      TRUE ~ "Other"
    ),
    EPU = region
  ) %>%
  filter(season %in% c("Spring", "Fall"), EPU %in% target_regions)

target_years <- sort(unique(dat_cleaned$year))

# --- 2. BUILD DYNAMIC SPATIOTEMPORAL GRID -------------------------------------
message("Extracting dynamic covariates to build seasonal grids...")

# A. Calculate long-term baseline medians (Fallback for unsampled year/regions)
baseline_stats <- dat_cleaned %>%
  group_by(EPU, season) %>%
  summarize(
    base_temp = median(temp, na.rm = TRUE),
    base_sigma0 = median(sigma0, na.rm = TRUE),
    base_doy = median(doy, na.rm = TRUE),
    .groups = "drop"
  )

# B. Calculate specific year-region medians
yearly_stats <- dat_cleaned %>%
  group_by(EPU, year, season) %>%
  summarize(
    yr_temp = median(temp, na.rm = TRUE),
    yr_sigma0 = median(sigma0, na.rm = TRUE),
    yr_doy = median(doy, na.rm = TRUE),
    .groups = "drop"
  )

# C. Expand base grid across all years and seasons
expanded_grid <- expand_grid(
  epu_grid_base,
  year = target_years,
  season = c("Spring", "Fall")
)

# D. Join covariates and fill gaps using coalesce()
full_pred_grid <- expanded_grid %>%
  left_join(baseline_stats, by = c("EPU", "season")) %>%
  left_join(yearly_stats, by = c("EPU", "year", "season")) %>%
  mutate(
    temp = coalesce(yr_temp, base_temp),
    sigma0 = coalesce(yr_sigma0, base_sigma0),
    doy = coalesce(yr_doy, base_doy),
    depth_ln = log(depth),
    fold_id = 1
  ) %>%
  select(X_km, Y_km, depth, depth_ln, EPU, year, season, temp, sigma0, doy, fold_id)

# Bake the grid to apply recipe normalization
baked_grid <- recipes::bake(rec_prepped, new_data = full_pred_grid) %>%
  mutate(
    season = full_pred_grid$season,
    EPU = full_pred_grid$EPU,
    depth = full_pred_grid$depth # (Bringing depth back too, just in case!)
  )

# --- 3. PREDICT & CALCULATE REGIONAL INDEX ------------------------------------
message("Predicting over grid and calculating standardized regional indices...")

seasons <- c("Spring", "Fall")
all_indices <- list()
all_maps <- list()

# Cell area in km^2 based on your grid_res_m config (e.g., 10000m = 100 sq km)
cell_area_km2 <- (grid_res_m / 1000)^2 

for (s in seasons) {
  message(sprintf(" -> Processing %s...", s))
  season_grid <- baked_grid %>% filter(season == s)
  
  # Predict (return_tmb_object = TRUE is required for get_index)
  preds <- predict(final_fit, newdata = season_grid, return_tmb_object = TRUE)
  
  # Unscale Gaussian Output
  pred_data <- preds$data %>% mutate(o2_umol_kg = est * 100)
  all_maps[[s]] <- pred_data
  
  # Calculate Regional Index using sdmTMB::get_index()
  for (region_name in target_regions) {
    region_subset <- season_grid %>% filter(EPU == region_name)
    
    if (nrow(region_subset) > 0) {
      idx <- get_index(
        preds, 
        area = rep(cell_area_km2, nrow(region_subset)), 
        bias_correct = FALSE 
      )
      
      # Convert index sum to an Area-Weighted Mean
      total_area <- nrow(region_subset) * cell_area_km2
      
      idx_clean <- idx %>%
        mutate(
          season = s,
          EPU = region_name,
          mean_o2 = (est / total_area) * 100,
          lwr_o2 = (lwr / total_area) * 100,
          upr_o2 = (upr / total_area) * 100
        )
      all_indices[[paste(s, region_name)]] <- idx_clean
    }
  }
}

final_index_df <- bind_rows(all_indices)
final_maps_df <- bind_rows(all_maps)

saveRDS(final_index_df, file.path(out_dir, "regional_o2_index.rds"))
saveRDS(final_maps_df, file.path(out_dir, "spatiotemporal_o2_maps.rds"))

# --- 4. PLOTTING --------------------------------------------------------------
message("Generating plots...")
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE)

# A. Time Series Index Plot
p_index <- ggplot(final_index_df, aes(x = year, y = mean_o2, color = season, fill = season)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr_o2, ymax = upr_o2), alpha = 0.2, color = NA) +
  facet_wrap(~EPU, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Spring" = "#2c7bb6", "Fall" = "#d7191c")) +
  scale_fill_manual(values = c("Spring" = "#2c7bb6", "Fall" = "#d7191c")) +
  labs(
    title = "Regional Mean Bottom Oxygen (Area-Weighted)",
    x = "Year", y = expression(Bottom~O[2]~(mu*mol~kg^{-1}))
  ) +
  theme_bw(base_size = 14) + theme(legend.position = "top", legend.title = element_blank())

ggsave(file.path(plot_dir, "regional_index_timeseries.png"), p_index, width = 8, height = 10, dpi = 300)

# B. Spatiotemporal Maps (Last 5 Years to avoid giant plots)
recent_years <- tail(target_years, 5)
map_data_recent <- final_maps_df %>% filter(year %in% recent_years)

p_maps <- ggplot(map_data_recent, aes(x = X_km, y = Y_km, fill = o2_umol_kg)) +
  geom_raster() +
  facet_grid(season ~ year) +
  scale_fill_viridis_c(option = "magma", name = expression(O[2])) +
  labs(
    title = "Empirical Bottom Oxygen Predictions",
    subtitle = paste("NEUS Shelf:", min(recent_years), "-", max(recent_years)),
    x = "Easting (km)", y = "Northing (km)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = "bottom")

ggsave(file.path(plot_dir, "recent_o2_maps.png"), p_maps, width = 14, height = 6, dpi = 300)

message(" [+] Script 05_ Complete! Check the 'plots' directory.")
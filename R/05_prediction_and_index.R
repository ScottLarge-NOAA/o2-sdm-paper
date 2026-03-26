# ==============================================================================
# 05_prediction_and_index.R
# Purpose: Projects the final GLORYS-native SDM onto a 10km spatiotemporal grid.
#          Extracts high-resolution daily physics from NetCDF files to generate
#          dynamic empirical maps and a highly accurate Regional O2 Index.
# ==============================================================================

library(sdmTMB)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(terra)
library(recipes)
source(here::here("R", "00_config_o2.R"))

message("--- Starting Spatial Prediction & Index Generation ---")

# --- 1. LOAD MODEL & DATA -----------------------------------------------------
final_fit <- readRDS(file.path(out_dir, "final_do_model.rds"))
dat <- readRDS(file.path(der_dir, "all_o2_dat_filtered.rds"))
rec_prepped <- readRDS(file.path(der_dir, "o2_recipe.rds"))
epu_grid_base <- readRDS(file.path(der_dir, "epu_grid.rds")) 

target_years <- sort(unique(dat$year))

# Define target days for seasonal map snapshots (Peak Spring & Peak Fall)
target_dates <- data.frame(
  date = c(as.Date(paste0(target_years, "-05-15")), as.Date(paste0(target_years, "-09-15"))),
  season = rep(c("Spring", "Fall"), each = length(target_years)),
  year = rep(target_years, 2),
  doy = rep(c(135, 258), each = length(target_years))
)

# --- 2. EXTRACT DYNAMIC GLORYS GRID -------------------------------------------
message("Extracting high-resolution GLORYS physics for the prediction grid...")

glorys_dir <- file.path(raw_dir, "glorys")
nc_files <- list.files(glorys_dir, pattern = "\\.nc$", full.names = TRUE)

if(length(nc_files) == 0) stop(" [X] CRITICAL: No GLORYS NetCDF files found!")

# Convert the 10km spatial grid to a terra SpatVector for extraction
grid_sf <- epu_grid_base %>%
  mutate(X_m = X_km * 1000, Y_m = Y_km * 1000) %>%
  st_as_sf(coords = c("X_m", "Y_m"), crs = crs_projected, remove = FALSE) %>%
  st_transform(4326) # Project to geographic (Lon/Lat) to query GLORYS

grid_vect <- vect(grid_sf)

grid_extractions <- list()

for (nc_file in nc_files) {
  
  bot_rast <- rast(nc_file, subds = "bottomT")
  sfc_rast <- rast(nc_file, subds = "thetao")
  mld_rast <- rast(nc_file, subds = "mlotst")
  
  rast_dates <- as.Date(terra::time(bot_rast))
  
  # Find which of our target snapshot dates live inside this specific NetCDF file
  matching_dates <- target_dates %>% filter(date %in% rast_dates)
  
  if (nrow(matching_dates) == 0) next
  
  message(sprintf(" -> Extracting %d seasonal snapshots from %s...", nrow(matching_dates), basename(nc_file)))
  
  for (i in 1:nrow(matching_dates)) {
    curr_date <- matching_dates$date[i]
    layer_idx <- match(curr_date, rast_dates)
    
    # Extract the physics for all 10km pixels for this specific day
    ext_bot <- terra::extract(bot_rast[[layer_idx]], grid_vect, ID = FALSE)[,1]
    ext_sfc <- terra::extract(sfc_rast[[layer_idx]], grid_vect, ID = FALSE)[,1]
    ext_mld <- terra::extract(mld_rast[[layer_idx]], grid_vect, ID = FALSE)[,1]
    
    # Build the dataframe for this season/year
    snap_df <- epu_grid_base %>%
      mutate(
        year = matching_dates$year[i],
        season = matching_dates$season[i],
        doy = matching_dates$doy[i],
        temp_glorys = ext_bot,
        sfc_temp_glorys = ext_sfc,
        mlotst_glorys = ext_mld,
        delta_t_glorys = sfc_temp_glorys - temp_glorys,
        # Recipe bypass: Provide dummy insitu temp so bake() doesn't crash. 
        # (Model 3 mathematically ignores this column anyway).
        temp_insitu = temp_glorys, 
        depth_ln = log(depth),
        fold_id = 1
      ) %>%
      # Drop pixels that fell off the GLORYS landmask
      tidyr::drop_na(temp_glorys, delta_t_glorys) 
    
    grid_extractions[[as.character(curr_date)]] <- snap_df
  }
}

full_pred_grid <- bind_rows(grid_extractions)

message("Baking prediction grid through the recipes normalization...")
baked_grid <- recipes::bake(rec_prepped, new_data = full_pred_grid) %>%
  mutate(
    season = full_pred_grid$season,
    EPU = full_pred_grid$EPU,
    depth = full_pred_grid$depth 
  )

# --- 3. PREDICT & CALCULATE REGIONAL INDEX ------------------------------------
message("Predicting spatial field and calculating standardized indices...")

seasons <- c("Spring", "Fall")
all_indices <- list()
all_maps <- list()

for (s in seasons) {
  message(sprintf(" -> Projecting %s maps...", s))
  season_grid <- baked_grid %>% filter(season == s)
  
  # Predict spatial field 
  preds <- predict(final_fit, newdata = season_grid)
  
  # Unscale Gaussian Output
  pred_data <- preds %>% mutate(o2_umol_kg = est * 100)
  all_maps[[s]] <- pred_data
  
  # Calculate Regional Mean (Because it's a uniform 10km grid, mean() IS area-weighted)
  idx_clean <- pred_data %>%
    group_by(year, EPU) %>%
    summarize(
      mean_o2 = mean(o2_umol_kg, na.rm = TRUE),
      sd_o2 = sd(o2_umol_kg, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    mutate(
      season = s,
      lwr_o2 = mean_o2 - (1.96 * sd_o2),
      upr_o2 = mean_o2 + (1.96 * sd_o2)
    )
  
  all_indices[[s]] <- idx_clean
}

final_index_df <- bind_rows(all_indices)
final_maps <- bind_rows(all_maps)

saveRDS(final_index_df, file.path(out_dir, "regional_o2_index.rds"))
saveRDS(final_maps, file.path(out_dir, "spatiotemporal_o2_maps.rds"))

# --- 4. PLOTTING --------------------------------------------------------------
message("Generating publication plots...")
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
    title = "Area-weighted mean bottom dissolved oxygen",
    caption = "Derived from GLORYS Reanalysis & Empirical Spatiotemporal Modeling",
    x = "Year", y = expression(Bottom~O[2]~(mu*mol~kg^{-1}))
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", 
        legend.title = element_blank())

ggsave(file.path(plot_dir, "regional_index_timeseries.png"), p_index, width = 8, height = 10, dpi = 300)

# B. Spatiotemporal Maps (Last 5 Years)
recent_years <- tail(target_years, 5)
map_data_recent <- final_maps %>% filter(year %in% recent_years)

p_maps <- ggplot(map_data_recent, aes(x = X_km, y = Y_km, fill = o2_umol_kg)) +
  geom_tile() +
  facet_grid(season ~ year) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = paste("NEUS Shelf:", min(recent_years), "-", max(recent_years)),
    x = "Easting (km)", y = "Northing (km)",
    fill = expression(Bottom~O[2]~(mu*mol~kg^{-1}))
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        legend.position = "bottom")

ggsave(file.path(plot_dir, "recent_o2_maps.png"), p_maps, width = 14, height = 6, dpi = 300)

message(" [+] Script 05_ Complete! Check the 'plots' directory to see the stratification maps!")
# ==============================================================================
# 02_depth_quality_control.R
# Purpose: Compare reported sample depths against NOAA bathymetry models. 
#          Filters out casts where the depth error exceeds 2*RMSE.
#          Uses robust terra raster caching for bathymetry.
# ==============================================================================

library(terra)
library(marmap)
source(here::here("R", "00_config_o2.R"))

message("--- Starting Depth Quality Control ---")

dat <- readRDS(file.path(der_dir, "all_o2_dat.rds"))

# --- 1. GET REGIONAL POLYGONS FROM CUSTOM SHAPEFILE ---------------------------
message("Loading custom EPU spatial boundaries...")
epu_path <- file.path(raw_dir, "EPU_NOESTUARIES.shp")

regions_sf <- read_sf(epu_path) %>% 
  st_transform(crs_projected) %>%
  st_make_valid() %>%
  rename_with(~"EPU", starts_with("EPU")) %>% 
  filter(EPU %in% target_regions) 

# Convert coordinates to km to match our pipeline geometry
st_geometry(regions_sf) <- st_geometry(regions_sf) / 1000
st_crs(regions_sf) <- sf::st_crs(paste0(crs_projected$wkt, " +units=km"))

regions_hull <- regions_sf %>%
  group_by(EPU) %>%
  summarise(geometry = st_combine(geometry), .groups = "drop") %>%
  st_convex_hull()

# --- 2. ROBUST BATHYMETRY DOWNLOAD & CACHING (Terra approach) -----------------
message("Fetching/Loading NOAA bathymetry raster...")
bathy_path <- file.path(der_dir, "neus_bathy.tif")

epu_geo <- st_transform(read_sf(epu_path) %>% filter(EPU %in% target_regions), crs_geographic)

if (!file.exists(bathy_path)) {
  message(" [!] Bathymetry .tif missing. Downloading via bounding box...")
  options(timeout = 600)
  bbox <- st_bbox(epu_geo)
  
  bathy_marmap <- marmap::getNOAA.bathy(lon1 = bbox["xmin"] - 1, lon2 = bbox["xmax"] + 1,
                                        lat1 = bbox["ymin"] - 1, lat2 = bbox["ymax"] + 1,
                                        resolution = 4, keep = FALSE)
  
  bathy_rast <- terra::rast(marmap::as.raster(bathy_marmap))
  terra::writeRaster(bathy_rast, bathy_path, overwrite = TRUE)
} else {
  message(" [v] Bathymetry .tif found. Loading from cache...")
  bathy_rast <- terra::rast(bathy_path)
}

# Convert the raster into a dataframe. 
# STRUCTURAL FIX: Explicitly name the columns X_km and Y_km to match core data.
bathy_points_geo <- terra::as.points(bathy_rast) %>% st_as_sf()
bathy_points_proj <- st_transform(bathy_points_geo, crs_projected)
coords_km <- st_coordinates(bathy_points_proj) / 1000

bathy_df <- tibble(
  X_km = coords_km[,1], 
  Y_km = coords_km[,2], 
  noaadepth = -1 * as.numeric(st_drop_geometry(bathy_points_geo)[,1]) 
) %>% filter(noaadepth > 0)

# --- 3. FIT BATHYMETRY MODELS PER REGION --------------------------------------
message("Fitting depth expectation models...")
maxdepth <- max(dat$depth, na.rm = TRUE) * 1.1
depth_models <- list()

# Convert bathy_df to sf using the correctly named columns
bathy_sf <- st_as_sf(bathy_df, coords = c("X_km", "Y_km"), crs = st_crs(regions_sf), remove = FALSE)

for (i in seq_along(target_regions)) {
  region_name <- target_regions[i]
  poly <- regions_hull[i, ]
  
  # Clip bathy to region
  region_bathy <- sf::st_filter(bathy_sf, poly) %>% st_drop_geometry() %>% filter(noaadepth <= maxdepth)
  
  # STRUCTURAL FIX: Train model natively on X_km and Y_km
  mesh <- sdmTMB::make_mesh(region_bathy, xy_cols = c("X_km", "Y_km"), cutoff = 45)
  depth_models[[region_name]] <- sdmTMB(log(noaadepth) ~ 1, data = region_bathy, 
                                        spatial = "on", mesh = mesh, family = gaussian())
}

# --- 4. PREDICT & FILTER BAD CASTS --------------------------------------------
message("Evaluating reported depths against expected bathymetry...")

dat_sf <- st_as_sf(dat, coords = c("X_km", "Y_km"), crs = st_crs(regions_sf), remove = FALSE)
dat_predict <- tibble()

for (i in seq_along(target_regions)) {
  region_name <- target_regions[i]
  region_dat <- sf::st_filter(dat_sf, regions_hull[i, ])
  
  if(nrow(region_dat) > 0) {
    region_df <- sf::st_drop_geometry(region_dat)
    
    preds <- predict(depth_models[[region_name]], newdata = region_df)
    preds$predicted_region <- region_name
    
    dat_predict <- bind_rows(dat_predict, preds)
  }
}

# Separate EcoMon (CTD) from Fishbot (Synoptic/Trawl)
ctd_dat <- dat_predict %>% filter(survey == "ecomon")
trawl_dat <- dat_predict %>% filter(survey != "ecomon")

# Calculate RMSE natively
rmse_df <- dat_predict %>%
  group_by(predicted_region) %>%
  summarise(rmse = sqrt(mean((log(depth) - est)^2, na.rm = TRUE)), .groups = "drop")

ctd_dat <- ctd_dat %>%
  left_join(rmse_df, by = "predicted_region") %>%
  mutate(depth_error = log(depth) - est) %>%
  filter(abs(depth_error) <= 2 * rmse) 

# Recombine and save
dat_filtered <- bind_rows(trawl_dat, ctd_dat) %>%
  select(survey, year, doy, X_km, Y_km, latitude, longitude, temp, o2, sigma0, salinity_psu, depth, region)

saveRDS(dat_filtered, file.path(der_dir, "all_o2_dat_filtered.rds"))
message(sprintf(" [+] QC Complete. Kept %d valid observations.", nrow(dat_filtered)))

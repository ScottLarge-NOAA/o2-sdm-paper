# ==============================================================================
# 01_data_initialization_o2.R
# Purpose: Extract ERDDAP DO data (EcoMon strictly), calculate metrics 
#          via gsw, and append daily GLORYS physical reanalysis data 
#          to resolve water column stratification.
# ==============================================================================

library(httr)    
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(terra) # Added for lightning-fast GLORYS raster extraction
source(here::here("R", "00_config_o2.R"))
sf::sf_use_s2(FALSE)

message("--- Starting DO Data Initialization (EcoMon Strictly) ---")

# --- 1. FETCH ERDDAP DATA -----------------------------------------------------
ecomon_path <- file.path(raw_dir, "ocdbs_v_erddap1.csv")

if (!file.exists(ecomon_path)) {
  message("Downloading EcoMon Data...")
  httr::GET("https://comet.nefsc.noaa.gov/erddap/tabledap/ocdbs_v_erddap1.csv", 
            httr::write_disk(ecomon_path, overwrite = TRUE), 
            httr::config(ssl_verifypeer = FALSE))
}

# --- 2. LOAD EPU SHAPEFILE ----------------------------------------------------
message("Loading spatial boundaries...")
epu_path <- file.path(raw_dir, "EPU_NOESTUARIES.shp")
epu_sf <- read_sf(epu_path) %>% 
  st_transform(crs_projected) %>%
  st_make_valid() %>%
  rename_with(~"EPU", starts_with("EPU")) %>% 
  select(EPU) %>%
  filter(EPU %in% target_regions)

# --- 3. CLEAN & SPATIALLY JOIN DATA (ECOMON) ----------------------------------
message("Processing EcoMon data...")

dat_eco <- read.csv(ecomon_path, header = TRUE, na.strings = "NaN") %>% 
  slice(-1) %>% # Remove metadata row
  mutate(
    date = as.Date(UTC_DATETIME),
    year = year(date), month = month(date), doy = yday(date),
    latitude = as.numeric(latitude), longitude = as.numeric(longitude), depth = as.numeric(depth),
    o2_mgl = as.numeric(dissolved_oxygen), 
    salinity_psu = as.numeric(sea_water_salinity),
    temperature_C = as.numeric(sea_water_temperature),
    survey = "ecomon"
  ) %>% 
  # Strict QA/QC: Drop any cast missing core thermodynamic variables
  tidyr::drop_na(longitude, latitude, depth, o2_mgl, temperature_C, salinity_psu) %>%
  
  # Grab the bottom-most sample per cast
  group_by(date, latitude, longitude) %>% 
  slice_max(order_by = depth, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  
  # Spatial Join to EPUs
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_geographic, remove = FALSE) %>%
  st_transform(crs_projected) %>%
  mutate(X_km = st_coordinates(.)[,1] / 1000, Y_km = st_coordinates(.)[,2] / 1000) %>%
  st_join(epu_sf) %>% 
  filter(!is.na(EPU)) %>%
  st_drop_geometry()

# --- 4. CALCULATE GSW DENSITY & DO UMOL/KG ------------------------------------
message("Calculating final oceanographic metrics...")

dat_gsw <- dat_eco %>%
  mutate(
    SA = gsw::gsw_SA_from_SP(salinity_psu, depth, longitude, latitude), 
    PT = gsw::gsw_pt_from_t(SA, temperature_C, depth), 
    CT = gsw::gsw_CT_from_t(SA, temperature_C, depth), 
    sigma0 = gsw::gsw_sigma0(SA, CT), 
    o2 = o2_mgl * 44660 / (sigma0 + 1000) 
  ) %>%
  # Rename in situ temp to preserve it for model comparisons!
  rename(temp_insitu = temperature_C, region = EPU) %>%
  select(survey, region, date, year, month, doy, X_km, Y_km, latitude, longitude, 
         depth, temp_insitu, salinity_psu, sigma0, o2) %>% 
  
  # Final safety drop for calculation errors
  tidyr::drop_na(depth, o2, temp_insitu, sigma0, X_km, Y_km) %>% 
  filter(o2 > 0)

# --- 5. EXTRACT GLORYS REANALYSIS DATA ----------------------------------------
message("Extracting daily GLORYS physics (Temperature & Mixed Layer Depth)...")

glorys_dir <- file.path(raw_dir, "glorys")

# Automatically find ALL NetCDF files in the directory
nc_files <- list.files(glorys_dir, pattern = "\\.nc$", full.names = TRUE)

if (length(nc_files) == 0) {
  stop("No NetCDF files found in the glorys directory! Check file paths.")
}

extracted_list <- list()

for (nc_file in nc_files) {
  message(sprintf(" -> Processing file: %s", basename(nc_file)))
  
  # Load the raster datasets (terra just reads metadata here, so it's instant)
  bot_rast <- rast(nc_file, subds = "bottomT")
  sfc_rast <- rast(nc_file, subds = "thetao")
  mld_rast <- rast(nc_file, subds = "mlotst")
  
  # MAGIC TRICK: Extract the exact calendar dates embedded in the NetCDF
  rast_dates <- as.Date(terra::time(bot_rast))
  
  # Filter the CTD data to ONLY include casts that fall within this file's dates
  dat_chunk <- dat_gsw %>% filter(date %in% rast_dates)
  
  if (nrow(dat_chunk) == 0) {
    message("    - No CTD casts match the dates in this file. Skipping.")
    next
  }
  
  message(sprintf("    - Extracting data for %d spatial points...", nrow(dat_chunk)))
  
  # Convert matching CTD points to terra SpatVector (Must use geographic crs 4326!)
  pt_vect <- vect(st_as_sf(dat_chunk, coords = c("longitude", "latitude"), crs = 4326))
  
  # Extract ALL time layers in the file for these specific spatial points
  bot_ext <- terra::extract(bot_rast, pt_vect, ID = FALSE)
  sfc_ext <- terra::extract(sfc_rast, pt_vect, ID = FALSE)
  mld_ext <- terra::extract(mld_rast, pt_vect, ID = FALSE)
  
  # MATCHING LOGIC: Find which column (layer) matches the specific date of each cast
  # match() returns the index of the first match, elegantly solving the 5-year offset
  layer_indices <- match(dat_chunk$date, rast_dates)
  
  # Pull the exact value from the extracted matrix
  dat_chunk$temp_glorys <- sapply(seq_len(nrow(dat_chunk)), function(i) as.numeric(bot_ext[i, layer_indices[i]]))
  dat_chunk$sfc_temp_glorys <- sapply(seq_len(nrow(dat_chunk)), function(i) as.numeric(sfc_ext[i, layer_indices[i]]))
  dat_chunk$mlotst_glorys <- sapply(seq_len(nrow(dat_chunk)), function(i) as.numeric(mld_ext[i, layer_indices[i]]))
  
  extracted_list[[basename(nc_file)]] <- dat_chunk
}

# Bind back together
all_o2_dat <- bind_rows(extracted_list) %>%
  # Just in case files had overlapping dates and duplicated a point, keep it clean
  distinct(survey, date, latitude, longitude, depth, .keep_all = TRUE) %>%
  mutate(
    delta_t_glorys = sfc_temp_glorys - temp_glorys
  ) %>%
  # Drop any points that fell off the GLORYS landmask (NAs)
  tidyr::drop_na(temp_glorys, mlotst_glorys)

# Save the final masterpiece
saveRDS(all_o2_dat, file.path(der_dir, "all_o2_dat.rds"))

message(sprintf(" [+] DO Data Initialization Complete. %d total EcoMon observations with GLORYS physics ready for modeling.", nrow(all_o2_dat)))

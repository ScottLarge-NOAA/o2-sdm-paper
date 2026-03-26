# ==============================================================================
# 01_data_initialization_o2.R
# Purpose: Extract ERDDAP DO data (EcoMon strictly), calculate metrics 
#          via gsw, and build the spatiotemporal prediction grid.
# ==============================================================================

library(httr)    
library(dplyr)
library(lubridate)
library(sf)
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

all_o2_dat <- dat_eco %>%
  mutate(
    SA = gsw::gsw_SA_from_SP(salinity_psu, depth, longitude, latitude), 
    PT = gsw::gsw_pt_from_t(SA, temperature_C, depth), 
    CT = gsw::gsw_CT_from_t(SA, temperature_C, depth), 
    sigma0 = gsw::gsw_sigma0(SA, CT), 
    o2 = o2_mgl * 44660 / (sigma0 + 1000) 
  ) %>%
  rename(temp = temperature_C, region = EPU) %>%
  select(survey, region, year, month, doy, X_km, Y_km, latitude, longitude, 
         depth, temp, salinity_psu, sigma0, o2) %>% 
  
  # Final safety drop for calculation errors
  tidyr::drop_na(depth, o2, temp, sigma0, X_km, Y_km) %>% 
  filter(o2 > 0)

saveRDS(all_o2_dat, file.path(der_dir, "all_o2_dat.rds"))

message(sprintf(" [+] DO Data Initialization Complete. %d total EcoMon observations ready for modeling.", nrow(all_o2_dat)))
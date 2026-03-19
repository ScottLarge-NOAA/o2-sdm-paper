# ==============================================================================
# 01_data_initialization_o2.R
# Purpose: Extract ERDDAP DO data, handle multi-platform integration, impute 
#          missing salinity using regional climatologies, calculate metrics 
#          via gsw, and build the spatiotemporal prediction grid.
# ==============================================================================

library(httr)    # For bypassing NOAA SSL certificates
library(stringr) # For parsing data_provider strings
source(here::here("R", "00_config_o2.R"))
sf::sf_use_s2(FALSE)

message("--- Starting DO Data Initialization ---")

# --- 1. FETCH ERDDAP DATA (SSL BYPASS) ----------------------------------------
ecomon_path <- file.path(raw_dir, "ocdbs_v_erddap1.csv")
fishbot_path <- file.path(raw_dir, "fishbot_realtime.csv")

if (!file.exists(ecomon_path)) {
  message("Downloading EcoMon Data...")
  httr::GET("https://comet.nefsc.noaa.gov/erddap/tabledap/ocdbs_v_erddap1.csv", 
            httr::write_disk(ecomon_path, overwrite = TRUE), 
            httr::config(ssl_verifypeer = FALSE))
}

if (!file.exists(fishbot_path)) {
  message("Downloading Fishbot Data...")
  httr::GET("https://erddap.ondeckdata.com/erddap/tabledap/fishbot_realtime.csv", 
            httr::write_disk(fishbot_path, overwrite = TRUE), 
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
  # Notice we DO NOT drop NA salinity here! Only drop missing core parameters.
  tidyr::drop_na(longitude, latitude, depth, o2_mgl, temperature_C) %>%
  group_by(date, latitude, longitude) %>% 
  slice_max(order_by = depth, n = 1, with_ties = FALSE) %>% ungroup() %>%
  # Spatial Join
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_geographic, remove = FALSE) %>%
  st_transform(crs_projected) %>%
  mutate(X_km = st_coordinates(.)[,1] / 1000, Y_km = st_coordinates(.)[,2] / 1000) %>%
  st_join(epu_sf) %>% 
  filter(!is.na(EPU)) %>%
  st_drop_geometry()

# --- 4. CLEAN & SPATIALLY JOIN DATA (FISHBOT) ---------------------------------
message("Processing Fishbot data...")

dat_fish <- read.csv(fishbot_path, header = TRUE, na.strings = "NaN") %>% 
  slice(-1) %>% 
  # Drop pre-summarized data (anything with a comma in data_provider)
  filter(!str_detect(data_provider, ",")) %>% 
  # Prevent Double Counting: Remove single-provider ECOMON cells
  filter(data_provider != "ECOMON") %>% 
  mutate(
    date = as.Date(time),
    year = year(date), month = month(date), doy = yday(date),
    latitude = as.numeric(latitude), longitude = as.numeric(longitude), depth = as.numeric(depth),
    o2_mgl = as.numeric(dissolved_oxygen), 
    salinity_psu = as.numeric(salinity),
    temperature_C = as.numeric(temperature),
    
    # Safely assign boolean logic based on 0/1 encoding
    is_dependent = case_when(
      fishery_dependent %in% c("1", "1.0", 1) ~ TRUE,
      fishery_dependent %in% c("0", "0.0", 0) ~ FALSE,
      TRUE ~ NA
    ),
    
    # Assign platform bucket
    survey = ifelse(is_dependent, "fishery_dependent", "fishery_independent")
  ) %>% 
  tidyr::drop_na(longitude, latitude, depth, o2_mgl, temperature_C, survey) %>%
  group_by(date, latitude, longitude) %>% 
  slice_max(order_by = depth, n = 1, with_ties = FALSE) %>% ungroup() %>%
  # Spatial Join
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_geographic, remove = FALSE) %>%
  st_transform(crs_projected) %>%
  mutate(X_km = st_coordinates(.)[,1] / 1000, Y_km = st_coordinates(.)[,2] / 1000) %>%
  st_join(epu_sf) %>% 
  filter(!is.na(EPU)) %>%
  st_drop_geometry()

# --- 5. IMPUTE MISSING SALINITY -----------------------------------------------
message("Building Salinity Climatology and imputing missing values...")

# Create a lookup table of average salinity by EPU and Month from EcoMon
salinity_climatology <- dat_eco %>%
  filter(!is.na(salinity_psu)) %>%
  group_by(EPU, month) %>%
  summarize(mean_salinity = mean(salinity_psu, na.rm = TRUE), .groups = "drop")

# Merge the climatology into Fishbot and fill gaps
dat_fish_imputed <- dat_fish %>%
  left_join(salinity_climatology, by = c("EPU", "month")) %>%
  mutate(
    salinity_imputed_flag = is.na(salinity_psu),
    # If salinity is missing, use regional monthly mean. If that's missing, use 33.5 PSU.
    salinity_psu = case_when(
      !is.na(salinity_psu) ~ salinity_psu,
      !is.na(mean_salinity) ~ mean_salinity,
      TRUE ~ 33.5 
    )
  ) %>%
  select(-mean_salinity)

# Add the flag to EcoMon for consistency
dat_eco_imputed <- dat_eco %>% mutate(salinity_imputed_flag = FALSE)

# --- 6. CALCULATE GSW DENSITY & DO UMOL/KG ------------------------------------
message("Calculating final oceanographic metrics...")

all_o2_dat <- bind_rows(dat_eco_imputed, dat_fish_imputed) %>%
  mutate(
    SA = gsw::gsw_SA_from_SP(salinity_psu, depth, longitude, latitude), 
    PT = gsw::gsw_pt_from_t(SA, temperature_C, depth), 
    CT = gsw::gsw_CT_from_t(SA, temperature_C, depth), 
    sigma0 = gsw::gsw_sigma0(SA, CT), 
    o2 = o2_mgl * 44660 / (sigma0 + 1000) 
  ) %>%
  rename(temp = temperature_C, region = EPU) %>%
  select(survey, region, year, month, doy, X_km, Y_km, latitude, longitude, 
         depth, temp, salinity_psu, salinity_imputed_flag, sigma0, o2) %>% 
  tidyr::drop_na(depth, o2, temp, sigma0, X_km, Y_km) |> 
  filter(o2 > 0)

saveRDS(all_o2_dat, file.path(der_dir, "all_o2_dat.rds"))

message(sprintf(" [+] DO Data Initialization Complete. %d total observations ready for modeling.", nrow(all_o2_dat)))
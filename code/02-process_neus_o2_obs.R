library(rerddap)
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
library(furrr)
library(future)
library(sdmTMB)
library(purrr)
library(recipes)
library(rsample)
# fishbot_info <- rerddap::info(datasetid = "fishbot_realtime", url = "https://erddap.ondeckdata.com/erddap")
# fishbot_path <- here::here("data/oxygen options/NEUS/fishbot_realtime.csv")

# ecomon_info <- rerddap::info(datasetid = "ocdbs_v_erddap1", url = "https://comet.nefsc.noaa.gov/erddap/")
# ecomon_path <- here::here("data/oxygen options/NEUS/ocdbs_v_erddap1.csv")
# 

projected_crs <-  sf::st_crs(32619)
geographic_crs <- sf::st_crs(4326)
# dat1 <- rerddap::tabledap(fishbot_info, url = "https://erddap.ondeckdata.com/erddap",
#                           store = disk(here::here("data/oxygen options/NEUS/")))

# file.rename(from = attr(dat1, "path"),
#             to = fishbot_path)
# dat1 <- NULL
# dat1 <- read.csv(fishbot_path,
#                 header = TRUE,
#                 na.strings = "NaN")


dat <- rerddap::tabledap(ecomon_info, url = "https://commet.nefsc.noaa.gov/erddap/",
                         store = disk(here::here("data/oxygen options/NEUS/")))

file.rename(from = attr(dat, "path"),
            to = ecomon_path)
dat <- NULL
dat <- read.csv(ecomon_path,
                header = TRUE,
                na.strings = "NaN")

ecomon_units <- dat[1,]

## Clean up data and grab the bottom depth (slice_max)
ecomon_bottom <- dat %>% 
  slice(-1) %>% ## remove the metadata row
  mutate(date = as.Date(UTC_DATETIME),
         year = year(date), 
         month = month(date),
         doy = yday(date),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         depth = as.numeric(depth),
         o2_mgl = as.numeric(dissolved_oxygen),
         pressure_dbar = as.numeric(pressure_dbars),
         salinity_psu = as.numeric(sea_water_salinity),
         temperature_C = as.numeric(sea_water_temperature)) %>% 
  group_by(date, latitude, longitude) %>% ## Remove the water column samples and just grab the bottom sample
  slice_max(order_by = depth, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(year, month, doy, longitude, latitude, depth, pressure_dbar, salinity_psu, o2_mgl, temperature_C) %>% 
  tidyr::drop_na(any_of(c("longitude", "latitude"))) ## remove samples without Lat and Long

## Calculate the micro-moles o2 per kg   
ecomon_sf <- ecomon_bottom %>% 
  mutate(SA = gsw::gsw_SA_from_SP(salinity_psu, depth, longitude, latitude), # Convert Practical Salinity to Absolute SalinitPT = gsw::gsw_pt_from_t(SA, temperature_C, depth), # Potential temperature from in-situ temperature
         CT = gsw::gsw_CT_from_t(SA, temperature_C, depth), # Convert temperature to conservative temperature
         sigma0_kgm3 = gsw::gsw_sigma0(SA, CT), # Potential density anomaly 
         o2_umolkg = o2_mgl * 44660/(sigma0_kgm3 + 1000)) %>% # Convert to micromoles per kg
  st_as_sf(coords=c('longitude','latitude'), crs = geographic_crs, remove = F) %>%
  st_transform(crs = projected_crs)

## Update CRS to use km instead of m
ecomon_dat <- ecomon_sf
sf::st_geometry(ecomon_dat) <- sf::st_geometry(ecomon_dat) / 1000
wkt_meters <- sf::st_crs(ecomon_sf)$wkt
wkt_km <- gsub('LENGTHUNIT\\["metre",1\\]', 'LENGTHUNIT\\["kilometre",1000\\]', wkt_meters)
sf::st_crs(ecomon_dat) <- wkt_km

ecomon_dat <- ecomon_dat %>%
   mutate(X = st_coordinates(.)[,1],Y = st_coordinates(.)[,2])


## Not sure this adds anything
# fishbot_dat <- dat1 %>%
#   slice(-1) %>% 
#   # filter(data_provider %in% c("eMOLT")) %>% #c("ECOMON", "eMOLT") 
#   mutate(date = as.Date(time),
#          year = year(date), 
#          month = month(date),
#          doy = yday(date),
#          latitude = as.numeric(latitude),
#          longitude = as.numeric(longitude),
#          depth = as.numeric(depth),
#          o2_mgl = as.numeric(dissolved_oxygen),
#          # pressure_dbar = as.numeric(pressure_dbars),
#          salinity_psu = as.numeric(salinity),
#          temperature_C = as.numeric(temperature)) %>% 
#   select(year, month, doy, longitude, latitude, depth, #pressure_dbar, 
#          salinity_psu, o2_mgl, temperature_C, data_provider) %>% 
#   tidyr::drop_na(c("longitude", "latitude", "salinity_psu", "o2_mgl", "temperature_C")) %>% 
#   st_as_sf(coords=c('longitude','latitude'), crs=4269, remove = F) %>%
#   st_transform(crs = "+proj=utm +zone=19 +dat1um=WGS84 +units=km") %>%
#   mutate(X = st_coordinates(.)[,1],Y = st_coordinates(.)[,2])
  

# tt <- dat1 %>%
#   slice(-1) %>% 
#   filter(data_provider %in% c("eMOLT")) %>% #c("ECOMON", "eMOLT") 
#   mutate(date = as.Date(time),
#          year = year(date), 
#          month = month(date),
#          doy = yday(date),
#          latitude = as.numeric(latitude),
#          longitude = as.numeric(longitude),
#          depth = as.numeric(depth),
#          o2_mgl = as.numeric(dissolved_oxygen),
#          salinity_psu = as.numeric(salinity),
#          temperature_C = as.numeric(temperature)) %>% 
#   select(year, month, doy, longitude, latitude, depth, #pressure_dbar,
#          salinity_psu, o2_mgl, temperature_C, data_provider)


# #convert oxygen mg/L to umol_kg
# fishbot_SA = gsw::gsw_SA_from_SP(fishbot_dat$salinity_psu, 
#                          fishbot_dat$depth,
#                          fishbot_dat$longitude,
#                          fishbot_dat$latitude) #absolute salinity for pot T calc
# fishbot_pt = gsw::gsw_pt_from_t(fishbot_SA,
#                         fishbot_dat$temperature_C,
#                         fishbot_dat$depth) #potential temp at a particular depth
# fishbot_CT = gsw::gsw_CT_from_t(fishbot_SA,
#                         fishbot_dat$temperature_C,
#                         fishbot_dat$depth) #conservative temp
# fishbot_dat$sigma0_kgm3 = gsw::gsw_sigma0(fishbot_SA,
#                                           fishbot_CT)
# fishbot_dat$O2_umolkg = fishbot_dat$o2_mgl*44660/(fishbot_dat$sigma0_kgm3+1000)


# ggplot(fishbot_dat %>% filter(!is.na(O2_umolkg)), aes(x = O2_umolkg, y = -depth, color = year)) +
#   geom_point()
# 
# do_dat <- fishbot_dat %>% 
#   na.omit(o2_mgl)
# ggplot(do_dat, aes(x = year, group = as.factor(month), fill = as.factor(month))) +
#   geom_histogram()
# ggplot(do_dat, aes(x = longitude, y = latitude, color = as.factor(month), size = O2_umolkg)) +
#   geom_point()+
#   facet_wrap(~year)
# https://comet.nefsc.noaa.gov/erddap/tabledap/ocdbs_v_erddap1.csv?UTC_DATETIME%2Clatitude%2Clongitude%2Cdepth%2Cpressure_dbars%2Csea_water_temperature%2Csea_water_salinity%2Cdissolved_oxygen%2Cfluorescence%2Cpar_sensor%2Ccast_number%2Ccruise_id%2Cpurpose_code%2Cbottom_depth%2CGEAR_TYPE

# ecomon_dat <- read.csv(here::here("data/oxygen options/NEUS/ocdbs_v_erddap1.csv"),
#                        header = TRUE,
#                        na.strings = "NaN")
# 
# ecomon_units <- ecomon_dat[1,]
# 
# ecomon_dat <- ecomon_dat[-1,]
# 
# dat <- ecomon_dat %>% 
#   # head(1000) %>% 
#   mutate(date = as.Date(UTC_DATETIME),
#          year = year(date), 
#          month = month(date),
#          doy = yday(date),
#          latitude = as.numeric(latitude),
#          longitude = as.numeric(longitude),
#          depth = as.numeric(depth),
#          o2_mgl = as.numeric(dissolved_oxygen),
#          pressure_dbar = as.numeric(pressure_dbars),
#          salinity_psu = as.numeric(sea_water_salinity),
#          temperature_C = as.numeric(sea_water_temperature)) %>% 
#   select(year, month, doy, longitude, latitude, depth, pressure_dbar, salinity_psu, o2_mgl, temperature_C)
# 
# dat1 <- ecomon_dat %>%
#   st_as_sf(coords=c('longitude','latitude'), crs=4269, remove = F) %>%
#   st_transform(crs = "+proj=utm +zone=19 +dat1um=WGS84 +units=km") %>%
#   mutate(X = st_coordinates(.)[,1],Y = st_coordinates(.)[,2])

#convert oxygen mg/L to umol_kg
# 
# tt <- ecomon_dat %>% 
#   mutate(SA = gsw::gsw_SA_from_SP(salinity_psu, depth, longitude, latitude),
#          PT = gsw::gsw_pt_from_t(SA, temperature_C, depth),
#          CT = gsw::gsw_CT_from_t(SA, temperature_C, depth),
#          sigma0_kgm3 = gsw::gsw_sigma0(SA, CT),
#          o2_umolkg = o2_mgl * 44660/(sigma0_kgm3 + 1000))
# 
# 
# ecomon_SA = gsw::gsw_SA_from_SP(ecomon_dat$salinity_psu, 
#                          ecomon_dat$depth,
#                          ecomon_dat$longitude,
#                          ecomon_dat$latitude) #absolute salinity for pot T calc
# ecomon_pt = gsw::gsw_pt_from_t(ecomon_SA,
#                         ecomon_dat$temperature_C,
#                         ecomon_dat$depth) #potential temp at a particular depth
# ecomon_CT = gsw::gsw_CT_from_t(ecomon_SA,
#                         ecomon_dat$temperature_C,
#                         ecomon_dat$depth) #conservative temp
# ecomon_dat$sigma0_kgm3 = gsw::gsw_sigma0(ecomon_SA, 
#                                          ecomon_CT) # Potential density anomaly 
# ecomon_dat$O2_umolkg = ecomon_dat$o2_mgl * 44660/(ecomon_dat$sigma0_kgm3 + 1000)
# 

# #Convert coordinates
# #Remove with missing coordinates
# dat <- dat %>%
#   st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%
#   st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=km") %>%
#   mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2])
# 
# do_dat <- ecomon_dat %>% 
#   na.omit(o2_mgl) %>% 
#   filter(o2_mgl > 0,
#          depth > 0)
# 
# ggplot(do_dat, aes(x = o2_umolkg, y = -depth, color = year)) +
#   geom_point()
# 
# ggplot(do_dat, aes(x = year, group = as.factor(month), fill = as.factor(month))) +
#   geom_histogram()
# ggplot(do_dat, aes(x = longitude, y = latitude, color = as.factor(month), size = o2_umolkg)) +
#   geom_point(alpha = 0.25) +
#   facet_wrap(~year) +
#   theme_minimal()
#   

# saveRDS(ecomon_dat, "data/processed_data/ecomon_processed.rds")

## 04-compile all o2 data -------
# ecomon_dat <- readRDS(here::here("data/processed_data/ecomon_processed.rds"))

ecomon <- ecomon_dat %>% 
  mutate(survey = "ecomon") %>%
  rename(temp = temperature_C,
         o2 = o2_umolkg,
         sigma0 = sigma0_kgm3) %>%
  select(survey, year, doy, X, Y, latitude, longitude, temp, o2, salinity_psu, sigma0, depth) %>% 
  sf::st_drop_geometry() %>% 
  tidyr::drop_na(depth, o2, temp, sigma0)

# saveRDS(ecomon, here::here("data/processed_data/all_o2_dat.rds"))
## 05-Estimate depths by region -------
# ecomon <- readRDS(here::here("data/processed_data/all_o2_dat.rds"))

latrange <- range(ecomon$latitude)
lonrange <- range(ecomon$longitude)

# get noaa bathymetry data
noaa_depths <- marmap::getNOAA.bathy(lon1 = lonrange[1],
                                     lon2 = lonrange[2],
                                     lat1 = latrange[1],
                                     lat2 = latrange[2],
                                     resolution = 4,
                                     keep = TRUE)
depths_sp <- marmap::as.SpatialGridDataFrame(noaa_depths)
# depths_sf <- sf::st_as_sf(depths_sp, crs = geographic_crs)

# depths_sf <- dplyr::filter(depths_sf, layer <0)
# sf::st_crs(depths_sf) <- projected_crs

## Update CRS to use km instead of m
depths_sf <- depths_sp %>% 
  sf::st_as_sf(crs = geographic_crs) %>% 
  sf::st_transform(crs = projected_crs) %>% 
  dplyr::filter(layer < 0)

wkt_meters <- sf::st_crs(depths_sf)$wkt
sf::st_geometry(depths_sf) <- sf::st_geometry(depths_sf) / 1000
wkt_km <- gsub('LENGTHUNIT\\["metre",1\\]', 'LENGTHUNIT\\["kilometre",1000\\]', wkt_meters)
sf::st_crs(depths_sf) <- wkt_km

# depths_sf <- sf::st_set_crs(depths_sf, projected_crs)

data(northwest_atlantic_grid, package="VAST")
regions <- northwest_atlantic_grid %>% 
  filter(EPU %in% c("Gulf_of_Maine", "Mid_Atlantic_Bight", "Georges_Bank")) %>% 
  select(longitude = Lon,
         latitude = Lat,
         region = EPU) %>% 
  as.data.frame()

# polygons_grid <- as.data.frame(regions[,c("longitude", "latitude", "region")])

#Coordinate system
regions_sf <- regions %>%
  sf::st_as_sf( coords = c( "longitude", "latitude" ), crs = geographic_crs, remove = FALSE) %>% 
  sf::st_transform(crs = projected_crs) %>% 
  group_by(region)

wkt_meters <- sf::st_crs(regions_sf)$wkt
sf::st_geometry(regions_sf) <- sf::st_geometry(regions_sf) / 1000
wkt_km <- gsub('LENGTHUNIT\\["metre",1\\]', 'LENGTHUNIT\\["kilometre",1000\\]', wkt_meters)
sf::st_crs(regions_sf) <- wkt_km

# regions_sf <- sf::st_set_crs(regions_sf, projected_crs)

#For each region, create a geometry and then convex hull polygon
regions_hull <- regions_sf %>%
  group_by(region) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()
# plot(regions_hull)
# save
saveRDS(regions_hull, file = "data/processed_data/neus_regions_hull.rds")


##Clip the bathymetry to the survey polygon
#Create a function that does this
cut_region <- function(x){
  test <- depths_sf[x,]
}

#Get survey names, separate into a list so lapply is easier to use
region_names <- regions_hull$region
regions_hull_list <- split(regions_hull[,2], seq(nrow(regions_hull)))
names(regions_hull_list) <- region_names

#Run this function for each set of polygons (the o2 and the catch ones)--this creates a list of the clipped sf data
bathymetry_regions_sf <- lapply(regions_hull_list, cut_region)

maxdepth <-  max(ecomon$depth)*1.1

# x = bathymetry_regions_sf$Georges_Bank
#Create a function to convert the clipped sf lists above to dataframes

sf_to_dataframe <- function(x){
  
  #Get coordinates and depth
  tmp <- sf::st_coordinates(x)
  depthlist <- -x$layer
  
  #Create tibble
  newdepth <- tibble(longitude = tmp[,1],
                     latitude = tmp[,2],
                     noaadepth = depthlist)
  #Filter the depth to the depths of the data
  newdepth <- newdepth %>%
    filter(noaadepth <= 1.1 * maxdepth )
 
  #Convert the coordinate reference system to the X and Y that we use
  newdepth <- newdepth %>%
    sf::st_as_sf(coords = c('longitude','latitude'), crs = projected_crs, remove = F) %>%
    # st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=km") %>%
    sf::st_transform(crs = projected_crs) %>% 
    mutate(X = sf::st_coordinates(.)[,1], Y = st_coordinates(.)[,2])
}

#Run function on each region for each of the two sets of polygons
bath_regions<- lapply(bathymetry_regions_sf, sf_to_dataframe)

#Save lists of dataframes
saveRDS(bath_regions, file="data/processed_data/neus_bathymetry_regions_from_grids.rds")

#Separate out the dataframes for plotting
bath_gom <- bath_regions[["Gulf_of_Maine"]]
bath_mab <- bath_regions[["Mid_Atlantic_Bight"]]
bath_gb <- bath_regions[["Georges_Bank"]]

#Re-combine into one dataframe with columns for region for more plotting
bath_gom$region <- "gom"
bath_mab$region <- "mab"
bath_gb$region <- "gb"
bathy_all <- as.data.frame(bind_rows(bath_gom, bath_mab, bath_gb))

saveRDS(bathy_all, file = here::here("data/processed_data/neus_bathymetry_regions_dataframe.rds"))

# ggplot(bathy_all, aes(x=longitude, y=latitude))+geom_point(aes(colour=-noaadepth), size=0.2)
           

## 06-predict depth by region filter ctd -----

# load bathymetry data
# bathy_all <- readRDS(here::here("data/processed_data/neus_bathymetry_regions_from_grids.rds"))

# load regional polygons
# regions_hull <- readRDS(here::here("data/processed_data/neus_regions_hull.rds"))


make_depth_model <- function(bathydat) {
  
  spde <- sdmTMB::make_mesh(data = as.data.frame(bathydat), 
                            xy_cols = c("X", "Y"), 
                            # n_knots = 300)
                            cutoff = 45)
  
  depth_model <- sdmTMB::sdmTMB(log(noaadepth) ~ 1,
                        data = as.data.frame(bathydat),
                        spatial = "on", 
                        mesh = spde,
                        family = gaussian()
  )
}
# ggplot(bathy_all, aes(x=longitude, y=latitude))+geom_point(aes(colour=region), size=0.2)
# bathydat <- bath_regions[[2]]
# tt <- as.data.frame(bathydat)
depth_models <- lapply(X = bath_regions,
                       FUN = make_depth_model)

# load all data
# ecomon <- readRDS("data/processed_data/all_o2_dat.rds")
# dat_df <- ecomon %>% 
#   select(-X, -Y) %>% 
#   # sf::st_drop_geometry() %>% 
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = geographic_crs) %>% 
#   sf::st_transform(projected_crs)

# dat_df <-  sf::st_as_sf(ecomon, coords = c("X", "Y"), crs = projected_crs)
dat_df <- ecomon_dat %>% 
  rename(temp = temperature_C,
         o2 = o2_umolkg,
         sigma0 = sigma0_kgm3) %>%
  select(year, doy, X, Y, latitude, longitude, temp, o2, salinity_psu, sigma0, depth) %>% 
  # sf::st_drop_geometry() %>% 
  tidyr::drop_na(depth, o2, temp, sigma0)

# cycle through all regions
region_list <- c("Georges_Bank", "Gulf_of_Maine", "Mid_Atlantic_Bight")

# ggplot() +
#   geom_sf(data = ecomon_dat, fill = "black", alpha = 0.2) +
#   geom_sf(data = regions_sf, aes(color = region))+
#   # geom_sf(data = ecomon_dat, color = "red", size = 2) +
#   # geom_sf(data = poly, fill = "lightblue", alpha = 0.7) +
#   theme_minimal() +
#   labs(title = "Visual Check: Do your points and polygons overlap?")

i = 1

for (i in 1:length(region_list)) {
  region <- region_list[i]
  poly <- regions_hull[i,2]
  model.2.use <- depth_models[[i]]
  # pull out observations within each region
  region_dat  <- sf::st_filter(dat_df, poly)
  
  # get predicted log(depth) for each observation, based on model fit to that region
  region_dat_predict <- predict(model.2.use, as.data.frame(region_dat))
  region_dat_predict$region <- region
  
  # save results in new data frame called dat_predict
  if (i == 1)
    dat_predict <- region_dat_predict
  if (i > 1)
    dat_predict <- bind_rows(dat_predict, region_dat_predict)
}

# extract latitude and longitude so that dataframe is consistent with others
# lon_lats <- sf::st_coordinates(dat_predict$geometry)
# dat_predict$longitude <- lon_lats[,1]
# dat_predict$latitude <- lon_lats[,2]

###Evaluate predictions
##Set ggplot themes
# theme_set(theme_bw(base_size = 25))
# theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Create a column for CTD vs synoptic
# ctd_dat <- dat_predict %>%
#   filter(!survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))
# ctd_dat$type <- "ctd"
# 
# trawl_dat <- dat_predict %>%
#   filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))
# trawl_dat$type <- "synoptic"

# dat_predict <- bind_rows(trawl_dat, ctd_dat)



##Plot
# ggplot(dat_predict, aes(x=log(depth), y=est))+
#   geom_point(aes(color = as.factor(year)), alpha = 0.2, show.legend = FALSE)+
#   geom_abline()+
#   facet_wrap(~region)+
#   coord_equal() +
#   xlab("Log(depth) Reported") +
#   ylab("Log(depth) Estimated") +
#   theme_minimal()

source(here::here("code/helper_funs.R"))
# Calculate RMSE by region for synoptic measurements (trawl / longline) 
rmse <- dat_predict %>%
  group_by(region) %>%
  summarise(rmse = Metrics::rmse(log(depth), est))

# calculate difference between observed / expected in log space for CTD casts
dat_predict$depth_error <- with(dat_predict, log(depth) - est)

# Loop through regions, for each, pull out CTD data for that region, and retain 
# rows where depth residual (in log space) is < 2 * RMSE for that region
for (i in 1:length(region_list)) {
  # extract CTD casts in this region
  ctd_region_dat <- dplyr::filter(dat_predict, region == region_list[i],
                                  abs(depth_error) <= 2 * rmse$rmse[i])
  if (i == 1) ctd_dat_filtered <- ctd_region_dat
  if (i > 1) ctd_dat_filtered <- bind_rows(ctd_dat_filtered, ctd_region_dat)
}


# ggplot(ctd_dat_filtered %>% filter(o2 > 0), aes(x = X, y = Y)) +
#   geom_point(aes(color = o2)) +
#   facet_wrap(~year) +
#   scale_color_viridis_c() +
#   theme_minimal()
# combine filtered CTD data with synoptic trawl data
# dat_filtered <- bind_rows(trawl_dat, ctd_dat_filtered)

# select columns that we want to retain
dat_filtered <- ctd_dat_filtered %>%
  select(year, doy, X, Y, latitude, longitude, temp, o2, sigma0, salinity_psu, depth, region)

# make this a dataframe by removing the geometry
dat_filtered <- st_drop_geometry(dat_filtered)

# save file
saveRDS(object = dat_filtered, file = "data/processed_data/all_o2_dat_filtered.rds")


## 07-fit_o2_models -----

### setup up mapping #----
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = projected_crs)

#Load oxygen data
dat_filtered <- as.data.frame(readRDS("data/processed_data/all_o2_dat_filtered.rds"))

#Remove any rows with missing data
dat_cleaned <- dat_filtered %>%
  tidyr::drop_na(depth, o2, temp, sigma0, doy, X, Y, year) %>% 
  filter(depth > 0,
         o2 > 0,
         o2 < 1500) %>% 
  mutate(sigma0 = if_else(sigma0 <= 24, 
                          24,
                          sigma0),
         depth_ln = log(depth),
         o2_scaled = o2/100#,
         # temp_scaled = as.numeric(scale(temp)),
         # sigma0_scaled = as.numeric(scale(sigma0))
) %>% 
#Restrict analysis to years with more than 50 observations
  group_by(year) %>% 
  filter(n() > 50) %>% 
  ungroup()

## CV
# dat_region <- dat_cleaned # %>% 
  # filter(region == "Mid_Atlantic_Bight") 

year_map <- dat_cleaned %>%
  distinct(year) %>%
  arrange(year) %>%
  mutate(fold_id = row_number())

dat_cleaned <- dat_cleaned %>%
  left_join(year_map, by = join_by(year))

dat_split <- dat_cleaned %>% 
  rsample::initial_split(0.8)  


training_data <- rsample::training(dat_split) 
testing_data <- rsample::testing(dat_split) 

my_recipe <- recipes::recipe(o2_scaled ~ ., data = training_data) %>%
  recipes::step_normalize(temp, sigma0, depth_ln)

rec_prepped <- recipes::prep(my_recipe, training = training_data)

baked_training_data <- recipes::bake(rec_prepped, new_data = training_data)
baked_testing_data <- recipes::bake(rec_prepped, new_data = testing_data)


spde <- sdmTMB::make_mesh(data = baked_training_data,
                          xy_cols = c("X", "Y"),
                          # cutoff = 45)
                          n_knots = 250)

## Table 2 
model_frame <- data.frame(
  equation = paste0("m", 1:12),
  structure = c(rep("None", 3), rep("Spatial", 3), rep("Annual", 3), rep("Spatiotemporal", 3)),
  spatial = c(rep("off", 3), rep("on", 9)),
  annual = c(rep("off", 6), rep("on", 3), rep("off", 3)),
  spatiotemporal = c(rep("off", 9), rep("ar1", 3)),
  temp = rep(c("off", "on", "on"), 4),
  sal = rep(c("off", "off", "on"), 4)
) %>%
  mutate(
    formula = purrr::pmap(list(annual, temp, sal), function(a, t, s) {
      terms <- c("s(depth_ln)", "s(doy, bs = 'cc')")
      if (t == "on") terms <- c(terms, "s(temp)")
      if (s == "on") terms <- c(terms, "s(sigma0)")
      
      intercept <- if (a == "on") "0" else "1"
      
      rhs <- paste(c(intercept, terms), collapse = " + ")
      as.formula(paste("o2_scaled ~", rhs))
    })
  )


run_models <- function(formula, spatial, spatiotemporal, annual, data_input) {
  
  k_val <- length(unique(data_input$year))
  
  # Define the core sdmTMB arguments in a list
  args <- list(
    formula = formula,
    data = data_input,
    mesh = spde,
    family = gaussian(),
    spatial = spatial,
    spatiotemporal = spatiotemporal,
    k_folds = k_val,
    fold_ids = data_input$fold_id,
    parallel = FALSE
  )
  
  # Determine if ANY time component exists (Annual RW or Spatiotemporal)
  use_time <- FALSE
  
  if (annual == "on") {
    args$time_varying <- as.formula("~ 1")
    args$time <- "year"
    use_time <- TRUE
  }
  
  # Conditionally set time and extra_time for temporal models
  if (spatiotemporal != "off") {
    args$time <- "year"
    use_time <- TRUE
  }
  # Nested logic: extra_time is only relevant for AR1/RW models
  if (use_time) {
    # calculate missing years based on the full sequence
    all_years <- tidyr::full_seq(data_input$year, period = 1)
    extra_years <- setdiff(all_years, unique(data_input$year))
    
    if (length(extra_years) > 0) {
      args$extra_time <- extra_years
    }
  }
  
  # Call the function with the dynamically built list of arguments
  do.call(sdmTMB::sdmTMB_cv, args)
}

# Create a directory for the saved models
if (!dir.exists(here::here("test_cases/NEUS/model_cache"))) dir.create(here::here("test_cases/NEUS/model_cache"))

# Wrapper function that handles the "Check, Run, Save" logic
run_and_save_model <- function(formula, spatial, spatiotemporal, annual, model_id, data_input) {
  
  # 1. Define the unique filename
  file_name <- paste0(here::here("test_cases/NEUS/model_cache/"), model_id, ".rds")
  
  # 2. Check if it already exists
  if (file.exists(file_name)) {
    message(paste("Skipping", model_id, "- already exists."))
    return(readRDS(file_name))
  }
  
  # 3. Force single-threaded execution
  Sys.setenv(OMP_NUM_THREADS = "1")
  Sys.setenv(MKL_NUM_THREADS = "1")
  Sys.setenv(OPENBLAS_NUM_THREADS = "1")
  Sys.setenv(BLIS_NUM_THREADS = "1")
  
  # 4. Run model 
  result <- tryCatch({
    fit <- run_models(formula, spatial, spatiotemporal, annual, data_input = data_input)
    fit 
  }, error = function(e) {
    # If the AR1 model crashes completely (before returning), capture the error
    return(paste("Error:", e$message))
  })
  
  # 5. Save the result immediately to disk
  saveRDS(result, file = file_name)
  
  return(result)
}


plan(multisession, workers = 8)

cv_fits <- model_frame %>% 
  mutate(cv_mods = furrr::future_pmap(
    .l = list(
      formula = formula,
      spatial = spatial,
      spatiotemporal = spatiotemporal, 
      annual = annual,
      model_id = equation 
    ),
    .f = run_and_save_model, 
    data_input = baked_training_data,
    .options = furrr::furrr_options(seed = TRUE,
                                    scheduling = 1,
                                    globals = c("run_models", "spde"))
  ))

plan(sequential)

get_cv_metrics <- function(file_path, formula_obj) {
  
  # 1. Load File
  if (!file.exists(file_path)) return(tibble(status = "File Not Found"))
  cv_obj <- readRDS(file_path)
  
  # 2. Check for Error Message
  if (is.character(cv_obj)) return(tibble(status = "Run Error", error_msg = cv_obj))
  
  # 3. Iterate through the models
  fold_fits <- cv_obj$models
  
  all_sane <- all(map_lgl(fold_fits, function(x) {
    # Check sanity for this specific fold
    s <- try(sdmTMB::sanity(x), silent = TRUE)
    if (inherits(s, "try-error")) FALSE else s$all_ok
  }))
  
  # 4. Calculate RMSE/MAE
  resp_var <- all.vars(as.formula(formula_obj))[1]
  df <- cv_obj$data
  errors <- df[[resp_var]] - df$cv_predicted
  
  tibble(
    status = "Success",
    all_converged = all(map_lgl(fold_fits, ~ .x$model$convergence == 0)),
    all_sane = all_sane,
    sum_loglik = cv_obj$sum_loglik,
    rmse = sqrt(mean(errors^2, na.rm = TRUE)),
    mae = mean(abs(errors), na.rm = TRUE)
  )
}

# get_cv_metrics <- function(file_path, formula_obj) {
#   
#   # --- 1. Basic Checks ---
#   if (is.na(file_path)) return(tibble(status = "Missing Path"))
#   if (!file.exists(file_path)) return(tibble(status = "File Not Found"))
#   
#   # --- 2. Load Object ---
#   cv_obj <- readRDS(file_path)
#   
#   if (is.character(cv_obj)) {
#     return(tibble(status = "Run Error", error_msg = cv_obj))
#   }
#   
#   # --- 3. Diagnostics ---
#   fold_fits <- cv_obj$models
#   
#   all_converged <- all(map_lgl(fold_fits, ~ .x$model$convergence == 0))
#   
#   all_sane <- all(map_lgl(fold_fits, function(x) {
#     s <- try(sdmTMB::sanity(x, silent = TRUE, gradient_thresh = 0.01))
#     if (inherits(s, "try-error")) FALSE else s$all_ok
#   }))
#   
#   # --- 4. Accuracy Metrics (RMSE / MAE) ---
#   
#   # ROBUST FIX: Extract response name from the formula passed from your dataframe
#   # We convert to formula just in case it's a string, then get the first variable
#   resp_var <- all.vars(as.formula(formula_obj))[1]
#   
#   # Extract Data
#   df <- cv_obj$data
#   
#   # Check if response column exists (Safety Check)
#   if (!resp_var %in% names(df)) {
#     return(tibble(status = "Error", error_msg = paste("Column", resp_var, "not found")))
#   }
#   
#   obs <- df[[resp_var]]
#   pred <- df$cv_predicted # This is the standard sdmTMB CV prediction column
#   
#   errors <- obs - pred
#   
#   tibble(
#     status = "Success",
#     all_converged = all_converged,
#     all_sane = all_sane,
#     sum_loglik = cv_obj$sum_loglik,
#     rmse = sqrt(mean(errors^2, na.rm = TRUE)),
#     mae = mean(abs(errors), na.rm = TRUE)
#   )
# }


results_frame <- model_frame %>% 
  mutate(model_path = here::here("test_cases/NEUS/model_cache", paste0( equation, ".rds")),
         metrics = purrr::map2(
           .x = model_path,
           .y = formula,
           .f = get_cv_metrics
         )) %>% 
  tidyr::unnest(metrics)

best_model_row <- results_frame %>% 
  filter(status == "Success", all_sane == TRUE) %>% 
  arrange(rmse) %>% 
  slice(1)

best_fit <- readRDS(best_model_row$model_path)


# 
# 
# 
# 
# summary_table <- cv_fits %>% 
#   slice(2) %>%
#   mutate(
#     # Calculate RMSE only for successful models, return NA for failed ones
#     rmse = map_dbl(cv_mods, ~{
#       if (is.null(.x)) {
#         NA_real_ # Return NA if the model is NULL
#       } else {
#         sqrt(mean((.x$data$o2_scaled - exp(.x$data$cv_predicted))^2, na.rm = TRUE))      }
#     }),
#     # Calculate MAE only for successful models, return NA for failed ones
#     mae = map_dbl(cv_mods, ~{
#       if (is.null(.x)) {
#         NA_real_ # Return NA if the model is NULL
#       } else {
#         mean(abs(.x$data$o2_scaled - exp(.x$data$cv_predicted)))      }
#     }),
#     converged = purrr::map_lgl(cv_mods, ~{
#         # 1. Extract the convergence code (an integer) from each of the k-fold fits
#       if(!is.null(.x)){
#         convergence_codes <- purrr::map_int(.x$fits, ~ .x$model$convergence)
#         
#         # 2. Return TRUE if and only if ALL folds have a convergence code of 0
#         all(convergence_codes == 0)
#       } else {
#         FALSE
#       }
#     }),
#     # Check convergence, returning FALSE for failed models
#     all_folds_ok = map_lgl(cv_mods, ~{
#       if (is.null(.x)) {
#         FALSE # A NULL model did not converge
#       } else {
#         # If the model exists, run the sanity checks
#         sanity_list <- purrr::map(.x$models, sdmTMB::sanity, silent = TRUE)
#         purrr::map_lgl(sanity_list, ~ .x$all_ok) %>% all()
#       }
#     })
#   ) %>%
#   select(-cv_mods) %>%
#   arrange(rmse)

## Look at fits by fold

predictions_by_fold <- cv_fits %>%
  select(equation, cv_mods) %>%
  mutate(pred_data = purrr::map(cv_mods, "data")) %>% 
  select(-cv_mods) %>% 
  tidyr::unnest(pred_data)

model_levels <- paste0("m", 1:12)
n_years <- length(unique(summary_table_fold$year))

summary_table_fold <- predictions_by_fold %>%
  group_by(equation, year) %>% # Group by Model and Year
  summarize(
    rmse = sqrt(mean((o2_scaled - cv_predicted)^2, na.rm = TRUE)),
    mae = mean(abs(o2_scaled - cv_predicted), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, rmse) # Sort to see the winner for each year

summary_table_fold <- summary_table_fold %>% 
  mutate(equation = factor(equation, levels = model_levels))

best_index <- which(model_levels == best_model_row$equation)

rect_data <- tibble(
  # X-axis spans from the start of the first tile (0.5) to the end of the last tile (n_years + 0.5)
  xmin = 0.5,
  xmax = n_years + 0.5,
  # Y-axis spans the entire winner's row index
  ymin = best_index - 0.5,
  ymax = best_index + 0.5
)

ggplot(summary_table_fold, aes(x = as.factor(year), y = equation, fill = rmse * 100)) +
  geom_tile() +
  geom_rect(data = rect_data,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = "black", fill = NA, linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_viridis_c() + # Darker = Better (Lower RMSE)
  scale_x_discrete(expand = c(0, 0)) + # Removes space around the tiles
  scale_y_discrete(expand = c(0, 0)) + # Removes space around the tiles
  labs(
    title = "Predictive Performance by Year",
    x = "Test Year",
    y = "Candidate Model",
    fill = "RMSE"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#08 - build final model -----

baked_full_data <- bake(rec_prepped, new_data = dat_cleaned)

spde_full <- sdmTMB::make_mesh(
  data = baked_full_data, 
  xy_cols = c("X", "Y"), 
  n_knots = 250
)

# best_model <- cv_fits %>% 
#   left_join(summary_table) %>% 
#   arrange(rmse) %>% 
#   slice(1) %>%
#   pull(cv_mods) %>% 
#   .[[1]]

# best_model_id <- summary_table %>% 
#   arrange(rmse) %>% 
#   slice(1) %>% 
#   pull(equation)
# 
# best_specs <- model_frame %>% 
#   filter(equation == best_model_id)


## Start here -- need to make sure fit_final_model aligns with "run models function above. 
fit_final_model <- function(specs, full_data, full_mesh) {
  
  # 1. Base Arguments
  args <- list(
    formula = specs$formula[[1]], # Extract formula from list-column
    data = full_data,
    mesh = full_mesh,
    family = gaussian(),
    spatial = specs$spatial,
    spatiotemporal = specs$spatiotemporal
  )
  
  # 2. Annual (Random Walk Intercept) logic
  if (specs$annual == "on") {
    args$time_varying <- as.formula("~ 1")
    args$time <- "year"
  }
  
  # 3. Spatiotemporal / Time logic
  if (specs$spatiotemporal != "off") {
    args$time <- "year"
  }
  
  # 4. Extra Time (Bridging potential gaps in the full data set)
  if (!is.null(args$time)) {
    all_years <- tidyr::full_seq(full_data$year, period = 1)
    extra_years <- setdiff(all_years, unique(full_data$year))
    
    if (length(extra_years) > 0) {
      args$extra_time <- extra_years
    }
  }
  
  # 5. Run sdmTMB
  do.call(sdmTMB::sdmTMB, args)
}

final_fit <- fit_final_model(
  specs = best_specs,
  full_data = baked_full_data,
  full_mesh = spde_full
)

# 1. Generate predictions on the training data
# This adds 'est' (link scale) and 'est_non_rf' (fixed effects only) columns
dat_plot <- predict(final_fit)

# 2. Calculate Residuals
# randomized = TRUE is best for checking normality, but strictly Gaussian models can use standard
dat_plot$residuals <- residuals(final_fit)

# 3. Unscale everything to raw units
# Assuming you used: o2_scaled = o2 / 100
# And log link (if you used Gamma) or Identity link (if Gaussian)
dat_plot <- dat_plot %>%
  mutate(
    # If Gaussian identity link:
    predicted_raw = est * 100, 
    observed_raw = o2_scaled * 100,
    
    # If you used Log link (Gamma):
    # predicted_raw = exp(est) * 100,
    # observed_raw = o2_scaled * 100
    
    # Depth for plotting
    depth_raw = exp(depth_ln)
  )

ggplot(dat_plot, aes(x = observed_raw, y = predicted_raw)) +
  # Use geom_hex if you have many points to avoid overplotting
  geom_bin2d(bins = 50) + 
  scale_fill_viridis_c() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Figure 1: Predicted vs. Observed Oxygen",
    x = "Observed Oxygen (raw units)",
    y = "Predicted Oxygen (raw units)"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1) # Keeps axes square


ggplot(dat_plot, aes(x = depth_raw, y = residuals)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "gam", color = "blue") + # Add a smoother to detect bias
  scale_x_log10() + # Log scale often helps visual inspection for depth
  labs(
    title = "Figure 2: Residuals by Bottom Depth",
    x = "Depth (log scale)",
    y = "Residuals"
  ) +
  theme_minimal()


ggplot(dat_plot, aes(x = X, y = Y, color = residuals)) +
  geom_point(size = 1, alpha = 0.8) +
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red") +
  coord_fixed() +
  labs(
    title = "Figure 3: Spatial Residuals",
    subtitle = "Red = Model Overpredicted; Blue = Model Underpredicted",
    color = "Residual"
  ) +
  theme_dark() # Dark theme makes colors pop

ggplot(dat_plot, aes(x = X, y = Y, color = predicted_raw)) +
  geom_point(size = 0.5) +
  scale_color_viridis_c(option = "magma") + # Magma is often good for O2 (Bright = High O2)
  facet_wrap(~year) +
  coord_fixed() +
  labs(
    title = "Figure 5: Predicted Bottom Oxygen",
    subtitle = "Modeled concentrations over space and time",
    color = "O2"
  ) +
  theme_minimal()


# Create a dummy dataset varying Depth, holding others at their mean
nd_depth <- data.frame(
  depth_ln = seq(min(dat_cleaned$depth_ln), max(dat_cleaned$depth_ln), length.out = 100),
  doy = mean(dat_cleaned$doy), # Fix seasonality
  temp = mean(dat_cleaned$temp), # Fix temp
  sigma0 = mean(dat_cleaned$sigma0), # Fix density
  year = 2014 # Pick a standard year
)

# Predict
p_depth <- predict(final_fit, newdata = nd_depth, se_fit = TRUE)

# Plot
ggplot(p_depth, aes(x = exp(depth_ln), y = est)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = est - 1.96 * est_se, ymax = est + 1.96 * est_se), alpha = 0.2) +
  labs(
    title = "Figure 6: Effect of Depth on Oxygen",
    subtitle = "Shaded area is 95% CI; all other variables held constant",
    x = "Depth (m)", y = "Predicted O2 (link scale)"
  ) + theme_minimal()



# 1. Create a prediction grid for ALL years (including the missing one)
# grid <- ... (your spatial grid replicated for every year)

# 2. Generate the index (area-weighted mean oxygen)
# area argument is the size of your grid cells (e.g., 4 km2)
index <- sdmTMB::get_index(
  predict(final_fit, newdata = grid, return_tmb_object = TRUE), 
  area = 4 
)

ggplot(index, aes(x = year, y = est)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(
    title = "Figure 7: Standardized Annual Oxygen Trend",
    subtitle = "Shows the estimated regional mean O2, including the interpolated missing year",
    y = "Total/Mean Oxygen Index"
  ) + theme_minimal()

preds <- predict(final_fit)

# Plot the Spatial Random Field (omega_s)
# If your model is 'm10-m12' (AR1), you might look at epsilon_st instead
ggplot(preds, aes(X, Y, color = omega_s)) +
  geom_point() +
  scale_color_gradient2() +
  labs(title = "Figure 8: Spatial Random Field (Unexplained Variance)")


# Predict with standard errors
preds_se <- predict(final_fit, newdata = grid, se_fit = TRUE)

ggplot(preds_se, aes(X, Y, fill = est_se)) +
  geom_raster() +
  scale_fill_viridis_c(option = "inferno", direction = -1) + # Light yellow = High Uncertainty
  facet_wrap(~year) +
  labs(
    title = "Figure 9: Prediction Uncertainty (Standard Error)",
    subtitle = "Lighter colors indicate areas with lower confidence"
  )



# colnames(cv_predictions)
# Plot observed vs. predicted values from the CV
ggplot(cv_predictions, aes(x = o2_scaled, y = cv_predicted)) +
  geom_point(aes(color = as.factor(year)), alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Observed Bottom Dissolved Oxygen", y = "Predicted Bottom Dissolved Oxygen (CV)", title = "Cross-Validation Performance") +
  tune::coord_obs_pred()

# Now you can inspect it
print(single_fit)

future::plan(future::multisession, workers = 4)
# This will run the CV in parallel across your available cores.
cv_fits <- furrr::future_pmap(
  .l = list(
    formula = model_frame$formula,
    spatial = model_frame$spatial,
    spatiotemporal = model_frame$spatiotemporal,
    time_arg = model_frame$time_arg,
    equation_name = model_frame$equation # Pass name for progress printing
  ),
  
  # .f is the function to apply to each set of arguments from .l
  .f = function(formula, spatial, time_arg, spatiotemporal, equation_name) {
    # This message helps track progress
    cat("Submitting job for model:", equation_name, "\n")
    
    # Run the cross-validation
    sdmTMB::sdmTMB_cv(
      formula = formula,
      data = training_data, # These variables are found in the main environment
      mesh = spde,
      time = time_arg,
      family = gaussian(),
      spatial = spatial,
      spatiotemporal = spatiotemporal,
      k_folds = 5
    )
  },
  # IMPORTANT: Use this option for reproducible results in parallel
  .options = furrr::furrr_options(seed = TRUE)
)

# It's good practice to shut down the parallel workers when you're done
future::plan(future::sequential)

# Assign the model names to the list for easy reference
names(cv_fits) <- model_frame$equation

# Now you can proceed with model comparison as before
# print(cv_fits$m12)
  
  
  #Create lists and matrices for storing RMSE and list for storing prediction datasets
  models <- c("persistent", "persistent_annual", "persistent_spatiotemporal", "null_temponly", "null_covariates", "persistent_temponly", "persistent_covariates", "annual_temponly", "annual_covariates", "spatiotemporal_temp","spatiotemporal_covariates", "null_null")
  models.2.use <- length(models)
  other.cols <- 2
  rmse_summary <- matrix(data=NA, nrow=length(yearlist), ncol=models.2.use + other.cols)
  colnames(rmse_summary) <- c("persistent", "persistent_annual", "persistent_spatiotemporal", "null_temponly", "null_covariates", "persistent_temponly", "persistent_covariates", "annual_temponly", "annual_covariates", "spatiotemporal_temp","spatiotemporal_covariates","null_null", "n_test","n_train")
  output <- list()
  
  
  
  
  
  # rsqlist <- rmselist <- rep(NA, length(yearlist))
  ##Fit model for each year of training data
  for (i in 1:length(yearlist)) {
    #Separate test and training data
    test_year <- yearlist[i]
    print(test_year)
    test_data <- dat.2.use %>%
      filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "ai") & year==test_year)
    train_data <- dat.2.use %>%
      filter(!((survey %in% c("nwfsc", "dfo", "goa", "EBS", "ai") & year==test_year)))
    train_data <- as.data.frame(train_data)
    test_data <- as.data.frame(test_data)
    #Determine if extra time is needed
    extra_years <- setdiff(yearlist, unique(train_data$year))
    #Are years continuous?
    train_years <- unique(train_data$year)
    train_years2 <- seq(from=min(train_years), to=max(train_years), by=1)
    extra_years2 <- setdiff(train_years2, train_years)
    extra_years <- append(extra_years, extra_years2)
    
    if(length(extra_years)==0) {extra_years = NULL}   
    ## Make Mesh and fit model ####
    spde <- make_mesh(data = train_data,
                      xy_cols = c("X", "Y"),
                      cutoff = 45)
    ##No covariates
    #Persistent
    print("fitting m1")
    m1 <- try(sdmTMB(
      formula = o2  ~ 1+s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off"
    ))
    #Peristent_annual
    print("fitting m2")
    m2 <- try(sdmTMB(
      formula = o2  ~ 1+ as.factor(year)+ s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      time = "year",
      spatial = "on",
      spatiotemporal  = "off",
      extra_time=c(extra_years)
    ))
    #Persistent_spatiotemporal
    print("fitting m3")
    m3 <- try(sdmTMB(
      formula = o2  ~ 1+ s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      time = "year",
      spatial = "on",
      spatiotemporal  = "ar1",
      extra_time=c(extra_years)
    ))
    ##Covariates
    #Null--no random effects
    print("fitting m4")
    m4 <- try(sdmTMB(
      formula = o2  ~ 1+s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "off",
      spatiotemporal  = "off"
    ))
    print("fitting m5")
    m5 <- try(sdmTMB(
      formula = o2  ~ 1 + s(sigma0) + s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "off",
      spatiotemporal  = "off"
    ))
    #Covariates + persistent
    print("fitting m6")
    m6 <- try(sdmTMB(
      formula = o2  ~ 1+s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off"
    ))
    print("fitting m7")
    m7 <- try(sdmTMB(
      formula = o2  ~ 1 + s(sigma0) + s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off"
    ))
    #Covariates + annual
    print("fitting m8")
    m8 <- try(sdmTMB(
      formula = o2  ~ 1+as.factor(year)+s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off"
    ))
    print("fitting m9")
    m9 <- try(sdmTMB(
      formula = o2  ~ 1 + as.factor(year)+s(sigma0) + s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off"
    ))
    #Covariates+spatiotemporal
    print("fitting m10")
    m10 <- try(sdmTMB(
      formula = o2  ~ 1+s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      time="year",
      spatiotemporal  = "ar1",
      extra_time=c(extra_years)
    ))
    print("fitting m11")
    m11 <- try(sdmTMB(
      formula = o2  ~ 1 +s(sigma0) + s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      time="year",
      spatiotemporal  = "ar1",
      extra_time=c(extra_years)
    ))
    print("fitting m12")
    m12 <-try(sdmTMB(
      formula = o2  ~ 1 + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatiotemporal  = "off",
      spatial="off",
      extra_time=c(extra_years)
    ))
    
    models <- list(m1,m2,m3, m4,m5,m6, m7, m8, m9,m10,m11, m12)
    names(models) <- models
    tmp.preds <- list()
    #Predict data from each model and calculate RMSE
    for (j in 1:length(models)){
      # Predict onto data
      test_predict_O2 <- try(predict(models[[j]], newdata = test_data))
      if(scale){
        test_predict_O2$est <- test_predict_O2$est*100
        test_predict_O2$o2 <- test_predict_O2$o2*100
        test_predict_O2$est2 <- ifelse(test_predict_O2$est<0, 0, test_predict_O2$est)
      }
      test_predict_O2$residual = try(test_predict_O2$o2 - test_predict_O2$est)
      test_predict_O2$residual2 = try(test_predict_O2$o2 - test_predict_O2$est2)
      rmse_summary[i,j] <- try(rmse(test_predict_O2$o2, test_predict_O2$est), silent=T)
      #    rmse_summary2[i,j] <- try(rmse(test_predict_O2$o2, test_predict_O2$est2), silent=T)
      tmp.preds[[j]] <- test_predict_O2
      #Number of datapoints in each year for calculating overall RMSE late
      if(j==1){
        ncols <- ncol(rmse_summary)
        rmse_summary[i,ncols -1] <- nrow(test_data)
        rmse_summary[i,ncols] <- nrow(train_data)
      }
    }
    tmp.output <- list(train_data, test_data, tmp.preds, models)
    names(tmp.output) <-c("train_data", "test_data", "predictions", "models")
    names(tmp.preds) <- models
    output[[i]] <- tmp.output
  }
  #Plot
  if(plotmodel){
    print("plots")
    try(plot_simple(tmp.output, dat.2.use))
    if(is.list(models[5])){
      print("marginal effects")
      try(plot_marginal_effects(models, tmp.preds, dat.2.use, 4))
    }
  }
  
  
  # #Clean RMSE table
  rmse_summary <- as.data.frame(rmse_summary)
  rmse_summary$year <- yearlist
  rmse_summary$region <- test_region
  # rmse_summary$persistent_spatial <- as.numeric(rmse_summary$persistent_spatial)
  # rmse_summary$persistent_spatial_year <- as.numeric(rmse_summary$persistent_spatial_year)
  # rmse_summary$year_temp_salinity <- as.numeric(rmse_summary$year_temp_salinity)
  # rmse_summary$temp_salinity_spatiotemporal <- as.numeric(rmse_summary$temp_salinity_spatiotemporal)
  # 
  # rmse_summary2 <- as.data.frame(rmse_summary2)
  #  rmse_summary2$year <- yearlist
  # rmse_summary2$region <- test_region
  # rmse_summary2$persistent_spatial <- as.numeric(rmse_summary2$persistent_spatial)
  # rmse_summary2$persistent_spatial_year <- as.numeric(rmse_summary2$persistent_spatial_year)
  # rmse_summary2$year_temp_salinity <- as.numeric(rmse_summary2$year_temp_salinity)
  # rmse_summary2$temp_salinity_spatiotemporal <- as.numeric(rmse_summary2$temp_salinity_spatiotemporal)
  # 
  ##Save models
  if (savemodel) {
    names(output) <- yearlist
    save(x = output, file = paste("outputs/o2_models_", test_region, ".Rdata", sep=""))
  }
  
  if(plotmodel){
    #Plot RMSE and save
    rmse_long <- pivot_longer(rmse_summary, 1:models.2.use, names_to="model")
    #Remove rows with less than n=50 in test data
    rmse_long <- filter(rmse_summary, n_test>50)
    rmse_long <- pivot_longer(rmse_long, 1:models.2.use, names_to="model")
    ggplot(rmse_long, aes(x=year, y=value))+
      geom_col(aes(fill=model), position="dodge")+
      ylab("RMSE")+
      ggtitle(paste(plot_title))+
      xlab("Year")+
      theme(legend.position="top")+
      theme_set(theme_bw(base_size = 15))+
      theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(paste("outputs/", plot_title, "_rmse_plot.pdf", sep=""))
  }
  
  #Calculate overall RMSE
  rmse_total <- as.data.frame(sapply(rmse_summary[,1:models.2.use], calc_rmse, rmse_summary$n_test))
  colnames(rmse_total) <- "rmse_total"
  print(rmse_total)
  
  #Save
  saveRDS(rmse_summary, file=paste("outputs/rmse_years_", test_region, ".rds", sep=""))
  #  saveRDS(rmse_summary2, file=paste("outputs/temp/rmse2_years_", test_region, ".rds", sep=""))
  saveRDS(rmse_total, file=paste("outputs/rmse_total_", test_region, ".rds", sep=""))
  
  #Return RMSE table
  return(rmse_summary)
}
#Apply to region

rmse_cc <- fit_models1(dat, test_region="cc", "California Current")
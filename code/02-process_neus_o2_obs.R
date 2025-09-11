library(rerddap)
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
fishbot_info <- rerddap::info(datasetid = "fishbot_realtime", url = "https://erddap.ondeckdata.com/erddap")

ecomon_info <- rerddap::info(datasetid = "ocdbs_v_erddap1", url = "https://comet.nefsc.noaa.gov/erddap/")
ecomon_path <- here::here("data/oxygen options/NEUS/ocdbs_v_erddap1.csv")
fishbot_path <- here::here("data/oxygen options/NEUS/fishbot_realtime.csv")

dat1 <- rerddap::tabledap(fishbot_info, url = "https://erddap.ondeckdata.com/erddap",
                          store = disk(here::here("data/oxygen options/NEUS/")))

file.rename(from = attr(dat1, "path"),
            to = fishbot_path)
dat1 <- NULL
dat1 <- read.csv(fishbot_path,
                header = TRUE,
                na.strings = "NaN")


dat <- rerddap::tabledap(ecomon_info, url = "https://commet.nefsc.noaa.gov/erddap/",
                         store = disk(here::here("data/oxygen options/NEUS/")))

file.rename(from = attr(dat, "path"),
            to = ecomon_path)
dat <- NULL
dat <- read.csv(ecomon_path,
                header = TRUE,
                na.strings = "NaN")

ecomon_units <- dat[1,]
# ecomon_dat <- dat[-1,]

ecomon_dat <- dat %>% 
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
  tidyr::drop_na(any_of(c("longitude", "latitude"))) %>% ## remove samples without Lat and Long
  st_as_sf(coords=c('longitude','latitude'), crs=4269, remove = F) %>%
  st_transform(crs = "+proj=utm +zone=19 +dat1um=WGS84 +units=km") %>%
  mutate(X = st_coordinates(.)[,1],Y = st_coordinates(.)[,2])

fishbot_dat <- dat1 %>%
  slice(-1) %>% 
  mutate(date = as.Date(time),
         year = year(date), 
         month = month(date),
         doy = yday(date),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         depth = as.numeric(depth),
         o2_mgl = as.numeric(dissolved_oxygen),
         # pressure_dbar = as.numeric(pressure_dbars),
         salinity_psu = as.numeric(salinity),
         temperature_C = as.numeric(temperature)) %>% 
  select(year, month, doy, longitude, latitude, depth, #pressure_dbar, 
         salinity_psu, o2_mgl, temperature_C, data_provider) %>% 
  na.omit(c("longitude", "latitude")) %>% 
  st_as_sf(coords=c('longitude','latitude'), crs=4269, remove = F) %>%
  st_transform(crs = "+proj=utm +zone=19 +dat1um=WGS84 +units=km") %>%
  mutate(X = st_coordinates(.)[,1],Y = st_coordinates(.)[,2])
  

tt <- dat1 %>%
  slice(-1) %>% 
  filter(data_provider %in% c("ECOMON", "eMOLT")) %>% 
  mutate(date = as.Date(time),
         year = year(date), 
         month = month(date),
         doy = yday(date),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         depth = as.numeric(depth),
         o2_mgl = as.numeric(dissolved_oxygen),
         salinity_psu = as.numeric(salinity),
         temperature_C = as.numeric(temperature)) %>% 
  select(year, month, doy, longitude, latitude, depth, #pressure_dbar,
         salinity_psu, o2_mgl, temperature_C, data_provider)


#convert oxygen mg/L to umol_kg
fishbot_SA = gsw::gsw_SA_from_SP(fishbot_dat$salinity_psu, 
                         fishbot_dat$depth,
                         fishbot_dat$longitude,
                         fishbot_dat$latitude) #absolute salinity for pot T calc
fishbot_pt = gsw::gsw_pt_from_t(fishbot_SA,
                        fishbot_dat$temperature_C,
                        fishbot_dat$depth) #potential temp at a particular depth
fishbot_CT = gsw::gsw_CT_from_t(fishbot_SA,
                        fishbot_dat$temperature_C,
                        fishbot_dat$depth) #conservative temp
fishbot_dat$sigma0_kgm3 = gsw::gsw_sigma0(fishbot_SA,
                                          fishbot_CT)
fishbot_dat$O2_umolkg = fishbot_dat$o2_mgl*44660/(fishbot_dat$sigma0_kgm3+1000)


ggplot(fishbot_dat %>% filter(!is.na(O2_umolkg)), aes(x = O2_umolkg, y = -depth, color = year)) +
  geom_point()

do_dat <- fishbot_dat %>% 
  na.omit(o2_mgl)
ggplot(do_dat, aes(x = year, group = as.factor(month), fill = as.factor(month))) +
  geom_histogram()
ggplot(do_dat, aes(x = longitude, y = latitude, color = as.factor(month), size = O2_umolkg)) +
  geom_point()+
  facet_wrap(~year)
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
ecomon_SA = gsw::gsw_SA_from_SP(ecomon_dat$salinity_psu, 
                         ecomon_dat$depth,
                         ecomon_dat$longitude,
                         ecomon_dat$latitude) #absolute salinity for pot T calc
ecomon_pt = gsw::gsw_pt_from_t(ecomon_SA,
                        ecomon_dat$temperature_C,
                        ecomon_dat$depth) #potential temp at a particular depth
ecomon_CT = gsw::gsw_CT_from_t(ecomon_SA,
                        ecomon_dat$temperature_C,
                        ecomon_dat$depth) #conservative temp
ecomon_dat$sigma0_kgm3 = gsw::gsw_sigma0(ecomon_SA, 
                                         ecomon_CT) # Potential density anomaly 
ecomon_dat$O2_umolkg = ecomon_dat$o2_mgl * 44660/(ecomon_dat$sigma0_kgm3 + 1000)


# #Convert coordinates
# #Remove with missing coordinates
# dat <- dat %>%
#   st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%
#   st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=km") %>%
#   mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2])

do_dat <- ecomon_dat %>% 
  na.omit(o2_mgl) %>% 
  filter(o2_mgl > 0,
         depth > 0)

ggplot(do_dat, aes(x = O2_umolkg, y = -depth, color = year)) +
  geom_point()

ggplot(do_dat, aes(x = year, group = as.factor(month), fill = as.factor(month))) +
  geom_histogram()
ggplot(do_dat, aes(x = longitude, y = latitude, color = as.factor(month), size = O2_umolkg)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~year) +
  theme_minimal()
  

saveRDS(ecomon_dat, "data/processed_data/ecomon_processed.rds")

## 04-compile all o2 data -------

ecomon <- ecomon_dat %>% 
  mutate(survey = "ecomon") %>% 
  rename(temp = temperature_C,
         o2 = O2_umolkg,
         sigma0 = sigma0_kgm3) %>%
  select(survey, year, doy, X, Y, latitude, longitude, temp, o2, salinity_psu, sigma0, depth) %>% 
  sf::st_drop_geometry() %>% 
  tidyr::drop_na(depth, o2, temp, sigma0)

saveRDS(ecomon, here::here("data/processed_data/all_o2_dat.rds"))
## 05-Estimate depths by region -------

latrange <- range(ecomon$latitude)
lonrange <- range(ecomon$longitude)

latrange <- range(ecomon$latitude)
lonrange <- range(ecomon$longitude)

# get noaa bathymetry data
noaa_depths <- marmap::getNOAA.bathy(lon1 = -180,
                                     lon2 = 180,
                                     lat1 = latrange[1],
                                     lat2 = latrange[2],
                                     resolution = 4,
                                     keep = TRUE)
depths_sp <- marmap::as.SpatialGridDataFrame(noaa_depths)
depths_sf <- sf::st_as_sf(depths_sp, crs = sf::st_crs(4269))
depths_sf <- dplyr::filter(depths_sf, layer <0)
epu_crs <- sf::st_crs(depths_sf)


data(northwest_atlantic_grid, package="VAST")
regions <- northwest_atlantic_grid %>% 
  filter(EPU %in% c("Gulf_of_Maine", "Mid_Atlantic_Bight", "Georges_Bank")) %>% 
  rename(longitude = Lon,
         latitude = Lat)

polygons_grid <- as.data.frame(regions[,c("longitude", "latitude", "EPU")])
#Coordinate system
regions.sf <- polygons_grid %>%
  sf::st_as_sf( coords = c( "longitude", "latitude" ), crs = epu_crs)
#For each region, create a geometry and then convex hull polygon
regions.hull <- regions.sf %>%
  group_by(EPU) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()

# save
saveRDS(regions.hull, file = "data/processed_data/neus_regions_hull.rds")


##Clip the bathymetry to the survey polygon
#Create a function that does this
cut_region <- function(x){
  test <- depths_sf[x,]
}

#Get survey names, separate into a list so lapply is easier to use
region_names <- regions.hull$EPU
regions.hull <- split(regions.hull[,2], seq(nrow(regions.hull)))
names(regions.hull) <- region_names

#Run this function for each set of polygons (the o2 and the catch ones)--this creates a list of the clipped sf data
bathymetry_regions_sf <- lapply(regions.hull, cut_region)

maxdepth <-  max(ecomon$depth)*1.1

# x = bathymetry_regions_sf$Georges_Bank
#Create a function to convert the clipped sf lists above to dataframes
sf_to_dataframe <- function(x){
  #Get coordinates and depth
  tmp <- st_coordinates(x)
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
    st_as_sf(coords=c('longitude','latitude'),crs = epu_crs,remove = F) %>%
    st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=km") %>%
    mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2])
}

#Run function on each region for each of the two sets of polygons
bath_regions<- lapply(bathymetry_regions_sf, sf_to_dataframe)

plot(bath_regions$Georges_Bank$noaadepth)

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
           

## 06-predict depth by region filter ctd

# load bathymetry data
bathy_all <- readRDS(here::here("data/processed_data/neus_bathymetry_regions_from_grids.rds"))
# load regional polygons
regions.hull <- readRDS(here::here("data/processed_data/neus_regions_hull.rds"))


make_depth_model <- function(bathydat) {
  
  spde <- sdmTMB::make_mesh(data = as.data.frame(bathydat), xy_cols = c("X", "Y"), n_knots = 300)
  
  depth_model <- sdmTMB::sdmTMB(log(noaadepth) ~ 1,
                        data = as.data.frame(bathydat),
                        spatial = "on", 
                        mesh = spde,
                        family = gaussian()
  )
}


depth_models <- lapply(X = bathy_all,
                       FUN = make_depth_model)



# load all data
dat <- readRDS("data/processed_data/all_o2_dat.rds")
dat_df <-  st_as_sf(dat, coords = c("longitude", "latitude"), crs = epu_crs)

# cycle through all regions
region_list <- c("Gulf_of_Maine", "Mid_Atlantic_Bight", "Georges_Bank")
# i = 1


for (i in 1:length(region_list)) {
  region <- region_list[i]
  poly <- regions.hull[i,2]
  model.2.use <- depth_models[[i]]
  # pull out observations within each region
  region_dat  <- st_filter(dat_df, poly)
  
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
lon_lats <- sf::st_coordinates(dat_predict$geometry)
dat_predict$longitude <- lon_lats[,1]
dat_predict$latitude <- lon_lats[,2]

###Evaluate predictions
##Set ggplot themes
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Create a column for CTD vs synoptic
ctd_dat <- dat_predict %>%
  # filter(!survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))
ctd_dat$type <- "ctd"

trawl_dat <- dat_predict %>%
  filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))
trawl_dat$type <- "synoptic"

dat_predict <- bind_rows(trawl_dat, ctd_dat)



##Plot
ggplot(dat_predict, aes(x=log(depth), y=est))+
  geom_point(aes(color = as.factor(year)))+
  geom_abline()+
  facet_wrap("survey", scales="free")+
  xlab("Log(depth) Reported")+
  ylab("Log(depth) Estimated")

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

# combine filtered CTD data with synoptic trawl data
dat_filtered <- bind_rows(trawl_dat, ctd_dat_filtered)

# select columns that we want to retain
dat_filtered <- dat_filtered %>%
  select(survey, year, doy, X, Y, latitude, longitude, temp, o2, sigma0, salinity_psu, depth, region)

# make this a dataframe by removing the geometry
dat_filtered <- st_drop_geometry(dat_filtered)

# save file
write_rds(x = dat_filtered, file = "data/processed_data/all_o2_dat_filtered.rds")




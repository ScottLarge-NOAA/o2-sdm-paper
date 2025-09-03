# ------------------------------------------------------------------------------
# 02-estimate_depths_by_region.R
#
# Adapted R script (i.e., 05-estimate_depths_by_region.R)
# from Julia Indivero's oxygen skill testing repo to skill test MOM6 oxygen 
# outputs against the Joint U.S.-Canada Pacific Hake Acoustic Trawl Survey in 
# situ data
# https://github.com/jindivero/o2-sdm-paper#
#
# Step 2: Create region polygons and add NOAA bathymetry
#
# Created by Julia Indivero
# Adapted: Aug 14th, 2025 by Olivia Gemmell, UW
#
# ------------------------------------------------------------------------------
# # Install the remotes package if you don't already have it
# install.packages("remotes")
# 
# # Install FishStatsUtils from GitHub
# remotes::install_github("James-Thorson/FishStatsUtils")


library(marmap)
library(sf)
library(dplyr)
library(ggplot2)
library(mapview)
library(devtools)
library(FishStatsUtils)
library(readr)

# set root directory
root_dir <- "test_cases/mom6/"

#### Oxygen data--get regions
dat <- readRDS(paste0(root_dir, "data/all_o2_dat.rds"))

latrange <- range(dat$latitude)
lonrange <- range(dat$longitude)

# ------------------------------------------------------------------------------
# get noaa bathymetry data
# ------------------------------------------------------------------------------
noaa_depths <- getNOAA.bathy(lon1 = -180,
                             lon2 = 180,
                             lat1 = latrange[1],
                             lat2 = latrange[2],
                             resolution = 4,
                             keep = TRUE)
depths_sp <- as.SpatialGridDataFrame(noaa_depths)
depths_sf <- st_as_sf(depths_sp, crs = st_crs(4326))
depths_sf <- dplyr::filter(depths_sf, layer <0)

# ------------------------------------------------------------------------------
# create regions
# ------------------------------------------------------------------------------
###Assign a column to label data in each region
##california current
data("california_current_grid")
cc <- california_current_grid
cc$region <- "cc"

##British columbia
data("bc_coast_grid")
bc <- bc_coast_grid
bc$region <- "bc"

##Combine back together
regions <- bind_rows(cc, bc)

#Check that all points were included
regions <- dplyr::rename(regions, longitude = Lon, latitude = Lat)

####Create a convex hull polygon for region using those survey extents
#Separate just columns of interest
polygons_grid <- as.data.frame(regions[,c("longitude", "latitude", "region")])
#Coordinate system
regions.sf <- polygons_grid %>%
  st_as_sf( coords = c( "longitude", "latitude" ), crs = 4326 )
#For each region, create a geometry and then convex hull polygon
regions.hull <- regions.sf %>%
  group_by(region) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()
# create bc, cc combined region 
bc_cc_hull <-  regions.sf %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()
bc_cc_hull$region <- "bc_cc"
# combined
regions.hull <- rbind(regions.hull, bc_cc_hull)

# save
write_rds(x = regions.hull, file = paste0(root_dir, "data/regions_hull.rds"))
#Check
# make sure points are sf
datapoints_sf <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326)
# Add them to your existing map
mapview(regions.hull) + mapview(datapoints_sf, col.regions = "red")

#Get survey names, separate into a list so lapply is easier to use
region_names <- regions.hull$region
regions.hull.list <- split(regions.hull[,2], seq(nrow(regions.hull)))
names(regions.hull.list) <- region_names

##Clip the bathymetry to the survey polygon
#Create a function that does this
cut_region <- function(x){
  test <- depths_sf[x,]
}

#Run this function for each set of polygons (the o2 and the catch ones)--this creates a list of the clipped sf data
bathymetry_regions_sf <- lapply(regions.hull.list, cut_region)

##Convert these sf lists to dataframes and clean up
#Get max depth in surveys for filtering the bathymetry data
maxdepth <-  max(dat$depth)*1.1

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
    st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
    mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2])
}

#Run function on each region for each of the two sets of polygons
bath_regions<- lapply(bathymetry_regions_sf, sf_to_dataframe)

#Save lists of dataframes
saveRDS(bath_regions, file= paste0(root_dir, "data/bathymetry_regions_from_grids.rds"))

#Separate out the dataframes for plotting
bath_bc <- bath_regions[["bc"]]
bath_cc <- bath_regions[["cc"]]
bath_bc_cc <- bath_regions[["bc_cc"]]

#check
mapview::mapview(bath_bc)
mapview::mapview(bath_cc)
mapview::mapview(bath_bc_cc)

#Re-combine into one dataframe with columns for region for more plotting
bath_bc$region <- "bc"
bath_cc$region <- "cc"
bath_bc_cc$region <- "bc_cc"
bath_all <- as.data.frame(bind_rows(bath_bc, bath_cc, bath_bc_cc))

saveRDS(bath_all, file=paste0(root_dir, "data/bathymetry_regions_dataframe.rds"))

ggplot(bath_all, aes(x=longitude, y=latitude))+geom_point(aes(colour=region), size=0.2)


# ------------------------------------------------------------------------------
# Add region column to oxygen dataframe
# ------------------------------------------------------------------------------
# Convert to sf
dat_sf <-  st_as_sf(dat, coords = c("longitude", "latitude"), crs = st_crs(4326))

# pull out observations within each region
region_list <- c("bc", "cc", "bc_cc")

for (i in 1:length(region_list)) {
  # find overlapping data points for each region polygon
  poly <- regions.hull[i, 2]
  region_dat  <- st_filter(dat_sf, poly)
  region_dat <- as.data.frame(region_dat)
  # Add latitude and longitude columns back
  lon_lats <- sf::st_coordinates(region_dat$geometry)
  region_dat$longitude <- lon_lats[,1]
  region_dat$latitude <- lon_lats[,2]
  # remove geometry columns
  region_dat <- region_dat %>%
    select(-geometry)
  # add region column 
  region_dat$region <- region_list[i]
  # save results in new data frame called dat_predict
  if (i == 1)
    dat_final <- region_dat
  if (i > 1)
    dat_final <- bind_rows(dat_final, region_dat)
}
# remove o2 outliers
dat_final <- dat_final[dat_final$o2 >= 0 & dat_final$o2 <= 500,]
# remove depths > 500 m because CTD only goes to 500 m
dat_final <- dat_final[dat_final$depth <= 500,]


# save
write_rds(x = dat_final, file = paste0(root_dir, "data/all_o2_dat_region.rds"))




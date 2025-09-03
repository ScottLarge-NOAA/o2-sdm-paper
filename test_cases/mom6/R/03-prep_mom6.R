# ------------------------------------------------------------------------------
# 03-prep_mom6.R
#
# Grabs MOM6 output by year, converts to dataframe, matches format, and saves rds
# files ready for model fitting
# 
# Step 3: prep MOM6 data for model fitting
#
# Created: Aug 14th, 2025 by Olivia Gemmell, UW
#
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(sf)
library(ncdf4)
library(furrr)
library(lubridate)
library(readr)

# set root directory
root_dir <- "test_cases/mom6/"

source(paste0(root_dir, "R/helper_funs.R"))

# ------------------------------------------------------------------------------
# Get MOM6 output by year and save 
# ------------------------------------------------------------------------------
# plan your parallel backend
plan(multisession, workers = floor(parallelly::availableCores()/2))  # adjust workers as needed

# OPeNDAP server URL
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northeast_pacific/full_domain/hindcast/monthly/regrid/r20250818/o2.nep.full.hcast.monthly.regrid.r20250818.199301-202312.nc"

# open once to grab metadata
ncopendap <- nc_open(url)
lon <- ncvar_get(ncopendap, "lon")
lat <- ncvar_get(ncopendap, "lat")
depth <- ncvar_get(ncopendap, "z_l")
time <- ncvar_get(ncopendap, "time")
time_units <- ncatt_get(ncopendap, "time", "units")$value
origin_string <- sub(".*since ", "", time_units)
origin_date <- as.Date(origin_string)
dates_all <- origin_date + time
nc_close(ncopendap)

# specify region of interest
lats <- c(30, 60)
lons <- c(225, 240)
lat_inds <- which(lat >= lats[1] & lat <= lats[2])
lon_inds <- which(lon >= lons[1] & lon <= lons[2])

# ------------------------------------------------------------------------------
# 2011
# ------------------------------------------------------------------------------
# specify year of interest
target_year <- 2011
# find indices of time steps in that year
year_inds <- which(format(dates_all, "%Y") == target_year)

# all combinations of time and depth indices
grid <- expand.grid(
  t = year_inds,  # target year
  d = seq_along(depth)
)

# run in parallel
o2_df_2011 <- future_pmap_dfr(grid, ~fetch_slice(..1, # first column in grid (t)
                                                 ..2, # second column in grid (d)
                                                 var = "o2")) 

# convert to matching format
o2_df_2011 <- convert_mom6(df = o2_df_2011)

# save
write_rds(o2_df_2011, 
          paste0(root_dir, "data/mom6/o2_df_2011.rds"))

# ------------------------------------------------------------------------------
# 2012
# ------------------------------------------------------------------------------
# specify year of interest
target_year <- 2012
# find indices of time steps in that year
year_inds <- which(format(dates_all, "%Y") == target_year)

# all combinations of time and depth indices
grid <- expand.grid(
  t = year_inds,  # target year
  d = seq_along(depth)
)

# run in parallel
o2_df_2012 <- future_pmap_dfr(grid, ~fetch_slice(..1, # first column in grid (t)
                                                 ..2, # second column in grid (d)
                                                 var = "o2")) 

# convert to matching format
o2_df_2012 <- convert_mom6(df = o2_df_2012)

# save
write_rds(o2_df_2012, 
          paste0(root_dir, "data/mom6/o2_df_2012.rds"))


# ------------------------------------------------------------------------------
# 2013
# ------------------------------------------------------------------------------
# specify year of interest
target_year <- 2013
# find indices of time steps in that year
year_inds <- which(format(dates_all, "%Y") == target_year)

# all combinations of time and depth indices
grid <- expand.grid(
  t = year_inds,  # target year
  d = seq_along(depth)
)

# run in parallel
o2_df_2013 <- future_pmap_dfr(grid, ~fetch_slice(..1, # first column in grid (t)
                                                 ..2, # second column in grid (d)
                                                 var = "o2")) 

# convert to matching format
o2_df_2013 <- convert_mom6(df = o2_df_2013)

# save
write_rds(o2_df_2013, 
          paste0(root_dir, "data/mom6/o2_df_2013.rds"))


# ------------------------------------------------------------------------------
# 2015
# ------------------------------------------------------------------------------
# specify year of interest
target_year <- 2015
# find indices of time steps in that year
year_inds <- which(format(dates_all, "%Y") == target_year)

# all combinations of time and depth indices
grid <- expand.grid(
  t = year_inds,  # target year
  d = seq_along(depth)
)

# run in parallel
o2_df_2015 <- future_pmap_dfr(grid, ~fetch_slice(..1, # first column in grid (t)
                                                 ..2, # second column in grid (d)
                                                 var = "o2")) 

# convert to matching format
o2_df_2015 <- convert_mom6(df = o2_df_2015)

# save
write_rds(o2_df_2015, 
          paste0(root_dir, "data/mom6/o2_df_2015.rds"))


# ------------------------------------------------------------------------------
# All dataframes combined
# ------------------------------------------------------------------------------
# combine all dataframes 
o2_df_all <- rbind(o2_df_2011,
                   o2_df_2012,
                   o2_df_2013,
                   o2_df_2015)

# save 
write_rds(o2_df_all, 
          paste0(root_dir, "data/mom6/o2_df_all.rds"))

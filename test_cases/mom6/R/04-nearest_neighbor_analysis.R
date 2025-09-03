# ------------------------------------------------------------------------------
# 04-nearest_neighbor_analysis.R
#
# Grabs MOM6 output at location, depth, and date of each in situ observation, 
# 
# Step 4: Compare MOM6 and in situ oxygen data using a simple nearest neighbor 
# analysis
#
# Created: Aug 29th, 2025 by Olivia Gemmell, UW
#
# ------------------------------------------------------------------------------

library(docstring)
library(ncdf4)
library(lubridate)
library(terra)
library(sf)
library(readr)


# set root directory
root_dir <- "test_cases/mom6/"

source(paste0(root_dir, "R/helper_funs.R"))

# ------------------------------------------------------------------------------
# Grab MOM6 data at location, depth, and date of in situ data
# ------------------------------------------------------------------------------
# load in situ data
in_situ <- readRDS(paste0(root_dir, "data/all_o2_dat_region.rds"))
in_situ <- in_situ[in_situ$region == "bc_cc", ]

# load MOM6 oxygen data
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northeast_pacific/full_domain/hindcast/monthly/regrid/r20250818/o2.nep.full.hcast.monthly.regrid.r20250818.199301-202312.nc"
# Read in a subset of MOM6 data based on unique dates and depths in the in situ data
# The 3D data is too large to pull it all at once using the get_mom6() function
# so we will pull the data one depth layer at a time and add it to the 
# in situ dataframe
# load depth values from netCDF file
nc <- nc_open(url)
depth_range <- ncvar_get(nc, "z_l")
i_min <- nearest_index(min(in_situ$depth), depth_range)
i_max <- nearest_index(max(in_situ$depth), depth_range)
# Make sure the indices are ordered correctly
depth_inds <- seq(min(i_min, i_max), max(i_min, i_max))
depths <- depth_range[i_min:i_max]

# add columns needed to extract MOM6 values
in_situ$lon_360 <- convert_to_360(lon = in_situ$longitude)
in_situ$lat <- in_situ$latitude
in_situ$depth_mom6 <- sapply(in_situ$depth, function(d) {
  depth_range[which.min(abs(depth_range - d))]
})

# Add MOM6 o2 values one depth layer at a time
for(d in depths){
  # grab MOM6 data for one depth level
  o2_list <- get_mom6(url = url,
                      var = "o2",
                      lats = c(30, 60),
                      lons = c(225, 240),
                      date = as.Date(unique(in_situ$date)),
                      dims = "3D",
                      depth = d)
  # Find the rows that match this depth
  idx <- which(in_situ$depth_mom6 == d)
  
# Get O2 values ONLY for those rows
  o2_mom6 <- add_mom6(
    df = in_situ[idx, ],
    list = o2_list,
    col_name = "o2_nn",
    dims = "3D",
    return_vector = TRUE)
  
  if(d == depths[1]){
    in_situ$o2_nn <- NA
    in_situ$o2_nn[idx] <- o2_mom6
  }else{
    in_situ$o2_nn[idx] <- o2_mom6
  }
}

# convert o2 values to um/kg
in_situ$o2_nn <- in_situ$o2_nn*1000000

# ------------------------------------------------------------------------------
# interpolate o2_nn values to match exact depths from CTD casts
# ------------------------------------------------------------------------------
# group by location and interpolate
# if the group has only 1 valid o2_nn value assign that value to all depths in 
# group 
# if all o2_nn values are NA in the group assign NA
# will assign values on the tail ends the nearest o2_nn value 
in_situ <- in_situ %>%
  group_by(longitude, latitude) %>%
  mutate(o2_nn_interp = {
    valid <- !is.na(o2_nn)
    # Case 1: Two or more non-NA values -> interpolate
    if (sum(valid) >= 2) {
      approx(
        x = depth_mom6[valid],
        y = o2_nn[valid],
        xout = depth,
        rule = 2
      )$y
    } 
    # Case 2: Exactly one non-NA -> fill with that value
    else if (sum(valid) == 1) {
      rep(o2_nn[valid], length(depth))
    } 
    # Case 3: All NAs -> return NA
    else {
      rep(NA_real_, length(depth))
    }
  }) %>%
  ungroup()

# save
write_rds(in_situ, 
          paste0(root_dir, "data/nearest_neighbor.rds"))






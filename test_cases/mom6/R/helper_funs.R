
simplify_df <- function(input_dat, makedoy = F) {
  # see if DOY is present, if not, add it
  if (makedoy) input_dat$doy <- as.POSIXlt(input_dat$date, format = "%Y-%b-%d")$yday
  output_dat <- input_dat %>%
    rename(temp = temperature_C,
           o2 = O2_umolkg,
           sigma0 = sigma0_kgm3) %>%
    select(survey, year, month, doy, date, X, Y, latitude, longitude, temp, o2, salinity_psu, sigma0, depth)
  
  # see if geometry is present, if so, delete
  if ("geometry" %in% names(output_dat)) output_dat <- sf::st_drop_geometry(output_dat)
  
  return(output_dat)
}


### Fetch one time × depth slice from MOM6 NetCDF ###
fetch_slice <- function(t, 
                        d, 
                        var) {
  # open connection inside worker
  nc <- nc_open(url)
  
  var_slice <- ncvar_get(nc, var,
                         start = c(min(lon_inds),  # lon
                                   min(lat_inds),  # lat
                                   d,              # depth
                                   t),             # time
                         count = c(length(lon_inds), 
                                   length(lat_inds), 
                                   1,              # single depth
                                   1))             # single time
  
  nc_close(nc)
  
  # convert to dataframe
  df <- expand.grid(
    longitude = lon[lon_inds],
    latitude = lat[lat_inds]
  )
  df$depth <- depth[d]
  df$time <- dates_all[t]
  df$o2 <- as.vector(var_slice)
  
  # enforce column order
  df <- df[, c(var, "longitude", "latitude", "depth", "time")]
  
  return(df)
}
# END fetch_slice()


### Convert MOM6 output to matching format ###
convert_mom6 <- function(df){
  # Make columns numeric
  df$longitude <- as.numeric(df$longitude)
  df$latitude <- as.numeric(df$latitude)
  df$depth <- as.numeric(df$depth)
  # convert any longitude on a d=360 scale to -180 to +180
  fix_index <- which((df$longitude) > 180)
  df$longitude[fix_index] <- df$longitude[fix_index] - 360
  # Add utm cooridinates, doy, year, and month columns
  df <- df %>%
    st_as_sf(coords = c('longitude','latitude'),
             crs = "OGC:CRS84",
             remove = F) %>%
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
    mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>%
    st_set_geometry(NULL)
  df$doy <- as.POSIXlt(df$time, format = "%Y-%b-%d")$yday
  df$year <- year(df$time)
  df$month <- month(df$time)
  # convert o2 from mol/kg to umol/kg
  df$o2 <- df$o2*1000000
  # remove negative o2 values and NAs
  df <- df[!is.na(df$o2 >= 0), ]
  return(df)
}
# END convert_mom6()


# ------------------------------------------------------------------------------
# Pull a subset of monthly MOM6 data from the CEFI portal
# ------------------------------------------------------------------------------
get_mom6 <- function(url,
                     var,
                     lats,
                     lons,
                     date,
                     dims,
                     depth = NULL){
  #' Pull a subset of MOM6 data from the CEFI data portal
  #' 
  #' @description
  #' Reads a subset of monthly MOM6 data from a netCDF file loaded from a user 
  #' specified URL at a specified location, time, and depth (if 3D). Supports
  #' 2D or 3D resolutions.
  #' 
  #' @param url Character. URL for the netCDF file generated from the 
  #' CEFI portal.
  #' @param var Character. The name of the variable to read from the netCDF 
  #' file.
  #' @param lats Numeric. Latitudinal range of interest in degrees north 
  #' (e.g., c(20, 40)).
  #' @param lons Numeric. Longitudinal range of interest in degrees east 
  #' (e.g., c(220, 250)).
  #' @param date Vector. Dates of interest in YYYY-MM-DD format.
  #' @param dims Character. Dimensions of the netCDF file, can be "3D" or "2D".
  #' @param depth Numeric. The target depth layer to pull data from the netCDF
  #' file. Optional; used only for 3D datasets. Default set to NULL.
  #'
  #' @returns A list with the following elements:
  #' \describe{
  #'   \item{data}{3D (if dims = "2D") or 4D (if dims = "3D") array of the
  #'   selected variable with dimensions [lon, lat, time] or [lon, lat, depth, time]}
  #'   \item{lon}{Numeric vector of longitude values corresponding to the subset region}
  #'   \item{lat}{Numeric vector of latitude values corresponding to the subset region}
  #'   \item{depth}{Vector of depth value(s) corresponding to the depth layer(s) pulled from MOM6. Displayed in 4D array only}
  #'   \item{dates}{Vector of `Date` objects corresponding to the selected time indices}
  #' }
  #'
  #' @examples
  #' \dontrun{
  #' # Example usage for 2D monthly surface temperature data
  #' library(ncdf4)
  #' library(lubridate)
  #' 
  #' # URL to remotely hosted MOM6 NetCDF data
  #' url <- "https://example.com/mom6_file.nc"
  #' 
  #' # Variable name to extract
  #' var <- "tos"  # surface ocean temperature
  #' 
  #' # Geographic bounds
  #' lats <- c(30, 50)
  #' lons <- c(220, 240)
  #' 
  #' # Time range of interest
  #' date <- as.Date(c("2005-01-01", "2005-02-01", "2005-03-01"))
  #' 
  #' # Extract data
  #' result <- get_mom6(url = url,
  #'                    var = var,
  #'                    lats = lats,
  #'                    lons = lons,
  #'                    date = date,
  #'                    dims = "2D")
  #' 
  #' # Plot data for first time step
  #' filled.contour(x = result$lon,
  #'                y = result$lat,
  #'                z = result$data[,,1],
  #'                main = paste("MOM6", var, "on", result$dates[1]))
  #' }
  
  # ----------------------------------------------------------------------------
  # 1. Data prep: open netCDF file and specify spatial range and dates
  # ----------------------------------------------------------------------------
  # specify date range
  target_dates <- as.Date(unique(date))
  # Open a NetCDF file lazily and remotely
  nc <- nc_open(url)
  # Read the data into memory based on spatial range and date range
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  # specify desired spatial range
  lat_inds <- which(lat >= lats[1] & lat <= lats[2])
  lon_inds <- which(lon >= lons[1] & lon <= lons[2]) 
  if (length(lat_inds) == 0 || length(lon_inds) == 0) stop("Lat/lon range out of bounds.")
  time <- ncvar_get(nc, "time")
  # Get time units
  time_units <- ncatt_get(nc, "time", "units")$value
  # Extract origin date from the time units string
  origin_string <- sub(".*since ", "", time_units)
  origin_date <- as.Date(origin_string)
  # Convert time to Dates
  dates_all <- origin_date + time 
  # convert to first of the month to match monthly resolution
  # dates from netCDF file
  dates_all_match <- floor_date(dates_all, "month")
  # User specified dates
  target_dates_match <- na.omit(unique(floor_date(target_dates, "month")))
  # Find matching time indices
  time_inds <- which(dates_all_match %in% target_dates_match)
  if (length(time_inds) == 0) stop("No matching dates found in NetCDF time dimension.")
  # Find matching depth indices
  if(!is.null(depth)){
    depth_range <- ncvar_get(nc, "z_l")
    cat("NetCDF has depth bins with the following midpoints:\n")
    print(depth_range)
    if(length(depth) < 2){
      depth_inds <- nearest_index(depth, depth_range)
      cat(paste0("depth bin ", depth_range[depth_inds], " selected."))
    }else{
      i_min <- nearest_index(min(depth), depth_range)
      i_max <- nearest_index(max(depth), depth_range)
      # Make sure the indices are ordered correctly
      depth_inds <- seq(min(i_min, i_max), max(i_min, i_max))
      cat(paste0("depth bins ", depth_range[min(depth_inds)], "-", depth_range[max(depth_inds)], " selected."))
    }
  }
  
  # ----------------------------------------------------------------------------
  # 2. Extract data from netCDF file and add to array
  # ----------------------------------------------------------------------------
  if(dims == "3D"){
    # Fast 3D extraction (bulk read over depth and time dimensions)
    data_array <- ncvar_get(nc, var,
                            start = c(min(lon_inds), 
                                      min(lat_inds), 
                                      min(depth_inds), 
                                      min(time_inds)),
                            count = c(length(lon_inds), 
                                      length(lat_inds), 
                                      length(depth_inds), 
                                      length(time_inds)))
    
    # Close NetCDF
    nc_close(nc)
    
    return(list(
      data = data_array, # [lon, lat, depth, time]
      lon = lon[lon_inds],
      lat = lat[lat_inds],
      depth = depth_range[depth_inds],
      dates = dates_all[time_inds]
    ))
  }else{
    # Fast 2D extraction (bulk read over time dimension)
    data_array <- ncvar_get(nc, var,
                            start = c(min(lon_inds), 
                                      min(lat_inds), 
                                      min(time_inds)),
                            count = c(length(lon_inds), 
                                      length(lat_inds), 
                                      length(time_inds)))
    
    nc_close(nc)
    
    return(list(
      data = data_array, # [lon, lat, time]
      lon = lon[lon_inds],
      lat = lat[lat_inds],
      dates = dates_all[time_inds]
    ))
  }
}
# END get_mom6()


# ------------------------------------------------------------------------------
# Add MOM6 values from get_mom6() output to a dataframe 
# ------------------------------------------------------------------------------
add_mom6 <- function(df,
                     list,
                     col_name,
                     dims,
                     return_vector = FALSE) {
  #' Add MOM6 values to a dataframe from get_mom6() output
  #'
  #' @description
  #' Adds variable values (e.g., SST) from a MOM6 NetCDF output list 
  #' (produced by `get_mom6()`) to a dataframe by spatially interpolating values
  #' at the observation locations and matching by date. If 3D, averages over depth
  #' layers before interpolation and fills in NAs using values at the next 
  #' deepest depth layer. If 2D, fills in NAs using the nearest non-NA cell
  #' via `get_nearest_non_na()`.
  #'
  #' @param df A dataframe with at least columns `lon_360`, `lat`, and `date`
  #' (date in YYYY-MM-DD format).
  #' @param list A list returned by `get_mom6()`, containing `data`, `lon`, 
  #' `lat`, `depth` (if 3D) and `dates`.
  #' @param col_name Character. The name of the new column to be added to
  #' \code{df}.
  #' @param dims Character. Dimensions of the `get_mom6()` list, can be "3D" or
  #' "2D".
  #' @param return_vector Boolean. Option to just return the extracted column
  #' values instead of the entire dataframe and the new column.
  #'
  #' @return A dataframe with a new column named \code{col_name} added, 
  #' containing the extracted MOM6 variable values.
  
  if (!all(c("lon_360", "lat", "date") %in% names(df))) {
    stop("df must contain 'lon_360', 'lat', and 'date' columns.")
  }
  
  df$date_match <- lubridate::floor_date(as.Date(df$date), "month")
  df[[col_name]] <- NA_real_
  
  for (i in seq_along(list$dates)) {
    this_date <- lubridate::floor_date(as.Date(list$dates[i]), "month")
    date_rows <- which(df$date_match == this_date)
    if (length(date_rows) == 0) next
    
    points <- terra::vect(df[date_rows, c("lon_360", "lat")],
                          geom = c("lon_360", "lat"),
                          crs = "EPSG:4326")
    
    if (dims == "2D" | dims == "3D" & length(list$depth) < 2) {
      data_slice <- list$data[, , i]
    } else if (dims == "3D") {
      data_slice <- apply(list$data[, , , i], c(1, 2), mean, na.rm = TRUE)
    } else {
      stop("dims must be '2D' or '3D'")
    }
    
    r <- terra::rast(
      nrows = length(list$lat),
      ncols = length(list$lon),
      xmin = min(list$lon),
      xmax = max(list$lon),
      ymin = min(list$lat),
      ymax = max(list$lat),
      vals = as.vector(data_slice[, length(list$lat):1])
    )
    crs(r) <- "EPSG:4326"
    
    extracted_vals <- terra::extract(r, points, method = "bilinear")[, 2]
    
    if (dims == "2D") {
      na_rows <- which(is.na(extracted_vals))
      if (length(na_rows) > 0) {
        for (j in na_rows) {
          extracted_vals[j] <- get_nearest_non_na(points[j, ], r)
        }
      }
    }
    
    df[[col_name]][date_rows] <- extracted_vals
  }
  
  # Always return a vector with the same length as df
  if (return_vector) {
    vals <- df[[col_name]]
    # Ensure correct length by padding NAs where nothing was matched
    if (length(vals) != nrow(df)) {
      vals <- rep(NA_real_, nrow(df))
    }
    return(vals)
  } else {
    return(df)
  }
}


# ------------------------------------------------------------------------------
# Find index of nearest value in a vector
# ------------------------------------------------------------------------------
# nearest_index()
nearest_index <- function(x, vec) {
  #' Returns the index of the element in a numeric vector that is closest to a
  #' specified value.
  #'
  #' @param x A numeric value to match.
  #' @param vec A numeric vector in which to find the closest value to \code{x}.
  #'
  #' @return An integer giving the index of the closest value in \code{vec}.
  #'
  #' @examples
  #' nearest_index(5.2, c(1, 3, 6, 8))
  #' # Returns: 3
  #'
  which.min(abs(vec - x))
}
# END nearest_index()


# ------------------------------------------------------------------------------
# Find nearest non-NA raster value for a point
# ------------------------------------------------------------------------------
get_nearest_non_na <- function(point, raster_layer) {
  #' This function finds the nearest non-NA raster value for a given spatial point
  #'
  #' @param point A `SpatVector` object representing a single spatial point.
  #' @param raster_layer A `SpatRaster` object from the `terra` package, in 
  #' which to search for the nearest non-NA value.
  #'
  #' @return A numeric value corresponding to the nearest non-NA raster cell.
  #'
  #' @examples
  #' \dontrun{
  #' library(terra)
  #' depth <- rast("data/raw/ocean_static.deptho.nc")
  #' pts <- vect(bmass, geom = c("lon_360", "lat"), crs = crs(depth))
  #' vals <- extract(depth, pts, method = "bilinear")
  #' corrected_values <- sapply(1:nrow(pts), function(j) {
  #'   if (is.na(vals[j, 2])) {
  #'     get_nearest_non_na(pts[j, ], depth)
  #'   } else {
  #'     vals[j, 2]
  #'   }
  #' })
  #' }
  #'
  # Compute distance to all raster cells
  distance_raster <- terra::distance(raster_layer, point)
  
  # Extract distances and raster values
  distances <- terra::values(distance_raster)
  raster_values <- terra::values(raster_layer)
  
  # Identify non-NA raster cells
  valid_indices <- which(!is.na(raster_values))
  
  # Find the nearest valid cell by minimizing distance
  nearest_valid_index <- valid_indices[which.min(distances[valid_indices])]
  
  # Return the value of the nearest valid raster cell
  return(raster_values[nearest_valid_index])
}
# END get_nearest_non_na()

# ------------------------------------------------------------------------------
# convert decimal degrees to 0-360
# ------------------------------------------------------------------------------
# convert_to_360()
convert_to_360 <- function(lon) {
  #' Convert Longitude to 0–360 Degree Format
  #'
  #' @description 
  #' Convert longitude values from the -180 to 180 degree system
  #' to the 0 to 360 degree system, which is commonly used in global climate
  #' and oceanographic datasets.
  #'
  #' @param lon A numeric vector of longitude values in decimal degrees 
  #' (-180 to 180).
  #'
  #' @return A numeric vector of longitudes in the 0 to 360 range.
  #'
  #' @examples
  #' convert_to_360(c(-123, 0, 150, -180))
  #' # Returns: 237, 0, 150, 180
  ifelse(lon < 0, lon + 360, lon)
}
# END convert_to_360()

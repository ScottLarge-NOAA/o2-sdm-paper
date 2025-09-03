# ------------------------------------------------------------------------------
# 01-process_o2_obs.R
#
# Adapted R scripts (i.e., 02-process_other_o2_obs.R and 04-compile_all_o2_data.R)
# from Julia Indivero's oxygen skill testing repo to skill test MOM6 oxygen 
# outputs against the Joint U.S.-Canada Pacific Hake Acoustic Trawl Survey in 
# situ data
# https://github.com/jindivero/o2-sdm-paper#
#
# Step 1: Process hake survey data
#
# Created by Julia Indivero
# Adapted: Aug 14th, 2025 by Olivia Gemmell, UW
#
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ncdf4)
library(lubridate)
library(sf)
library(seacarb)
library(readxl)

# set root directory
root_dir <- "test_cases/mom6/"

# source(paste0(root_dir, "R/util_funs.R"))
source(paste0(root_dir, "R/helper_funs.R"))


# ------------------------------------------------------------------------------
# Process hake survey data
# ------------------------------------------------------------------------------
### West Coast Hake Survey
#2011
#Location data
loc <- read_excel(paste0(root_dir, "data/hake/CTD-Location-Files/201103_ctd_locs.xlsx"))
#All the CTD files
all_files <- list.files(path = paste0(root_dir, "data/hake/CTD-data/2011-Hake"),
                        pattern=".csv", 
                        recursive=T)
dats_2011 <- list()
for(i in 1:length(all_files)){
  dat <- read.csv(paste0(root_dir, "data/hake/CTD-data/2011-Hake/",  all_files[i]))
  dat$ctd <- substr(all_files[i],1,nchar(all_files[i])-4)
  loc_i <- as.data.frame(loc[i,])
  dat <- bind_cols(dat, loc_i)
  dats_2011[[all_files[i]]] <- dat
}
#Combine
dats_2011 <- bind_rows(dats_2011)

#2012
loc <- read_excel(paste0(root_dir, "data/hake/CTD-Location-Files/201204_ctd_locs.xlsx"))
all_files <- list.files(path = paste0(root_dir, "data/hake/CTD-data/2012-Hake"),
                        pattern=".csv", 
                        recursive=T)
dats_2012 <- list()
for(i in 1:length(all_files)){
  dat <- read.csv(paste0(root_dir, "data/hake/CTD-data/2012-Hake/",  all_files[i]))
  dat$ctd <- substr(all_files[i],1,nchar(all_files[i])-4)
  loc_i <- as.data.frame(loc[i,])
  dat <- bind_cols(dat, loc_i)
  dats_2012[[all_files[i]]] <- dat
}

dats_2012 <- bind_rows(dats_2012)

#2013
loc <- read_excel(paste0(root_dir, "data/hake/CTD-Location-Files/201305_ctd_locs.xlsx"))
all_files <- list.files(path = paste0(root_dir, "data/hake/CTD-data/2013-Hake"),
                        pattern=".csv", 
                        recursive=T)
dats_2013 <- list()
for(i in 1:length(all_files)){
  dat <- read.csv(paste0(root_dir, "data/hake/CTD-data/2013-Hake/",  all_files[i]))
  dat$ctd <- substr(all_files[i],1,nchar(all_files[i])-4)
  loc_i <- as.data.frame(loc[i,])
  dat <- bind_cols(dat, loc_i)
  dats_2013[[all_files[i]]] <- dat
}

dats_2013 <- bind_rows(dats_2013)

#2015
loc <- read_excel(paste0(root_dir, "data/hake/CTD-Location-Files/201507_ctd_locs.xlsx"))
all_files <- list.files(path = paste0(root_dir, "data/hake/CTD-data/2015-Hake"),
                        pattern=".csv", 
                        recursive=T)
dats_2015 <- list()
for(i in 1:length(all_files)){
  dat <- read.csv(paste0(root_dir, "data/hake/CTD-data/2015-Hake/",  all_files[i]))
  dat$ctd <- substr(all_files[i],1,nchar(all_files[i])-4)
  loc_i <- as.data.frame(loc[i,])
  dat <- bind_cols(dat, loc_i)
  dats_2015[[all_files[i]]] <- dat
}

dats_2015 <- bind_rows(dats_2015)

#Combine
dats2 <- bind_rows(dats_2011, dats_2012, dats_2013, dats_2015)

#Columns of interest
#Columns needed
hake <- dats2[,c("T090C", "DepSM", "Sal00", "Sbeox0ML.L", "system (UTC)", "latitude (dec deg)", "longitude(dec deg)")]

#Relabel
colnames(hake) <- c("temperature_C", "depth", "salinity_psu", "do_mlpL", "date", "latitude", "longitude")

#Get lat and long in UTC coordinates
hake <- hake %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2])

#sigma and convert O2
SA = gsw_SA_from_SP(hake$salinity_psu,hake$depth,hake$longitude,hake$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,hake$temperature_C,hake$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,hake$temperature_C,hake$depth) #conservative temp
hake$sigma0_kgm3 = gsw_sigma0(SA,CT)
hake$O2_umolkg = hake$do_mlpL*44660/(hake$sigma0_kgm3+1000)

#Survey
hake$survey <- "hake"

#Type
hake$type <- "ctd"

#Get date in correct format
hake$date <- substr(hake$date, 1,10)
year <- substr(hake$date, nchar(hake$date)-3, nchar(hake$date))
month <- substr(hake$date, 1,2)
day <- substr(hake$date,4,5)
hake$date <- as.POSIXct(as.Date(with(dat,paste(year,month,day,sep="-")),"%Y-%m-%d"))
hake$date2 <- as.POSIXct(as.Date(with(hake,paste(year,month,day,sep="-")),"%Y-%m-%d"))


#DOY
hake$doy <- as.POSIXlt(hake$date, format = "%Y-%b-%d")$yday

#month and year
hake$month <- month(hake$date)
hake$year <- year(hake$date)

#Save
saveRDS(hake, paste0(root_dir, "data/hake_oxygen_processed.rds"))

# simplify the dataframe (extract only useful columns and rename)
hake <- simplify_df(hake)
# remove depth with error codes -999
hake <- dplyr::filter(hake, !depth == -999)

# remove rows with missing data
hake <- hake %>%
  drop_na(depth, o2, temp, sigma0)

saveRDS(hake, file = paste0(root_dir, "data/all_o2_dat.rds"))




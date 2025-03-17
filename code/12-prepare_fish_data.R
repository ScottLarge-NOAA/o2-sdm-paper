library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggplot2)
library(tidync)
library(sdmTMB)

setwd("~/Dropbox/GitHub/o2-sdm")
source("code/helper_funs.R")
source("code/util_funs.R")

dat_sablefish <- prepare_data(spc="sablefish", sci_name="anoplopoma fimbria", mi=F)
dat_dsole <- prepare_data(spc="dover sole", sci_name="microstomus pacificus", mi=F)
dat_lspiny <- prepare_data(spc="longspine thornyhead", sci_name="sebastolobus altivelis", mi=F)

#Clean up
dat_sablefish <- as.data.frame(dat[,c(1:34)])
dat_lspiny <- as.data.frame(dat_lspiny[,c(1:34)])

saveRDS(dat_sablefish, "data/processed_data/dat_sablefish.rds")
saveRDS(dat_dsole, "data/processed_data/dat_dsole.rds")

##Get GOBH interpolated to each fish trawl
dat <- readRDS("data/processed_data/dat_sablefish.rds")
dat <- as.data.frame(dat[,c(1:34)])
##GOBH predictions
#Set do threshold level for GLORYS data
do_threshold <- 0

#Filter days of GLORYS data to every 10th day?
filter_time <- T

#Filter GLORYS data to bottom depth? ()
filter_depth <- F

#Set base WD
basewd <-"~/Dropbox/GitHub/o2-sdm"

#Set GLORYS data location
gloryswd <- "~/Dropbox/GitHub/o2-sdm/data/GLORYS"

# load regional polygons
regions.hull <- readRDS("data/processed_data/regions_hull.rds")

#Remove
dat <- dat  %>%
  drop_na(depth,year)
#Add depth_ln for predicting
dat$depth_ln <- log(dat$depth)

#Apply
dat_gobh1 <- gobh_interpolate(filter(dat, survey=="nwfsc"), "cc", filter_depth, filter_time, gloryswd, basewd, do_threshold)
saveRDS(dat_gobh1, file="data/processed_data/gobh_trawls_cc.rds")
dat_gobh2 <- gobh_interpolate(filter(dat, survey=="dfo"), "bc", filter_depth, filter_time, gloryswd, basewd, do_threshold)
saveRDS(dat_gobh2, file="data/processed_data/gobh_trawls_bc.rds")
dat_gobh3 <- gobh_interpolate(filter(dat, survey=="EBS"|survey=="NBS"), "ebs", filter_depth, filter_time, gloryswd, basewd, do_threshold)
saveRDS(dat_gobh3, file="data/processed_data/gobh_trawls_goa.rds")
dat_gobh4 <- gobh_interpolate(filter(dat, survey=="GOA"), "goa", filter_depth, filter_time, gloryswd, basewd, do_threshold)
saveRDS(dat_gobh4, file="data/processed_data/gobh_trawls_ebs.rds")
#dat_gobh5 <- gobh_interpolate(dat, "ai", filter_depth, filter_time, gloryswd, basewd, do_threshold)
#saveRDS(dat_gobh5, file="data/processed_data/gobh_trawls_ai.rds")

#Combine
dat_gobh <- bind_rows(dat_gobh1, dat_gobh2,dat_gobh3,dat_gobh4)
dat_gobh <- select(dat_gobh, survey, event_id, est)
dat_gobh$o2_gobh <- dat_gobh$est*100
dat_gobh$est <- NULL

#Save
saveRDS(dat_gobh, file="data/processed_data/gobh_trawls.rds")

##Add to fish data
#Restrict to only CC and BC
dat_gobh <- readRDS(file="data/processed_data/gobh_trawls.rds")
dat_sablefish <- filter(dat_sablefish, survey=="nwfsc"|survey=="dfo")
dat_gobh2 <- filter(dat_gobh, survey=="nwfsc"|survey=="dfo")
dat_sablefish <- left_join(dat_sablefish, dat_gobh, by="event_id")

dat_dsole <- filter(dat_dsole, survey=="nwfsc"|survey=="dfo")
dat_dsole <- left_join(dat_dsole, dat_gobh, by=c("event_id", "survey"))

dat_lspiny <- filter(dat_lspiny, survey=="nwfsc"|survey=="dfo")
dat_lspiny <- left_join(dat_lspiny, dat_gobh2, by=c("event_id", "survey"))


#Re-save
saveRDS(dat_sablefish, "data/processed_data/dat_sablefish.rds")
saveRDS(dat_dsole, "data/processed_data/dat_dsole.rds")
saveRDS(dat_lspiny, "data/processed_data/dat_lthorny.rds")

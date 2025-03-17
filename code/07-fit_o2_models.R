remove.packages("sdmTMB")
install.packages("sdmTMB")
library(sdmTMB)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(rnaturalearth)
library(sf)
library(ggpubr)
library(visreg)

#Set wd
setwd("~/Dropbox/GitHub/o2-sdm")

#Load functions
source("code/util_funs.R")
source("code/helper_funs.R")

# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)

#Load oxygen data
dat <- as.data.frame(readRDS("data/processed_data/all_o2_dat_filtered.rds"))

#Remove any rows with missing data
dat <- dat %>%
  drop_na(depth, o2, temp, sigma0, doy, X, Y, year)

#Remove weird depths
dat <- filter(dat, depth>0)

#Remove oxygen outliers
dat <- filter(dat, o2<1500)

#Set minimum sigma
minsigma0 <- 24
dat$sigma0[dat$sigma0 <= minsigma0] <- minsigma0

# remove older (earlier than 2000) data
dat <- dplyr::filter(dat, year >=2000)

#Log depth
dat$depth_ln <- log(dat$depth)

#Save model outputs?
savemodel=T
#Plot models and save?
plotmodel = F
#Remove OCNMS?
ocnms =F
#Restrict testing years to just if more than 50 observations?
n_50 =T

#Scale o2?
scale <- T
if(scale==T){
  dat$o2 <- dat$o2/100
}

#test removing OCNMS
if(ocnms){
dat <- filter(dat, survey!="ocnms")
}

#Function to fit model for a specific region
fit_models1 <- function(dat, test_region, plot_title){
  ##Set up data
  #Filter to region
  dat.2.use <- as.data.frame(filter(dat, region==test_region))
  if(test_region=="goa") {dat.2.use <- filter(dat.2.use, !(survey=="ai")&!(survey=="EBS"))}
  #Just trawl survey data
  trawl_dat <- dat.2.use %>%
    filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "ai"))
  other_dat <- dat.2.use %>%
    filter(!(survey %in% c("nwfsc", "dfo", "goa", "EBS", "ai")))
  #Years in trawl data available
  yearlist <- sort(unique(trawl_dat$year))
  
  if(n_50){
    counts <- count(trawl_dat, year)
    counts <- filter(counts, n>50)
    yearlist <- sort(unique(counts$year))
  }
  
  #Remove years if no non-synoptic data available
  train_years <- unique(other_dat$year)
  yearlist <- yearlist[which(yearlist %in% train_years)]
  
  #Remove NA if there
  yearlist <- yearlist[!is.na(yearlist)]

  
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
rmse_bc <- fit_models1(dat, "bc", "British Columbia")
rmse_goa <- fit_models1(dat, "goa", "Gulf of Alaska")
rmse_ebs <- fit_models1(dat, "ebs", "Eastern Bering Sea")
rmse_ai <- fit_models1(dat, "ai", "Aleutian Islands")

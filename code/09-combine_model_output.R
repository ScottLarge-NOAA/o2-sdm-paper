library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(ggpattern)
library(marmap)
library(sf)
library(dplyr)
library(ggplot2)
library(mapview)
library(devtools)
library(FishStatsUtils)
setwd("~/Dropbox/GitHub/o2-sdm")
#Load functions
source("code/helper_funs.R")

###Synoptic only###
##Combine overall RMSE of each region into a single table
#Read files
rmse_total_cc <- readRDS("outputs/rmse_total_cc.rds")
rmse_total_bc <- readRDS("outputs/rmse_total_bc.rds")
rmse_total_goa <- readRDS("outputs/rmse_total_goa.rds")
rmse_total_ebs <- readRDS("outputs/rmse_total_ebs.rds")
rmse_total_ai <- readRDS("outputs/rmse_total_ai.rds")

#Combine
rmse_totals <- bind_cols(rmse_total_cc, rmse_total_bc, rmse_total_goa, rmse_total_ebs, rmse_total_ai)
rmse_totals$model <-  rownames(rmse_totals)

colnames(rmse_totals) <- c("cc", "bc","goa", "ebs", "ai", "model")
rmse_totals <- select(rmse_totals, model, cc, bc, goa, ebs, ai)

saveRDS(rmse_totals, file="outputs/rmse_total_combined.rds")

##Plot RMSE vs n_train
rmse_cc <- readRDS("outputs/rmse_years_cc.rds")
rmse_bc <- readRDS("outputs/rmse_years_bc.rds")
rmse_goa <- readRDS("outputs/rmse_years_goa.rds")
rmse_ebs <- readRDS("outputs/rmse_years_ebs.rds")
rmse_ai <- readRDS("outputs/rmse_years_ai.rds")

#Combine
rmse_combined <-bind_rows(rmse_cc, rmse_bc, rmse_goa, rmse_ebs, rmse_ai)
saveRDS(rmse_combined, file="outputs/rmse_combined.rds")

#Re-order
rmse_combined$region <- factor(rmse_combined$region, levels=c("cc", "bc", "goa", "ebs", "ai"))

###Plot for proto-draft--
ggplot(rmse_combined, aes(x=n_train, y=spatiotemporal_covariates))+
  geom_boxplot(aes(colour=region))+
  geom_point(aes(colour=region))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))+
  theme(legend.title = element_blank())+
  xlab("Number of Observations in Training Data")+
  ylab("RMSE of Spatio-temporal Model")+
  theme(legend.position=c(0.8,0.7))+
  scale_colour_discrete(labels=c("California Current", "British Columbia", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands"))

ggsave(
  paste("outputs/plots/rmse_vs_ntrain.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##Calculate spatial area of each survey extent to calculate density
#Load survey grids
load("data/fish_raw/nwfsc_grid.rda")
load("data/fish_raw/afsc_grid.rda")
load("data/fish_raw/dfo_synoptic_grid.rda")
#Sum area
cc_area <- sum(nwfsc_grid$area)
bc_area <- sum(dfo_synoptic_grid$area)
goa_grid <- subset(afsc_grid, afsc_grid$survey=="Gulf of Alaska Bottom Trawl Survey")
ebs_grid <- subset(afsc_grid, afsc_grid$survey=="Eastern Bering Sea Slope Bottom Trawl Survey")
ai_grid <- subset(afsc_grid, afsc_grid$survey=="Aleutian Islands Bottom Trawl Survey")
goa_area <- sum(nwfsc_grid$area)
goa_area <- sum(goa_grid$area)
ebs_area <- sum(ebs_grid$area)
ai_area <- sum(ai_grid$area)
#Add to RMSE
rmse_combined$area <- case_when(rmse_combined$region=="cc"~cc_area,
                                rmse_combined$region=="bc"~bc_area,
                                rmse_combined$region=="goa"~goa_area,
                                rmse_combined$region=="ebs"~ebs_area,
                                rmse_combined$region=="ai"~ai_area)
#Calculate
rmse_combined$n_train_density <- rmse_combined$n_train/rmse_combined$area

##Plot
#Make times 100 to be per kilometer (convert from hectare)
ggplot(rmse_combined, aes(x=(n_train_density*100), y=spatiotemporal_covariates))+
  geom_boxplot(aes(colour=region))+
  geom_point(aes(colour=region))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))+
  theme(legend.title = element_blank())+
  labs(x = bquote('Spatial Density of Observations in Training Data '(N~km^-2)), y = bquote("RMSE of FishOx Predictions"))+
  theme(legend.position=c(0.8,0.7))+
  scale_colour_discrete(labels=c("California Current", "British Columbia", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands"))

ggsave(
  paste("outputs/plots/rmse_vs_ntrain_density.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

###With Glorys data###
##Total RMSE per region GlORYS model
glorys_rmse_cc <- readRDS("outputs/glorys_summary_cc.rds")
glorys_rmse_cc$region <- "cc"
glorys_rmse_bc <- readRDS("outputs/glorys_summary_bc.rds")
glorys_rmse_bc$region <- "bc"
glorys_rmse_goa <- readRDS("outputs/glorys_rmse_goa.rds")
glorys_rmse_goa$region <- "goa"
glorys_rmse_ebs <- readRDS("outputs/glorys_summary_ebs.rds")
glorys_rmse_ebs$region <- "ebs"
glorys_rmse_ai <- readRDS("outputs/glorys_summary_ai.rds")
glorys_rmse_ai$region <- "ai"

glorys_rmsetotal_cc <- as.data.frame(calc_rmse(glorys_rmse_cc$glorys, glorys_rmse_cc$n_test))
glorys_rmsetotal_bc <- as.data.frame(calc_rmse(glorys_rmse_bc$glorys, glorys_rmse_bc$n_test))
glorys_rmsetotal_goa <- as.data.frame(calc_rmse(glorys_rmse_goa$glorys, glorys_rmse_goa$n_test))
glorys_rmsetotal_ebs <- as.data.frame(calc_rmse(glorys_rmse_ebs$glorys, glorys_rmse_ebs$n_test))
glorys_rmsetotal_ai <- as.data.frame(calc_rmse(glorys_rmse_ai$glorys, glorys_rmse_ai$n_test))

#Add to rmse_totals table
rmse_totals[nrow(rmse_totals) + 1,] = c("glorys", glorys_rmsetotal_cc, glorys_rmsetotal_bc, glorys_rmsetotal_goa, glorys_rmsetotal_ebs, glorys_rmsetotal_ai)

#Total RMSE per region GLORYS matching
glorys_rmsetotal_cc <- readRDS("outputs/glorys_rmsetotal_matching_cc.rds")
glorys_rmsetotal_bc <- readRDS("outputs/glorys_rmsetotal_matching_bc.rds")
glorys_rmsetotal_goa <- readRDS("outputs/glorys_rmsetotal_matching_goa.rds")
glorys_rmsetotal_ebs <- readRDS("outputs/glorys_rmsetotal_matching_ebs.rds")
glorys_rmsetotal_ai <- readRDS("outputs/glorys_rmsetotal_matching_ai.rds")

rmse_totals[nrow(rmse_totals) + 1,] = c("glorys_matching", glorys_rmsetotal_cc, glorys_rmsetotal_bc, glorys_rmsetotal_goa, glorys_rmsetotal_ebs, glorys_rmsetotal_ai)

saveRDS(rmse_totals, file="outputs/rmse_glorys_total_combined.rds")

##RMSE per year
glorys_rmse_cc <- readRDS("outputs/glorys_summary_cc.rds")
glorys_rmse_cc$region <- "cc"
glorys_rmse_bc <- readRDS("outputs/glorys_summary_bc.rds")
glorys_rmse_bc$region <- "bc"
glorys_rmse_goa <- readRDS("outputs/glorys_rmse_goa.rds")
glorys_rmse_goa$region <- "goa"
glorys_rmse_ebs <- readRDS("outputs/glorys_summary_ebs.rds")
glorys_rmse_ebs$region <- "ebs"
glorys_rmse_ai <- readRDS("outputs/glorys_summary_ai.rds")
glorys_rmse_ai$region <- "ai"

#Combined
rmse_glorys_combined <- bind_rows(glorys_rmse_cc, glorys_rmse_bc, glorys_rmse_goa, glorys_rmse_ebs, glorys_rmse_ai)
rmse_glorys_combined <- select(rmse_glorys_combined, region, year, glorys)

#GLORYS matching RMSE by year
glorys_rmse_cc <- readRDS("outputs/glorys_rmse_matching_cc.rds")
glorys_rmse_cc$region <- "cc"
glorys_rmse_bc <- readRDS("outputs/glorys_rmse_matching_bc.rds")
glorys_rmse_bc$region <- "bc"
glorys_rmse_goa <- readRDS("outputs/glorys_rmse_matching_goa.rds")
glorys_rmse_goa$region <- "goa"
glorys_rmse_ebs <- readRDS("outputs/glorys_rmse_matching_ebs.rds")
glorys_rmse_ebs$region <- "ebs"
glorys_rmse_ai <- readRDS("outputs/glorys_rmse_matching_ai.rds")
glorys_rmse_ai$region <- "ai"

rmse_glorys_combined_matching <- bind_rows(glorys_rmse_cc, glorys_rmse_bc, glorys_rmse_goa, glorys_rmse_ebs, glorys_rmse_ai)
colnames(rmse_glorys_combined_matching)[1] <- "glorys_matching"
rmse_glorys_combined_matching <- select(rmse_glorys_combined_matching, region, year, glorys_matching)

#Combine with model ones
rmse_combined2 <- left_join(rmse_combined, rmse_glorys_combined,by=c("region","year"))
rmse_combined2 <- left_join(rmse_combined2, rmse_glorys_combined_matching,by=c("region","year"))

#Convert to nice table and csv for supplement
rmse_combined2 <- select(rmse_combined2, region, year, n_train, n_test, null_null, null_temponly, null_covariates, persistent, persistent_temponly, persistent_covariates,persistent_annual, annual_temponly, annual_covariates,persistent_spatiotemporal, spatiotemporal_temp, spatiotemporal_covariates, glorys_matching, glorys)
rmse_combined2$region <- case_when(rmse_combined2$region=="cc"~"California Current",
                                  rmse_combined2$region=="bc"~"British Columbia",
                                  rmse_combined2$region=="goa"~"Gulf of Alaska",
                                  rmse_combined2$region=="ebs"~"Eastern Bering Sea",
                                  rmse_combined2$region=="ai"~"Aleutian Islands")
rmse_combined2[,c(5:18)] <- round(rmse_combined2[,c(5:18)], digits=1)
write.csv(rmse_combined2, file="outputs/rmse_glorys_combined_table.csv", row.names=F)

#Convert long
rmse_combined2 <- pivot_longer(rmse_combined2, c(5:17), names_to="model")
labs <- c("Aleutian Islands", "British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska")
names(labs) <- c("ai", "bc", "cc", "ebs", "goa")
rmse_combined2$region <- factor(rmse_combined2$region, levels=c("California Current", "British Columbia", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands"))

#### Plot for paper ####
#Add columns for random and covariates
rmse_combined2$random <- case_when(rmse_combined2$model=="null_null"|rmse_combined2$model=="null_covariates"|rmse_combined2$model=="null_temponly"~"none",
                                   rmse_combined2$model=="annual_temponly"|rmse_combined2$model=="annual_covariates"|rmse_combined2$model=="persistent_annual"~"annual",
                                   rmse_combined2$model=="persistent"|rmse_combined2$model=="persistent_covariates"|rmse_combined2$model=="persistent_temponly"~"spatial",
                                   rmse_combined2$model=="persistent_spatiotemporal"|rmse_combined2$model=="spatiotemporal_covariates"|rmse_combined2$model=="spatiotemporal_temp"~"spatiotemporal")

rmse_combined2$covariates <- case_when(rmse_combined2$model=="null_null"|rmse_combined2$model=="persistent"|rmse_combined2$model=="persistent_spatiotemporal"|rmse_combined2$model=="persistent_annual"~"none",
                                   rmse_combined2$model=="annual_temponly"|rmse_combined2$model=="persistent_temponly"|rmse_combined2$model=="null_temponly"|rmse_combined2$model=="spatiotemporal_temp"~"temp",
                                   rmse_combined2$model=="null_covariates"|rmse_combined2$model=="persistent_covariates"|rmse_combined2$model=="spatiotemporal_covariates"|rmse_combined2$model=="annual_covariates"~"temp+sal")
#Order categories
rmse_combined2$random <- factor(rmse_combined2$random, levels=c("none", "spatial", "annual", "spatiotemporal"))
rmse_combined2$covariates <- factor(rmse_combined2$covariates, levels=c("none", "temp", "temp+sal"))

#Remove GLORYS
rmse_plot <- subset(rmse_combined2, model!="glorys")
rmse_plot <- subset(rmse_plot, model!="glorys_matching")
glorys <- subset(rmse_combined2, model=="glorys"|model=="glorys_matching")

ggplot(rmse_plot, aes(x=year, y=value))+
  geom_line(aes(colour=random, linetype=covariates), size=1.1)+
  facet_wrap("region", labeller = labeller(region=labs), scales="free_y")+
  ylab("RMSE")+
  xlab("Year")+
  theme(legend.position="top")+
  theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme_minimal(base_size=15) +
  #scale_x_continuous(breaks=c(2008,2012,2016,2020,2024))+
  theme(legend.position=c(0.85,0.2))+
  labs(colour="Spatial/ \nTemporal Structure", linetype="Covariates")+
  scale_linetype_manual(values=c("dotted", "twodash", "solid"))

ggsave(
  paste("outputs/plots/rmse_years.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 11,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##GLORYS
ggplot(filter(rmse_combined2, model=="glorys"|model=="glorys_matching"|model=="spatiotemporal_covariates"), aes(x=year, y=value))+
  geom_line(aes(color=model))+
  facet_wrap("region", scales="free_y", labeller = labeller(region=labs))+
  ylab("RMSE")+
  xlab("Year")+
  theme(legend.position="top")+
  theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=c(2008,2012,2016,2020,2024))+
  theme_minimal(base_size=20) +
  theme(legend.title=element_blank())+
  xlab("Year")+
  ylab("RMSE")+
  theme(legend.position=c(0.82,0.2))+
 scale_color_discrete(labels=c("GOBH interpolation", "GOBH nearest-neighbor", "In situ spatio-temporal"))

ggsave(
  paste("outputs/plots/rmse_glorys_st.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9.5,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##Add GLORYS back
ggplot(rmse_plot, aes(x=year, y=value))+
  geom_line(aes(colour=random, linetype=covariates), size=1.1)+
  facet_wrap("region", labeller = labeller(region=labs), scales="free_y")+
  geom_ribbon(glorys, mapping=aes(x=year, ymin=value,ymax=value, fill=model, colour=model))+
  ylab("RMSE")+
  xlab("Year")+
  theme(legend.position="top")+
  theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_minimal(base_size=15) +
  theme(legend.text=element_text(size=11))+
  #scale_x_continuous(breaks=c(2008,2012,2016,2020,2024))+
  theme(legend.position=c(0.85,0.15))+
  labs(colour="Spatial/ \nTemporal Structure", linetype="Covariates")+
  scale_linetype_manual(values=c("dotted", "twodash", "solid"))+
  theme(legend.spacing.y = unit(0.01, 'cm')) +
  scale_fill_manual(name="Oceanographic Ouptut", labels=c("nearest neighbor", "interpolation"), values=c("#FF61CC", "#00A9FF"), )

ggsave(
  paste("outputs/plots/rmse_glorys_all_annual.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9.5,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

###Combine residuals for *best-fitting* model across regions
##read in model output files
load("outputs/o2_models_cc.Rdata")
cc <- output
load("outputs/o2_models_bc.Rdata")
bc <- output
load("outputs/o2_models_goa.Rdata")
goa <- output
load("outputs/o2_models_ebs.Rdata")
ebs <- output
load("outputs/o2_models_ai.Rdata")
ai <- output

#Isolate the predictions for the *best-fitting* model for each year
glorys <- F
#identify best-fitting model by region
best <- list()
slice1 <- rmse_totals %>% slice_min(cc)
best[1] <- slice1[1,1]
slice1 <- rmse_totals %>% slice_min(bc)
best[2] <- slice1[1,1]
slice1 <- rmse_totals %>% slice_min(goa)
best[3]<- slice1[1,1]
slice1 <- rmse_totals %>% slice_min(ebs)
best[4] <- slice1[1,1]
slice1 <- rmse_totals %>% slice_min(ai)
best[5] <- slice1[1,1]
best <- unlist(best)

models <- c("persistent", "persistent_annual", "persistent_spatiotemporal", "null_temponly", "null_covariates", "persistent_temponly", "persistent_covariates", "annual_temponly", "annual_covariates", "spatiotemporal_temp","spatiotemporal_covariates", "null_null")

#Label predictions with the model names
relabel <- function(x){
  names(x[["predictions"]]) <- models
  return(x)
}
cc <- lapply(cc, relabel)
bc <- lapply(bc, relabel)
goa <- lapply(goa, relabel)
ebs <- lapply(ebs, relabel)
ai <- lapply(ai, relabel)

#Could also give this a list of other models
ai <- combine_preds(ai, glorys, best[1])
bc <- combine_preds(bc, glorys, best[2])
cc <- combine_preds(cc, glorys, best[3])
ebs <- combine_preds(ebs, glorys,best[4])
goa <- combine_preds(goa, glorys, best[5])

combined_preds <- bind_rows(cc, bc, goa, ebs, ai)
saveRDS(combined_preds, file="outputs/combined_preds.rds")

##How many are above or below 50?
test <- subset(combined_preds, residual>50|residual< -50)
count <- test %>% count(region)
#Proportion within 50
1-(nrow(test)/nrow(combined_preds))

##How many are above or below 25?
test <- subset(combined_preds, residual>25|residual< -25)
count <- test %>% count(region)
#Proportion within 25
1-(nrow(test)/nrow(combined_preds))

#What about if switch any negative to 0?
test <- subset(combined_preds, residual2>50|residual2< -50)
count <- test %>% count(region)
#Proportion within 50
1-(nrow(test)/nrow(combined_preds))

##How many are above or below 25?
test <- subset(combined_preds, residual2>25|residual2< -25)
count <- test %>% count(region)
#Proportion within 25
1-(nrow(test)/nrow(combined_preds))

#Reorder regions
combined_preds$region <- factor(combined_preds$region, levels=c("cc", "bc", "goa", "ebs", "ai"))
labs <- c("British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska", "Aleutian Islands")
names(labs) <- c("bc", "cc", "ebs", "goa", "ai")

##Plot density plots
ggplot(combined_preds, aes(x=residual))+
  geom_density(aes(colour=region))+
  scale_colour_discrete(labels=c("California Current", "British Columbia", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands"))+
  xlab("Prediction Residual")+
  ylab("Density")+
  xlim(-100,100)+
  scale_y_continuous(expand=c(0,0))+
theme_minimal()+
  theme(text=element_text(size=20))+
  theme(legend.position=c(0.8,0.8))+
  theme(legend.title=element_blank())

ggsave(
  paste("outputs/plots/residual_density.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

#1:1 obs: preds lines--Make each region a separate plot
ggplot(combined_preds, aes(x=o2,y=est))+
  geom_point(colour="grey")+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(expand = c(0, 0), limits=c(0,500)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,500))+
  theme_minimal()+
  theme(panel.spacing = unit(2, "lines"))+
  theme(text=element_text(size=18))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.3))+
  scale_color_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys")

ggsave(
  paste("outputs/plots/preds_vs_obs.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height =6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##Calculate variance for calculating normalized RMSE
sd <- combined_preds %>% group_by(region)%>%
  summarise(sd_var1 = sd(residual, na.rm=TRUE))%>%
  ungroup()


###GLORYS data
load("outputs/o2_models_glorys/cc.Rdata")
cc <- output
load("outputs/o2_models_glorys/bc.Rdata")
bc <- output
load("outputs/o2_models_glorys/goa.Rdata")
goa <- output
load("outputs/o2_models_glorys/ebs.Rdata")
ebs <- output
load("outputs/o2_models_glorys/ai.Rdata")
ai <- output

glorys <- T
ai <- combine_preds(ai, glorys, "ai")
bc <- combine_preds(bc, glorys, "bc")
cc <- combine_preds(cc, glorys, "bc")
ebs <- combine_preds(ebs, glorys, "ebs")
goa <- combine_preds(goa, glorys, "goa")

combined_preds_glorys <- bind_rows(cc, bc, goa, ebs, ai)
saveRDS(combined_preds_glorys, file="outputs/combined_preds_glorys.rds")

ggplot(combined_preds_glorys, aes(x=residual))+
  geom_density(aes(colour=region))+
  scale_colour_discrete(labels=c("California Current", "British Columbia", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands"))+
  xlab("Prediction Residual")+
  ylab("Density")+
  xlim(-100,100)+
  theme_bw(base_size=20) +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.8))

#Make each region a separate plot
ggplot(combined_preds, aes(x=o2,y=est))+
  geom_point(aes(colour=year), alpha=0.9)+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  theme_bw(base_size=20) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,NA))+
  theme_minimal()+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.3))+
  scale_color_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys")

###Isolate just one year in one region from each
test <- filter(combined_preds, region=="cc"&year==2012)
test$type <- "insitu observations spatio-temporal model"
test2 <- filter(combined_preds_glorys, region=="cc"&year==2012)
test2$type <- "Global Ocean Biogeochemistry Hindcast"

test <- bind_rows(test, test2)

test$type <- factor(test$type, levels=c("insitu observations spatio-temporal model", "Global Ocean Biogeochemistry Hindcast"))

p1 <- ggplot(test, aes(x=o2,y=est))+
  geom_point(aes(colour=type))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  theme_minimal(base_size=15) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,NA))+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.3,0.8))+
  scale_colour_manual(values=c("black", "grey50"))

p2 <- ggplot(test, aes(x=residual))+
  geom_density(aes(colour=type))+
  xlab("Prediction Residual")+
  ylab("Density")+
  theme_minimal(base_size=15) +
  scale_y_continuous(expand=c(0,0))+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=13))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.8))+
  scale_colour_manual(values=c("black", "grey50"))

figure <- ggarrange(p1, p2, labels=c("A", "B"),
                    ncol = 2, nrow = 1, common.legend=T, legend="bottom")

ggsave(
  paste("outputs/plots/example_gobh_insitu.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)


#Plot all residuals on a map per year per region, by color
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)
map_resids <- function(dat, us_coast_proj, region_title){
  test_region <- unique(dat$region)
  if(test_region=="ebs"){
    xlims <- c(-3000*1000, -1500*1000)
    ylims <- c(6500*1000, 8000*1000)
    lons <- c(-170, -163, -155)
    lats <- c(47,51,55)
  }
  if(test_region=="goa"){
    xlims <- c(min(dat$X)*1000, max(dat$X)*1000)
    ylims <- c(min(dat$Y)*1000, max(dat$Y)*1000)
    a <- round(min(dat$longitude))+6
    b <- round(max(dat$longitude))
    lons <- c(a, -149, b)
    lats <- c(47, 50,52)
  }

  if(test_region=="cc"){
    xlims <- c(min(dat$X)*1000, max(dat$X)*1000)
    ylims <- c(min(dat$Y)*1000, max(dat$Y)*1000)
    lons <- c(round(min(dat$longitude)+2), round(max(dat$longitude)))
    lats <- c(35, 40, 45)
  }
  if(test_region=="bc"){
    xlims <- c(min(dat$X)*1000, max(dat$X)*1000)
    ylims <- c(min(dat$Y)*1000, max(dat$Y)*1000)
    lons <- c(round(min(dat$longitude)+2), round(max(dat$longitude)))
    lats <- c(48, 51, 54)
  }
  if(test_region=="ai"){
    dat <- filter(dat, longitude<0)
    xlims <- c(min(dat$X)*1000, max(dat$X)*1000)
    ylims <- c(min(dat$Y)*1000, max(dat$Y)*1000)
    lons <- c(round(min(dat$longitude)+5), -170, round(max(dat$longitude)))
    lats <- c(49,50,51)
  }
  ggplot(us_coast_proj) + geom_sf() +
    geom_point(dat, ,mapping=aes(x=X*1000,y=Y*1000, colour=residual), size=1)+
    #  geom_point(dat ,mapping=aes(x=X*1000,y=Y*1000, colour=residual), alpha=0.4)+
     facet_wrap("year")+
    scale_x_continuous(breaks=lons, limits=xlims)+
    scale_y_continuous(breaks=lats, limits=ylims)+
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(text=element_text(size=20))+
    ggtitle(paste(region_title))+
    theme(legend.position="right")+
    colorspace::scale_color_continuous_diverging(
                           limits = c(-50, 50),
                           alpha=0.5,
                           oob = scales::squish,
                           name = bquote(Residual),
                           breaks = c(-50, 0, 50)
    )
  ggsave(
    paste("outputs/plots/residuals_", region_title,".png", sep=""),
    plot = last_plot(),
    device = NULL,
    path = NULL,
    scale = 1,
    width = 9,
    height = 11,
    units = c("in"),
    dpi = 600,
    limitsize = TRUE, bg="white"
  )
}

map_resids(filter(combined_preds, region=="cc"), us_coast_proj, region_title="California Current")
map_resids(filter(combined_preds, region=="bc"), us_coast_proj,region_title="British Columbia")
map_resids(filter(combined_preds, region=="goa"), us_coast_proj, "Gulf of Alaska")
map_resids(filter(combined_preds, region=="ebs"), us_coast_proj, "Eastern Bering Sea")
map_resids(filter(combined_preds, region=="ai"), us_coast_proj, "Aleutian Islands")


#Residual plot for paper: by depth
ggplot(combined_preds, aes(x=o2,y=-depth))+
  geom_point(aes(colour=residual), alpha=0.4)+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Depth (m)")+
  theme_bw(base_size=20) +
  # scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) +
  #scale_y_continuous(expand = c(0, 0), limits=c(0,NA))+
  theme_minimal()+
  theme(text=element_text(size=20))+
  theme(text=element_text(size=18))+
  theme(legend.position=c(0.8,0.3))+
  colorspace::scale_color_continuous_diverging(
    limits = c(-50, 50),
    alpha=0.5,
    oob = scales::squish,
    name = bquote(Residual),
    breaks = c(-50, 0, 50)
  )

ggsave(
  paste("outputs/plots/depth_o2_resids.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

#Residuals by Oxygen prediction and different other variables
ggplot(combined_preds, aes(x=est,y=residual))+
  geom_point(aes(colour=doy), alpha=0.4)+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Oxygen Prediction")+
  ylab("Residual")+
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) +
  scale_y_continuous(expand = c(0, 0), limits=c(-50,50))+
  theme_minimal(base_size=20)+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.position=c(0.8,0.3))+
  scale_colour_viridis()

ggsave(
  paste("outputs/plots/resids_o2_doy.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

ggplot(combined_preds, aes(x=est,y=residual))+
  geom_point(aes(colour=sigma0), alpha=0.4)+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Oxygen Prediction")+
  ylab("Residual")+
  theme_bw(base_size=20) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) +
  scale_y_continuous(expand = c(0, 0), limits=c(-50,50))+
  theme_minimal(base_size=20)+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.position=c(0.8,0.3))+
  scale_colour_viridis()

ggsave(
  paste("outputs/plots/resids_o2_sal.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

ggplot(combined_preds, aes(x=est,y=residual))+
  geom_point(aes(colour=temp), alpha=0.4)+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Oxygen Prediction")+
  ylab("Residual")+
  theme_minimal(base_size=20)+
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) +
  scale_y_continuous(expand = c(0, 0), limits=c(-50,50))+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.position=c(0.8,0.3))+
  scale_colour_viridis()

ggsave(
  paste("outputs/plots/resids_o2_temp.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

ggplot(combined_preds, aes(x=est,y=residual))+
  geom_point(aes(colour=year), alpha=0.4)+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Oxygen Prediction")+
  ylab("Residual")+
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) +
  scale_y_continuous(expand = c(0, 0), limits=c(-50,50))+
  theme_minimal(base_size=20)+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.position=c(0.8,0.3))+
  scale_colour_viridis()

ggsave(
  paste("outputs/plots/resids_o2_year.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)


###Other residual patterns####
ggplot(combined_preds, aes(x=o2,y=est))+
  geom_point(aes(colour=year))+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(expand = c(0, 0), limits=c(0,500)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,500))+
  theme_minimal()+
  theme(text=element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.3))+
  scale_color_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys")

ggsave(
  paste("outputs/plots/obs_preds_year.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

ggplot(combined_preds, aes(x=o2,y=est))+
  geom_point(aes(colour=temp))+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(expand = c(0, 0), limits=c(0,500)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,500))+
  theme_minimal()+
  theme(text=element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.3))+
  scale_color_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys")

ggsave(
  paste("outputs/plots/obs_preds_temp.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

ggplot(combined_preds, aes(x=o2,y=est))+
  geom_point(aes(colour=sigma0))+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(expand = c(0, 0), limits=c(0,500)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,500))+
  theme_minimal()+
  theme(text=element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.3))+
  scale_color_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys")

ggsave(
  paste("outputs/plots/obs_preds_sigma0.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

ggplot(combined_preds, aes(x=o2,y=est))+
  geom_point(aes(colour=doy))+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(expand = c(0, 0), limits=c(0,500)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,500))+
  theme_minimal()+
  theme(text=element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.3))+
  scale_color_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys")

ggsave(
  paste("outputs/plots/obs_preds_doy.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

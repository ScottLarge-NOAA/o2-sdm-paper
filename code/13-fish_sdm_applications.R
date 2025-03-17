remove.packages("sdmTMB")
remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE,  ref="newbreakpt")
library(sdmTMB)

library(dplyr)
library(tidyr)
library(tidync)
library(ggplot2)
library(ggpubr)

#Set wd
setwd("~/Dropbox/GitHub/o2-sdm")

#Load functions
source("code/helper_funs.R")
source("code/util_funs.R")

#Load regional polygons
regions.hull <- readRDS("data/processed_data/regions_hull.rds")

#Pull the previously run output
use_previous <- T
if(use_previous) {
  load("outputs/sablefish_application.RData")
  load(file="outputs/dsole_application.RData")
}

#Fit for each region and species of interest
#If True, GOBH will re-pull the data, fit the model, and predict the GOBH values. If False, will use the already predicted values (Warning it will take a long time if T)
GOBH <- F

#Remove CPUE outliers?
remove_outlier <- T

#Filter to the middle size classes?
filter_size <- F

#Include random effects in predictions?
rf <- T

#Use new breakpoint function from the thresholds paper?
new_breakpt <- T

#Filter predicted and GOBH to only the years/points with synoptic data?
filter_years <- F

#For predicted data, only use to fill in missing data, and use the synoptic data for the rest?
combine_pred <- T

#This function will pull the correct species data, fit the integrated O2 model and predict those values, create three separate data sets of
#synoptic only, the integrated O2 model predictions, and the GOBH predictions, then fit each set of models to each data set.
#Returned object: named list of:
#the data, the model fits, the AIC table, dataframe of all parameter estimates (all data types and models combined into one dataframe, in long format w/ columns for data type and model type), and the dataframes with predictions (for each data type, and for each model)
sablefish_cc <- model_comparison("sablefish", "cc", gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred)
sablefish_bc <- model_comparison("sablefish", "bc", gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred)

dsole_cc <- model_comparison("dsole", "cc", gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred)
dsole_bc <- model_comparison("dsole", "bc", gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred)

save <- T
if(save){
save(sablefish_cc, sablefish_bc, file="outputs/sablefish_application.RData")
save(dsole_cc, dsole_bc, file="outputs/dsole_application.RData")

}

#Look at parameter estimates
par1 <- par_estimates(sablefish_cc, "cc", "sablefish")
par2 <- par_estimates(sablefish_bc, "bc", "sablefish")
par3 <- par_estimates(dsole_cc, "cc", "dover sole")
par4 <- par_estimates(dsole_bc, "bc", "dover sole")
pars <- bind_rows(par1, par2, par3, par4)

###Plot marginal effect of O2 component of model:
#Combined into one plot
outputs <- list(sablefish_cc, sablefish_bc, dsole_cc, dsole_bc)
region <- c("California Current", "British Columbia", "California Current", "British Columbia")
species <- c("Sablefish", "Sablefish", "Dover sole", "Dover sole")
plot1 <- plot_marginal3(outputs, "breakpt(o2)")
print(plot1)
ggsave(
  paste("outputs/plots/sdm_comparison_marginal_effect.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 8.5,
  units = c("in"),
  bg="white",
  dpi = 600,
  limitsize = TRUE
)

###Plot oxygen data for the three types and different sources
a <- plot_dat(sablefish_cc, legend=F)
#a <- annotate_figure(a, "Sablefish, California Current")

b <- plot_dat(sablefish_bc, legend=F)
#b <- annotate_figure(b, "Sablefish, British Columbia")

c <- plot_dat(dsole_cc, legend=F)
#c <- annotate_figure(c, "Dover sole, California Current")

d <- plot_dat(dsole_bc, legend=T)
#d <- annotate_figure(d, "Dover sole, British Columbia")

fig <- ggarrange(a,c, b, d, common.legend=T, labels=c("Sablefish, California Current", "Sablefish, British Columbia", "Dover Sole, California Current", "Dover sole, British Columbia"), vjust=0.3, hjust=-0.3)
annotate_figure(fig, bottom=text_grob('Dissolved Bottom Oxygen ('~mu~"mol"~kg^-1~")"), left="Depth (m)",fig.lab.size=20)
print(fig)

ggsave(
  paste("outputs/plots/o2_sdm_data_comparison.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 11,
  units = c("in"),
  bg="white",
  dpi = 600,
  limitsize = TRUE
)


###Table of breakpoint parameter estimates
a <- par_table(sablefish_cc, "sablefish", "cc")
a$region <- "California Current"
a$species <- "Sablefish"
b <- par_table(sablefish_bc, "sablefish", "bc")
b$region <- "British Columbia"
b$species <- "Sablefish"
c <- par_table(dsole_cc, "dsole", "cc")
c$region <- "California Current"
c$species <- "Dover sole"
d <- par_table(dsole_bc, "dsole", "bc")
d$region <- "British Columbia"
d$species <- "Dover sole"

pars <- bind_rows(a,b,c,d)

write.csv(pars, file="outputs/pars_estimates_breakpoint.csv")

##Test whether it is the greater expansion of years that matters:
filter_years <- T
combine_preds <- F
sablefish_cc <- model_comparison("sablefish", "cc", gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred)
sablefish_bc <- model_comparison("sablefish", "bc", gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred)

dsole_cc <- model_comparison("dsole", "cc", gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred)
dsole_bc <- model_comparison("dsole", "bc", gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred)


##Plot marginal effect of O2 component of model:
#Combined into one plot
outputs <- list(sablefish_cc, sablefish_bc, dsole_cc, dsole_bc)
region <- c("California Current", "British Columbia", "California Current", "British Columbia")
species <- c("Sablefish", "Sablefish", "Dover sole", "Dover sole")
plot1 <- plot_marginal3(outputs, "breakpt(o2)")
print(plot1)
ggsave(
  paste("outputs/plots/sdm_comparison_marginal_effect_filter_years.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 8.5,
  units = c("in"),
  bg="white",
  dpi = 600,
  limitsize = TRUE
)

###Plot marignal effects manually
###Plot marginal effect of O2 component of model:
#Combined into one plot
#Run predictions for all
output_preds1 <- brkptfun(sablefish_cc, "California Current", "Sablefish")
output_preds2 <- brkptfun(dsole_cc, "California Current", "Dover sole")
output_preds3 <- brkptfun(sablefish_bc, "British Columbia", "Sablefish")
output_preds4 <- brkptfun(dsole_bc, "British Columbia", "Dover sole")

preds <- bind_rows(output_preds1, output_preds2, output_preds3, output_preds4)

ggplot(preds, aes(o2, y=exp(est)))+
  facet_wrap(region~species, scales="free_y")+
  geom_line(aes(colour=data))+
  geom_ribbon(aes(ymin = exp(est_low), ymax = exp(est_high), fill=data), alpha=0.4)+
  # scale_y_continuous(limits=c(0,50), expand = expansion(mult = c(0, 0.0))) +
  labs(x = bquote('Oxygen'~mu~"mol"~kg^-1), y = bquote('Population Density'~(kg~km^-2)))+
  theme_minimal()+
  theme(legend.position="top")+
  theme(text=element_text(size=15))

ggsave(
  paste("outputs/plots/sdm_comparison_marginal_effect_filter_years.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 8.5,
  units = c("in"),
  bg="white",
  dpi = 600,
  limitsize = TRUE
)

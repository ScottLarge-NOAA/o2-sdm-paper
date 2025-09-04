# ------------------------------------------------------------------------------
# 05-mom6_fit_model.R
#
# Adapted R script (i.e., 08-glorys_fit_models.R)
# from Julia Indivero's oxygen skill testing repo to skill test MOM6 oxygen 
# outputs against the Joint U.S.-Canada Pacific Hake Acoustic Trawl Survey in 
# situ data
# https://github.com/jindivero/o2-sdm-paper#
#
# Step 5: Fit GLMM to MOM6 output and calculate RMSE against hake survey data
#
# Created by Julia Indivero
# Adapted: Aug 14th, 2025 by Olivia Gemmell, UW
#
# ------------------------------------------------------------------------------

library(sdmTMB)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(rnaturalearth)
library(sf)
library(ggpubr)
library(visreg)
library(tidync)
library(marmap)
library(bestNormalize)
library(readr)

# set root directory
root_dir <- "test_cases/mom6/"

#Load functions
# source("bin/skill_test_mom6/R/util_funs.R")
source(paste0(root_dir, "R/helper_funs.R"))


# load regional polygons
regions.hull <- readRDS(paste0(root_dir, "data/regions_hull.rds"))

# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)

#Load oxygen data
dat <- readRDS(paste0(root_dir, "data/all_o2_dat_region.rds"))

#Make depth 0 NA
dat$depth <- ifelse(dat$depth==0, NA, dat$depth)

#Remove any rows with missing data
dat <- dat %>%
  drop_na(depth, o2, temp, sigma0, month, X, Y, year)

#Set minimum sigma
minsigma0 <- 24
dat$sigma0[dat$sigma0 <= minsigma0] <- minsigma0

#Log depth
dat$depth_ln <- log(dat$depth)

#Save model outputs?
savemodel=T
#Plot models and save?
plotmodel = T


#Scale?
scale <- F
if(scale==T){
  dat$o2 <- dat$o2/100
}


#Run model fit
rmse_bc_cc <- mom6_fit(dat = dat, 
                       test_region = "bc_cc", 
                       root_dir = root_dir,
                       scale = FALSE,
                       quantile_transform = TRUE)

# ------------------------------------------------------------------------------
# look at results
# ------------------------------------------------------------------------------
# Load region data
preds_all <- read_rds(file = paste0(root_dir, "data/mom6/mom6_bc_cc.rds"))
preds_all <- preds_all[preds_all$depth <= 500, ]
preds_all <- preds_all[preds_all$o2 >= 0, ]
preds_all$depth_ln <- log(preds_all$depth)
# Apply ordered quantile normalization to o2 column
bn <- orderNorm(preds_all$o2)

rmse_bc_cc$rmse_summary

### 2011
# Calculate dharma residuals 
mod <- rmse_bc_cc$models$m_2011
sim_resids <- simulate(mod, nsim = 500, type = "mle-mvn")
dharma_residuals(sim_resids, mod)

# Get fitted values from your model
res_df <- data.frame(
  fitted = predict(mod)$est,
  residuals = residuals(mod, type = "pearson")
)
# Create the fitted vs residuals plot
ggplot(res_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  theme_minimal() +
  labs(
    x = "Fitted Values",
    y = "Pearson Residuals",
    title = "Fitted vs Residuals Plot"
  )

# print out the figure 
rmse_bc_cc$figures$fig_2011

# response curve
# generate response curves by hand
# generate a dataframe of all potential values setting other covariates to 
# a fixed value
preds <- preds_all[preds_all$year == 2011, ]
d_pred <- rmse_bc_cc$predictions$preds_2011
nd <- data.frame(depth_ln = seq(min(preds$depth_ln), 
                                max(preds$depth_ln), 
                                length.out = 250),
                 month = as.factor(2)) # pick any factor level
# predict to the new data
p <- predict(mod,
             newdata = nd,
             se_fit = TRUE, # adds confidence intervals
             re_form = ~ 0 # don't include spatial random fields
)
# reverse the quantile transformation
p$est <- predict(bn, newdata = p$est, inverse = TRUE)
p$est_sd <- predict(bn, newdata = p$est_sd, inverse = TRUE)

# plot
plot_resp_curve(p = p,
                p_pred = depth_ln, 
                d_pred = d_pred$depth,
                xlab = "Depth")




### 2012
# Calculate dharma residuals 
mod <- rmse_bc_cc$models$m_2012
sim_resids <- simulate(mod, nsim = 500, type = "mle-mvn")
dharma_residuals(sim_resids, mod)
plot(sim_resids)
# print out the figure 
rmse_bc_cc$figures$fig_2012


### 2013
# Calculate dharma residuals 
mod <- rmse_bc_cc$models$m_2013
sim_resids <- simulate(mod, nsim = 500, type = "mle-mvn")
dharma_residuals(sim_resids, mod)
plot(sim_resids)
# print out the figure 
rmse_bc_cc$figures$fig_2013


### 2015
# Calculate dharma residuals 
mod <- rmse_bc_cc$models$m_2015
sim_resids <- simulate(mod, nsim = 500, type = "mle-mvn")
dharma_residuals(sim_resids, mod)
plot(sim_resids)
# print out the figure 
rmse_bc_cc$figures$fig_2015


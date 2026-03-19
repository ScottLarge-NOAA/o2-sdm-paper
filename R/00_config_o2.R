# ==============================================================================
# 00_config_o2.R
# Purpose: Master configuration for NEUS Dissolved Oxygen SDM Pipeline.
# ==============================================================================

library(dplyr)
library(tidyr)
library(here)
library(terra)
library(stringr)
library(sf)
library(readr)
library(rerddap)
library(lubridate)
library(gsw)
library(sdmTMB)
library(httr)

message("--- Loading Global DO Pipeline Configuration ---")

# --- 1. COORDINATE REFERENCE SYSTEMS & SPATIAL GLOBALS ------------------------
crs_projected  <- sf::st_crs(32619) # UTM Zone 19N (Meters)
crs_geographic <- sf::st_crs(4326)  # WGS84

grid_res_m <- 10000 
target_regions <- c("MAB", "GOM", "GB") 

# --- 2. OPERATIONAL DIRECTORIES -----------------------------------------------
raw_dir <- here::here("data", "raw-data")
der_dir <- here::here("data", "derived-data")
out_dir <- here::here("output", "o2_model")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(der_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- 3. SPATIOTEMPORAL MODELING PARAMETERS ------------------------------------
mesh_cutoff <- 25 # km distance for mesh knots (adjust based on data density)

operational_priors <- sdmTMB::sdmTMBpriors(
  matern_s = sdmTMB::pc_matern(
    range_gt = 5, range_prob = 0.05, 
    sigma_lt = 25, sigma_prob = 0.05
  )
)

message("--- Configuration Successfully Loaded ---\n")
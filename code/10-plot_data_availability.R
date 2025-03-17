library(sdmTMB)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(visreg)
library(ggpubr)
setwd("~/Dropbox/GitHub/o2-sdm")

#Load functions
source("code/helper_funs.R")

theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

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

count <- dat %>% count(year, survey, region)
count <- dat %>% count(survey)
count <- dat %>% count(region)

#number in each survey

#Region labels
labs <- c("Aleutian Islands", "British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska")
names(labs) <- c("ai", "bc", "cc", "ebs", "goa")
dat$region <- factor(dat$region, levels=c("cc", "bc", "goa", "ebs", "ai"))

##Plot barplot of all data available
ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=3, labeller = labeller(region=labs), scales="free_y")+
  scale_x_continuous(breaks=c(2000,2010,2020), limits=c(2000,2027))+
  theme_minimal(base_size=20)+
  xlab("Year")+
  ylab("Number of Observations")+
  scale_fill_discrete(labels=
  c("*AFSC Aleutian Islands", "calCOFI", "CODAP", "*DFO", "*AFSC Bering Sea",
    "*AFSC Gulf of Alaska", "NWFSC Hake", "*IPHC Longline", "Newport Line", "*NWFSC West Coast",
    "OCNMS", "WCOA"))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))

ggsave(
  paste("outputs/plots/data_available.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 7,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

###Map of all data available
# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)


###Map of data available
ggplot(us_coast_proj) + geom_sf() +
  xlim(min(dat$X)*1000, max(dat$X)*1000)+
  ylim(min(dat$Y)*1000, max(dat$Y)*1000)+
  geom_point(filter(dat, year>1999), mapping=aes(x=X*1000, y=Y*1000,colour=survey))+
  facet_wrap("year", ncol=7)+
  theme_minimal(base_size=20)+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_colour_discrete(labels=
                        c("*AFSC Aleutian Islands", "calCOFI", "CODAP", "*DFO", "*AFSC Bering Sea",
                          "*AFSC Gulf of Alaska", "NWFSC Hake", "*IPHC Longline", "Newport Line", "*NWFSC West Coast",
                          "OCNMS", "WCOA"))+
  theme(legend.position="none")


ggsave(
  paste("outputs/plots/data_available_map.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 11,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

###Plot of trawl data available versus synoptic data available
##Read in example fish data
fish_dat <- as.data.frame(readRDS("data/processed_data/fish_trawls.rds"))
fish_dat$region <- case_when(fish_dat$survey=="dfo"~"bc",
                             fish_dat$survey=="EBS"~"ebs",
                             fish_dat$survey=="GOA"~"goa",
                             fish_dat$survey=="nwfsc"~"cc",
                             fish_dat$survey=="NBS"~"ebs")
labs <- c("British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska")
names(labs) <- c("bc", "cc", "ebs", "goa")
dat2 <- filter(dat, region!="ai")
dat2 <- dat2 %>%
  filter(survey %in% c("nwfsc", "dfo", "goa", "EBS"))
dat2$type <- "Concurrent Oxygen Data"
fish_dat$type <- "Bottom Trawl Survey"
labs <- c("British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska")
names(labs) <- c("bc", "cc", "ebs", "goa")
fish_dat$geometry <- NULL
fish_dat <- as.data.frame(fish_dat)
dat2 <- as.data.frame(dat2)
dats <- bind_rows(fish_dat, dat2)
dats$region <- factor(dats$region, levels=c("cc", "bc", "goa", "ebs"))

ggplot(dats, aes(x=year, fill=type, linetype=type))+
  stat_count()+
  facet_wrap("region", ncol=2, labeller = labeller(region=labs))+
  #xlim(2000,2024)+
  theme_minimal(base_size=20)+
  xlab("Year")+
  ylab("Number of Observations")+
  theme(legend.position="top")+
  scale_fill_manual(values=c("black", "grey70"))+
  theme(legend.title=element_blank())

ggsave(
  paste("outputs/plots/fish_trawl_synoptic.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 6.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##Example plot of GOBH data for region and year
##GOBH data
load("outputs/o2_models_glorys/cc.Rdata")
cc <- output
cc <- cc[[4]][["glorys_data"]]
dat <- filter(dat, region=="cc"&year==2012)
dat$type <- "Bottom Trawl Survey"
cc$type <- "Global Ocean Biogeochemisty Hindcast"

test <- bind_rows(dat,cc)

ggplot(us_coast_proj) + geom_sf() +
  ylim(4400*1000, 4600*1000)+
  geom_point(test,mapping=aes(x=X*1000, y=Y*1000,shape=type, colour=type))+
  theme_minimal(base_size=20)+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(breaks=c(-125,-126), limits=c(300*1000, 450*1000))+
  theme(legend.position="top")+
  theme(legend.direction="vertical")+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))+
  scale_colour_manual(values=c("navyblue", "orange"))+
  scale_shape_manual(values=c(19,8))

ggsave(
  paste("outputs/plots/gobh_trawl_example.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 5,
  height = 5.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)


##Plot regional extents
ggplot(us_coast_proj) + geom_sf() +
  xlim(min(dat$X)*1000, max(dat$X)*1000)+
  ylim(min(dat$Y)*1000, max(dat$Y)*1000)+
  geom_point(filter(dat, year>1999), mapping=aes(x=X*1000, y=Y*1000,colour=region))+
  theme_minimal(base_size=20)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(legend.position="none")

ggsave(
  paste("outputs/plots/map_all_regions.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 8.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

###Plot density of oxygen observations by survey type
dat2 <- dat %>%
  filter(survey %in% c("nwfsc", "dfo", "goa", "EBS",  "ai"))
dat2$type <- "Synoptic"
dat3 <- dat %>%
  filter(!survey %in% c("nwfsc", "dfo", "goa", "EBS", "ai"))
dat3$type <- "Independent"
dat <- bind_rows(dat2, dat3)

ggplot(dat, aes(x=o2))+
  facet_wrap("region", ncol=3, labeller = labeller(region=labs), scales="free_y")+
  geom_density(aes(colour=type, linetype=type), size=1, fill="lightgrey", alpha=0.1)+
  xlab("Dissolved Oxygen")+
  ylab("Density")+
  theme_bw(base_size=25) +
  theme_minimal(base_size=20)+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.position=c(0.8,0.3))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))+
  scale_colour_manual(values=c("black", "black"))+
  scale_linetype_manual(values=c("dashed", "solid"))

ggsave(
  paste("outputs/plots/oxygen_data_distribution.png"),
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


##Each survey
ggplot(dat, aes(x=o2))+
  facet_wrap("region", ncol=3, labeller = labeller(region=labs), scales="free_y")+
  geom_density(aes(fill=survey))+
  xlab("Dissolved Oxygen")+
  ylab("Density")+
  theme_bw(base_size=20) +
  theme_minimal()+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.position=c(0.8,0.3))+
  scale_fill_discrete(labels=
                        c("*AFSC Aleutian Islands", "calCOFI", "CODAP", "*DFO", "*AFSC Bering Sea",
                          "*AFSC Gulf of Alaska", "NWFSC Hake", "*IPHC Longline", "Newport Line", "*NWFSC West Coast",
                          "OCNMS", "WCOA"))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))

ggsave(
  paste("outputs/plots/oxygen_data_distribution.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 8.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##Salinity/temp/depth correlogram
p1 <- ggplot(dat, aes(y=-depth, x=o2))+
  geom_point(aes(colour=temp))+
  facet_wrap("region", ncol=1, labeller = labeller(region=labs), scales="free_y")+
  scale_colour_viridis()+
  theme_bw(base_size=20) +
  theme_minimal()+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15), legend.position="top")+
  xlab("Dissolved Oxygen")+
  ylab("Depth (m)")
p2 <- ggplot(dat, aes(y=-depth, x=o2))+
  geom_point(aes(colour=sigma0))+
  facet_wrap("region", ncol=1, labeller = labeller(region=labs), scales="free_y")+
  scale_colour_viridis()+
  theme_bw(base_size=20) +
  theme_minimal()+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15), legend.position="top")+
  xlab("Dissolved Oxygen")
ggarrange(p1, p2)

ggsave(
  paste("outputs/plots/o2_temp_sal_depth_correlation.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 8.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##Evaluate salinity data
dat <- as.data.frame(readRDS("data/processed_data/all_o2_dat_filtered.rds"))

ggplot(dat, aes(x=sigma0))+
  geom_histogram(aes(fill=as.factor(year)))+
  facet_wrap("region", scales="free")+
  xlim(0,24)+
  theme_minimal()+
  theme(text=element_text(size=15))

#cc <- filter(dat, region=="cc")
ggplot(us_coast_proj) + geom_sf() +
  xlim(min(dat$X)*1000, max(dat$X)*1000)+
  ylim(min(dat$Y)*1000, max(dat$Y)*1000)+
  geom_point(filter(dat, sigma0<24), mapping=aes(x=X*1000, y=Y*1000,colour=sigma0), size=2, position="jitter")+
  theme_minimal()+
  theme(text=element_text(size=15))+
  theme(legend.position=c(0.5,0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Salinity Observations Below Sigma0=24")

ggplot(us_coast_proj) + geom_sf() +
  xlim(min(dat$X)*1000, max(dat$X)*1000)+
  ylim(min(dat$Y)*1000, max(dat$Y)*1000)+
  geom_point(filter(dat, sigma0<24),
             mapping=aes(x=X*1000, y=Y*1000, colour=sigma0), size=2)+
  theme_minimal()+
  facet_wrap("year")+
  theme(text=element_text(size=15))+
  theme(legend.position=c(0.9,0.2))+
  ggtitle("Below sigma0=24")+
  ylab("Latitude")+
  xlab("Longitude")

#Temperature and salinity available

#model salinity
# remove older (earlier than 2000) data
dat <- dplyr::filter(dat, year >=2000)

#Log depth
dat$depth_ln <- log(dat$depth)

#Remove any rows with missing data
dat <- dat %>%
  drop_na(depth, depth_ln, o2, temp, sigma0, doy, X, Y)
dat$o2 <- dat$o2/100

region_list <- c("cc", "bc", "goa", "ebs", "ai")
pdf("outputs/salinity_smooth_effects.pdf")
par(mfrow=c(3,2))
for (i in 1:length(region_list)){
test_region <- region_list[i]
test_dat <- filter(dat, region==test_region)
spde <- make_mesh(data = test_dat,
                  xy_cols = c("X", "Y"),
                  cutoff = 45)
m <- try(sdmTMB(
  formula = o2  ~ 1 + s(sigma0),
  mesh = spde,
  data = test_dat,
  family = gaussian(),
  spatial = "on",
  spatiotemporal  = "off"
))
visreg(m, "sigma0", ylim=c(-2,10))
}

dev.off()

##Plot correlations of depth/temp/sal/o2 (colored by region)



##Plot distribution of O2 values for each survey by each region










####Other options
#Filter by region
dat.2.use <- as.data.frame(filter(dat, region=="cc"))

count <- dat.2.use %>% count(year, survey)

ggplot(dat.2.use, aes(x=year))+stat_count(aes(fill=survey))+xlim(2012,2019)

dat.2.use <- as.data.frame(filter(dat, region=="bc"))

count <- dat.2.use %>% count(year, survey)

dat$region <- case_when(dat$region=="ai"~"Aleutian Islands",
                        dat$region=="bc"~"British Columbia",
                        dat$region=="cc"~"California Current",
                        dat$region=="ebs"~"Eastern Bering Sea",
                        dat$region=="goa"~"Gulf of Alaska")

ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=1, nrow=5, scales="free_y")+
 theme_minimal()+ylab("Number of oxygen observations")+xlab("Year")

ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=1, nrow=5, scales="free_y")+
  theme_minimal()+
  ylab("Number of oxygen observations")+
  xlab("Year")+
  xlim(1995,2023)

ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=1, nrow=5, scales="free_y")+
  theme_minimal()+
  ylab("Number of oxygen observations")+
  xlab("Year")+
  xlim(2015,2023)

#Plot temp/sal/o2 data available
dat_sablefish$temperature_C <- ifelse((is.na(dat_sablefish$temperature_C)& !is.na(dat_sablefish$bottom_temperature_c)), dat_sablefish$bottom_temperature_c, dat_sablefish$temperature_C)
dat_sablefish$sal <- ifelse(!is.na(dat_sablefish$salinity_psu), 1,0)
dat_sablefish$t <- ifelse(!is.na(dat_sablefish$temperature_C), 1,0)
dat_sablefish$d <-  ifelse(!is.na(dat_sablefish$O2_umolkg), 1,0)
dat_sablefish$available <- case_when((dat_sablefish$t==1 & dat_sablefish$sal==1 &dat_sablefish$d==0)~"temp+sal",
                                     (dat_sablefish$t==1 & dat_sablefish$sal==1 &dat_sablefish$d==1)~"all",
                                     (dat_sablefish$t==1 & dat_sablefish$sal==0 &dat_sablefish$d==0)~"temp only",
                                     (dat_sablefish$t==0 & dat_sablefish$sal==1 &dat_sablefish$d==0)~"sal only",
                                     (dat_sablefish$t==0 & dat_sablefish$sal==0 &dat_sablefish$d==0)~"none")


ggplot(dat_sablefish, aes(x=year, y=cpue_kg_km2))+geom_col(aes(fill=available))+
  facet_wrap("survey", scales="free")+
  theme_minimal()+
  theme(text=element_text(size=15))+
  theme(legend.position=(c(0.8,0.2)))

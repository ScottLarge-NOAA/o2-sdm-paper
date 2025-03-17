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

# load regional polygons
regions.hull <- readRDS("data/processed_data/regions_hull.rds")

#Pull the previously run output
use_previous <- T
if(use_previous) {
  load("outputs/sablefish_application.RData")
  load("outputs/dsole_application.RData")
}

###Map oxygen values below estimated threshold ###
table <- par_table(sablefish_cc, "sablefish", "cc")
table <- par_table_raw(sablefish_cc, "sablefish", "cc")
# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)
#Set latitude and longitude
#Set latitude and longitude
dat1 <- sablefish_cc[["data"]][["Synoptic"]]
xlims <- c(min(dat1$X)*1000, max(dat1$X)*1000)
ylims <- c(min(dat1$Y)*1000, max(dat1$Y)*1000)
lats <- c(round(min(dat1$latitude)),  round(max(dat1$latitude)))
lons <- c(round(min(dat1$longitude)+2), round(max(dat1$longitude)))

dat1 <- sablefish_cc[["data"]][["Synoptic"]]
dat1$threshold <- table$estimate[2]
dat1$se<- table$std.error[2]
dat2 <- sablefish_cc[["data"]][["Predicted"]]
dat2$threshold <- table$estimate[4]
dat2$se<- table$std.error[4]
dat3 <- sablefish_cc[["data"]][["GOBH"]]
dat3$threshold <- table$estimate[6]
dat3$se<- table$std.error[6]

a <-ggplot(us_coast_proj) + geom_sf() +
  geom_point(filter(dat1,year==2023&o2>threshold), mapping=aes(x=X*1000, y=Y*1000), colour="lightblue", alpha=0.4)+
  geom_point(filter(dat1,year==2023&o2<threshold), mapping=aes(x=X*1000, y=Y*1000), alpha=0.4, colour="red2")+
   # geom_point(filter(dat1,year==2023&o2<(threshold+se)), mapping=aes(x=X*1000, y=Y*1000), colour="green4", alpha=0.4)+
  ylim(ylims)+
  scale_x_continuous(breaks=lons, limits=xlims)+
  theme_minimal()+
  theme(text=element_text(size=15))+
  xlab(" ")+
  ylab("Latitude")+
  ggtitle("Concurrent")

b <-ggplot(us_coast_proj) + geom_sf() +
  geom_point(filter(dat2,year==2023&o2>threshold), mapping=aes(x=X*1000, y=Y*1000), colour="lightblue", alpha=0.4)+
  geom_point(filter(dat2,year==2023&o2<threshold), mapping=aes(x=X*1000, y=Y*1000), alpha=0.4, colour="red2")+
  # geom_point(filter(dat1,year==2023&o2<(threshold+se)), mapping=aes(x=X*1000, y=Y*1000), colour="green4", alpha=0.4)+
  ylim(ylims)+
  scale_x_continuous(breaks=lons, limits=xlims)+
  theme_minimal()+
  theme(text=element_text(size=15))+
  xlab("Longitude")+
  ylab("")+
  ggtitle("Empirical \n Statistical")

c <-ggplot(us_coast_proj) + geom_sf() +
  geom_point(filter(dat3,year==2023&o2>threshold), mapping=aes(x=X*1000, y=Y*1000), colour="lightblue", alpha=0.4)+
  geom_point(filter(dat3,year==2023&o2<threshold), mapping=aes(x=X*1000, y=Y*1000), alpha=0.4, colour="red2")+
  # geom_point(filter(dat1,year==2023&o2<(threshold+se)), mapping=aes(x=X*1000, y=Y*1000), colour="green4", alpha=0.4)+
  ylim(ylims)+
  scale_x_continuous(breaks=lons, limits=xlims)+
  theme_minimal()+
  theme(text=element_text(size=15))+
  xlab("")+
  ylab(" ")+
  ggtitle("Oceanographic")

ggarrange(a,b,c, ncol=3, common.legend=T)

#Save
ggsave(
  paste("outputs/plots/unsuitable_habitat.png"),
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

##With standard errors
a <-ggplot(us_coast_proj) + geom_sf() +
  geom_point(filter(dat1,year==2023&o2>threshold), mapping=aes(x=X*1000, y=Y*1000), colour="lightblue", alpha=0.4)+
   geom_point(filter(dat1,year==2023&o2<(threshold+se)), mapping=aes(x=X*1000, y=Y*1000), colour="yellow", alpha=0.4)+
  geom_point(filter(dat1,year==2023&o2<threshold), mapping=aes(x=X*1000, y=Y*1000), alpha=0.4, colour="orange")+
   geom_point(filter(dat1,year==2023&o2<(threshold-se)), mapping=aes(x=X*1000, y=Y*1000), alpha=0.4, colour="red2")+
  ylim(ylims)+
  scale_x_continuous(breaks=lons, limits=xlims)+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Synoptic")

b <-ggplot(us_coast_proj) + geom_sf() +
  geom_point(filter(dat2,year==2023&o2>threshold), mapping=aes(x=X*1000, y=Y*1000), colour="lightblue", alpha=0.4)+
  geom_point(filter(dat2,year==2023&o2<(threshold+se)), mapping=aes(x=X*1000, y=Y*1000), colour="yellow", alpha=0.4)+
  geom_point(filter(dat2,year==2023&o2<threshold), mapping=aes(x=X*1000, y=Y*1000), alpha=0.4, colour="orange")+
  geom_point(filter(dat2,year==2023&o2<(threshold-se)), mapping=aes(x=X*1000, y=Y*1000), alpha=0.4, colour="red2")+
  ylim(ylims)+
  scale_x_continuous(breaks=lons, limits=xlims)+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Predicted")

c <-ggplot(us_coast_proj) + geom_sf() +
  geom_point(filter(dat3,year==2023&o2>threshold), mapping=aes(x=X*1000, y=Y*1000), colour="lightblue", alpha=0.4)+
  geom_point(filter(dat3,year==2023&o2<(threshold+se)), mapping=aes(x=X*1000, y=Y*1000), colour="yellow", alpha=0.4)+
  geom_point(filter(dat3,year==2023&o2<threshold), mapping=aes(x=X*1000, y=Y*1000), alpha=0.4, colour="orange")+
  geom_point(filter(dat3,year==2023&o2<(threshold-se)), mapping=aes(x=X*1000, y=Y*1000), alpha=0.4, colour="red2")+
  ylim(ylims)+
  scale_x_continuous(breaks=lons, limits=xlims)+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("GOBH")

ggarrange(a,b,c, ncol=3, common.legend=T)

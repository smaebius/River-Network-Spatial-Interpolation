
rm(list=ls(all.names=TRUE)) ; cat('/014'); gc()

library(tidyverse)

################################################################################
# obtain average precipitation

path.root = "C:/Users/sm6432/Projects/Spatial-Interpolation/"

station.index.t <- read.csv(paste0(path.root,"data/StationIndex_9323sites_v1.4_R1.csv"),
                            colClasses = c("Site.ID"="character"))

input.path = "W:/Users/HKim/StatisticalAttributionModel/FloodMagnitude_v1.4/00_Station_info/Inputdata/CONUS/Drivers_Basin_and_Season_Average/"

Basin.tot <- sf::st_read(paste0(path.root,"data/DE_basin.gpkg"))

# convert to sf object
stations.sf = st_as_sf(station.index.t, coords = c("Lon", "Lat"), 
                       crs = 4326, agr = "constant")

# filter to DE river basin
DE.stations = st_filter(stations.sf, Basin.tot)

ggplot() +
  geom_sf(data = Basin.tot) +
  geom_sf(data = DE.stations, color = "blue", size = 2) +
  coord_sf(datum = st_crs(Basin.tot)) +
  theme_bw()

################################################################################

sites = unique(DE.stations$Site.ID)

s = sites[1]



plot(s.ppt$DJF)

DJF.climate = NULL

for (s in sites) {
  s.input = read.csv(paste0(input.path, "ts-", s, "_Basin_and_Season_Average.csv"))
  s.ppt = s.input %>% filter(Variable == "PPT")
  s.tmean = s.input %>% filter(Variable == "TMEAN")
  s.df = data.frame(Site.ID = s,
                    DJF.ppt.ave = mean(s.ppt$DJF, na.rm=T),
                    DJF.tmean.ave = mean(s.tmean$DJF, na.rm=T))
  DJF.climate = rbind(DJF.climate, s.df)
}

DJF.climate

DE.stations = left_join(DE.stations, DJF.climate, by="Site.ID")

ggplot() +
  geom_sf(data = Basin.tot) +
  geom_sf(data = DE.stations, aes(color=DJF.ppt.ave), size = 2) +
  coord_sf(datum = st_crs(Basin.tot)) +
  theme_bw()

# st_write(DE.stations, paste0(path.root,"data/DE_stations.gpkg"))

# Test Run of Torgegram for Delaware River Basin
# https://usepa.github.io/SSN2/articles/introduction.html
# https://pet221.github.io/SSNbler/articles/introduction.html
# Elevation: https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html

rm(list=ls(all.names=TRUE)) ; cat('\014'); gc()

library(tidyverse)
library(sf)
library(SSN2)
library(SSNbler)

################################################################################

path.root = "C:/Users/sm6432/Projects/River-Network-Spatial-Interpolation/"
# path.root = "/scratch/gpfs/GVILLARI/sm6432/Spatial-Interpolation/"

# load station index
station.index.t <- read.csv(paste0(path.root,"data/StationIndex_5720sites_v1.4_R3.csv"),
                              colClasses = c("Site.ID"="character"))

stations.sf = st_as_sf(station.index.t, coords = c("Lon", "Lat"),
                       crs = 4326, agr = "constant")

# load additional USGS gages
# usgs.sites <- read.csv(paste0(path.root, "data/USGS-gages.csv"))


# gpkg for stations with added climatology
usgs.sites.sf <- st_read(paste0(path.root,"data/DE_stations.gpkg"))

# filter to "observed" data with model parameters
sf.cols <- c("DJF.ppt.ave", "DJF.tmean.ave", "geom")
stations.sf <- st_join(stations.sf, usgs.sites.sf[,sf.cols], left=T)
stations.sf <- st_filter(stations.sf, usgs.sites.sf)

# read the basin shape file for comid 24903566
Basin.tot <- sf::st_read(paste0(path.root,"data/DE_basin.gpkg"))

# load DE flowlines
# DE.flow <- st_read(paste0(path.root,
#                           "data/flowlines_outlet_comid_24903566/flowlines_outlet_comid_24903566.shp"))
# load NSI flowlines for the midatlantic
# https://research.fs.usda.gov/rmrs/projects/national-stream-internet#download-data
MA.flow <- st_read(paste0(path.root, "data/US-MA-NSI/Flowline_MA02_NSI.shp"))
DE.flow = st_transform(MA.flow, st_crs(Basin.tot))
DE.flow <- st_filter(DE.flow, Basin.tot)

# # convert to sf object
# stations.sf = st_as_sf(station.index.t, coords = c("Lon", "Lat"), 
#                        crs = 4326, agr = "constant")
# usgs.sites.sf = st_as_sf(usgs.sites, coords = c("dec_long_va", "dec_lat_va"), 
#                        crs = 4326, agr = "constant")

# filter to DE river basin
DE.stations = st_filter(stations.sf, Basin.tot)
DE.stations.full = st_filter(usgs.sites.sf, Basin.tot)

# visualize data
ggplot() +
  # geom_sf(data = Basin.tot) +
  geom_sf(data = DE.flow) +
  geom_sf(data = DE.stations.full, colour = "purple", size = 1) +
  geom_sf(data = DE.stations, color = "blue", size = 2) +
  coord_sf(datum = st_crs(DE.flow)) +
  theme_bw()

# transform to DE basin crs
# Proj4 of http://spatialreference.org/ref/esri/usa-contiguous-albers-equal-area-conic/
crs.usa = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
DE.flow = st_transform(DE.flow, crs.usa)
# stations.sf = st_transform(stations.sf, st_crs(Basin.tot))
# usgs.sites.sf = st_transform(usgs.sites.sf, st_crs(Basin.tot))
DE.stations = st_transform(DE.stations, crs.usa)
DE.stations.full = st_transform(DE.stations.full, crs.usa)

# create predictions dataset from stream branches
# https://gis.stackexchange.com/questions/277219/sf-equivalent-of-r-maptools-packages-spatiallinesmidpoints
st_line_midpoints <- function(sf_lines = NULL) {
  g <- st_geometry(sf_lines)
  g_mids <- lapply(g, function(x) {
    coords <- as.matrix(x)
    # this is just a copypaste of View(maptools:::getMidpoints):
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }
    mids <- st_point(get_mids(coords))
  })
  out <- st_sfc(g_mids, crs = st_crs(sf_lines))
  out <- st_sf(out)
}

DE.pts <- st_line_midpoints(DE.flow)

# st_crs(DE.pts) == st_crs(DE.stations)
# plot(DE.flow[,1])
# plot(DE.pts)
################################################################################
# format to SSN object

# create a landscape network from flowlines
flow.path <- paste0(path.root, "data/DE-NSI")

edges <- lines_to_lsn(
  streams = DE.flow,
  lsn_path = flow.path,
  check_topology = TRUE,
  snap_tolerance = 0.05, # distance between node and edge
  topo_tolerance = 1, # distance between two nodes
  overwrite = TRUE
)

# check topological errors
node.errors <- sf::st_read(paste0(path.root,"data/DE-NSI/node_errors.gpkg"))
node.errors <- st_transform(node.errors, crs.usa)
table(node.errors$error)

DE.flow.errors <- st_filter(DE.flow, node.errors)

# filter out the smaller links
n <- nrow(node.errors)
flow.bad.links <- NULL
for (i in 1:n) {
  flow.bad.link <- st_filter(DE.flow, node.errors[i,]) %>%
    filter(AreaSqKM == min(AreaSqKM))
  flow.bad.links <- rbind(flow.bad.links, flow.bad.link)
}

# add special case
flow.bad.link <- DE.flow %>% filter(REACHCODE == "02040205000469")
flow.bad.links <- rbind(flow.bad.links, flow.bad.link)

# remove bad links
DE.flow.clean <- DE.flow %>%
  filter(!geometry %in% flow.bad.links$geometry)

# test results
# plot(DE.flow.clean[,1])


################################################################################
# read clean results
DE.flow.clean <- st_read(paste0(path.root,"data/DE-basin-network.gpkg"))

# create landscape network from clean network
flow.path <- paste0(path.root, "data/DE-NSI")
edges <- lines_to_lsn(
  streams = DE.flow.clean,
  lsn_path = flow.path,
  check_topology = TRUE,
  snap_tolerance = 0.05, # distance between node and edge
  topo_tolerance = 1, # distance between nodes
  overwrite = TRUE
)

# st_write(DE.flow.clean, paste0(path.root,"data/DE-basin-network.gpkg"), delete_dsn=T)

################################################################################
# incorporate sites to landscape network

obs <- sites_to_lsn(
  sites = DE.stations,
  edges = edges,
  lsn_path = flow.path,
  file_name = "obs.gpkg",
  snap_tolerance = 100,
  save_local = TRUE,
  overwrite = TRUE
)

preds <- sites_to_lsn(
  sites = DE.pts,
  edges = edges,
  save_local = TRUE,
  lsn_path = flow.path,
  file_name = "pred-DE.gpkg",
  snap_tolerance = 100,
  overwrite = TRUE
)

################################################################################
# visualize upstream distance
edges <- updist_edges(
  edges = edges,
  save_local = TRUE,
  lsn_path = flow.path,
  calc_length = TRUE,
  overwrite = T
)

names(edges)

site.list <- updist_sites(
  sites = list(
    obs = obs,
    pred = preds),
  edges = edges,
  length_col = "Length",
  save_local = TRUE,
  lsn_path = flow.path,
  overwrite = T
)

ggplot() +
  geom_sf(data = edges, aes(color = upDist)) +
  geom_sf(data = site.list$obs, aes(color = upDist)) +
  coord_sf(datum = 4326) +
  scale_color_viridis_c(name="Upstream Distance") +
  theme_bw()

################################################################################

# calculate additive function values (afvs) and segment proportional influence (PI)
# https://pubs.usgs.gov/of/2019/1096/ofr20191096.pdf

edges <- afv_edges(
  edges = edges,
  infl_col = "TotDASqKM", # total upstream cumulative drainage area (at downstream end)
  segpi_col = "areaPI",
  afv_col = "afvArea",
  lsn_path = flow.path
)

site.list <- afv_sites(
  sites = site.list,
  edges = edges,
  afv_col = "afvArea",
  save_local = TRUE,
  lsn_path = flow.path
)

summary(edges$afvArea)
summary(site.list$pred$afvArea)

################################################################################

# create SSN object

DE_ssn <- ssn_assemble(
  edges = edges,
  lsn_path = flow.path,
  obs_sites = site.list$obs,
  preds_list = site.list[c("pred")],
  ssn_path = paste0(path.root, "data/DE.ssn-v2"),
  import = TRUE,
  check = TRUE,
  afv_col = "afvArea",
  overwrite = TRUE
)

DE_ssn <- ssn_assemble(
  edges = edges,
  lsn_path = flow.path,
  obs_sites = site.list$obs,
  preds_list = site.list[c("pred")],
  ssn_path = paste0(path.root, "data/DE.ssn-v2"),
  import = TRUE,
  check = TRUE,
  afv_col = "afvArea",
  overwrite = TRUE
)

DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                           predpts="pred")

################################################################################

# add flood magnitude parameters
DJF.params <- read.csv(paste0(path.root, "data/Output03_parameters_DJF.csv"))

DE.obs = DE_ssn$obs
DE.obs = DE.obs %>%
  left_join(DJF.params, by="Site.ID")

# check for zero inflated distribution
nrow(DE.obs[DE.obs$Nu_intercept != 0,])

# load results to ssn object
DE_ssn$obs = DE.obs
# DE_ssn$edges = DE_ssn$edges %>%
#   left_join(DJF.param)

ggplot() +
  geom_sf(data = DE_ssn$edges, color="gray") +
  geom_sf(data = DE_ssn$obs, aes(color = Mu_prec.conc)) +
  coord_sf(datum = 4326) +
  scale_color_viridis_c(name="Mu: DJF Concurrent Precipitation") +
  theme_bw()

################################################################################

# add elevation data
# https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
obs.elev <- get_elev_point(DE_ssn$obs, src = "epqs")
preds.elev <- get_elev_point(DE_ssn$preds$pred, src = "epqs")
# write_sf(obs.elev, paste0(path.root, "data/obs.elevation.gpkg"))
# write_sf(preds.elev, paste0(path.root, "data/preds.elevation.gpkg"))

################################################################################

# fit spatial model
# response: Mu DJF concurrent precipitation
# explanatory: elevation only?

# directed hydrological distance matrices
ssn_create_distmat(
  ssn.object = DE_ssn,
  predpts = "pred",
  among_predpts = TRUE,
  overwrite = TRUE
)

names(DE_ssn$obs)

tg <- Torgegram(
  formula = Mu_prec.conc ~Mu_prec.conc,
  ssn.object = DE_ssn,
  type = c("flowcon", "flowuncon", "euclid")
)

plot(tg)

plot(tg$flowcon$dist, tg$flowcon$gamma)
plot(tg$flowuncon$dist, tg$flowuncon$gamma)
plot(tg$euclid$dist, tg$euclid$gamma)

par(mfrow=c(1,3))
plot(tg, type="flowcon")
plot(tg, type="flowuncon")
plot(tg, type="euclid")
par(mfrow=c(1,1))

summary(tg$flowcon)

ssn_mod <- ssn_lm(
  formula = Mu_prec.conc ~ upDist + DJF.ppt.ave + DJF.tmean.ave,
  ssn.object = DE_ssn,
  tailup_type = "exponential",
  taildown_type = "exponential",
  euclid_type = "gaussian",
  additive = "afvArea"
)


summary(ssn_mod)

varcomp(ssn_mod)

plot(ssn_mod)

predict(ssn_mod, newdata = "pred")

aug_preds <- augment(ssn_mod, newdata="pred")
aug_preds[,".fitted"]

# distinguish between train and test
aug_preds$data = "test"
train.sites = unique(DE.stations$Site.ID)
aug_preds[aug_preds$Site.ID %in% train.sites,]$data = "train"

ggplot() +
  geom_sf(data = DE_ssn$edges) +
  geom_sf(data = aug_preds, aes(shape=data), color="black", size = 5) +
  geom_sf(data = aug_preds, aes(color = .fitted, shape=data), size = 4) +
  scale_color_viridis_c(option = "H") +
  theme_bw()

# library(gamlss)
# newdata<-rGA(1000,mu=5000,sigma=0.45)
# hist(newdata)

# qGA(0.50, mu = exp(seas.param.values$Mu_intercept +
#                      seas.param.values$Mu_prec.conc *
#                      revert.NQ(P1, data.seas$P1) +
#                      seas.param.values$Mu_temp.conc * 
#                      revert.NQ(T1, data.seas$T1) +
#                      seas.param.values$Mu_prec.lag *
#                      revert.NQ(P2, data.seas$P2) +
#                      seas.param.values$Mu_temp.lag *
#                      revert.NQ(T2, data.seas$T2)),
    # sigma = exp(seas.param.values$Sig_intercept +
    #               seas.param.values$Sig_prec.conc *
    #               revert.NQ(P1, data.seas$P1) +
    #               seas.param.values$Sig_temp.conc *
    #               revert.NQ(T1, data.seas$T1) +
    #               seas.param.values$Sig_prec.lag *
    #               revert.NQ(P2, data.seas$P2) +
    #               seas.param.values$Sig_temp.lag *
    #               revert.NQ(T2, data.seas$T2)))




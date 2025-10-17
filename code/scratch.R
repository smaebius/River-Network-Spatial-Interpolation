# Delaware outlet and river
# https://reference.geoconnex.us/collections/mainstems/items?filter=name_at_outlet%20ILIKE%20%27%Delaware%%27https://reference.geoconnex.us/collections/mainstems/items?filter=name_at_outlet%20ILIKE%20%27%Delaware%%27
# https://geoconnex.us/ref/mainstems/2122629
# https://geoconnex.us/nhdplusv2/comid/24903800
library(nhdplusTools)

# 3DHP tutorial
# https://cran.r-project.org/web/packages/nhdplusTools/vignettes/get_3dhp_data.html
comid <- "24903800"

point <- c(-75.357, 39.275) |>
  sf::st_point() |>
  sf::st_sfc(crs = 4326) |> sf::st_sf()

# dm <- c('https://geoconnex.us/ref/mainstems/323742',
# 'https://geoconnex.us/ref/mainstems/312091')

flowline <- get_3dhp(point, type = "flowline", buffer = 500)
flowline <- flowline %>% filter(id3dhp == "G9OPE")

pc <- function(x) sf::st_geometry(sf::st_transform(x, 3857))

# plot_nhdplus(bbox = sf::st_bbox(flowline),
#              plot_config = list(flowline = list(col = NULL)), zoom = 10)
# plot(pc(flowline), add = TRUE, col = "darkblue", lwd = 2)
# plot(pc(point), pch = "O", add = TRUE)

# get flowlines
basin <- dataRetrieval::findNLDI(comid = comid, find = "basin")
# basin <- basin[[1]]
network <- get_3dhp(basin$basin, type = "flowline")
water <- get_3dhp(basin$basin, type = "waterbody")
hydrolocation <- get_3dhp(basin$basin, type = "hydrolocation - reach code, external connection")

# down_mains <- get_3dhp(ids = dm, type = "flowline")

old_par <- par(mar = c(0, 0, 0, 0))
plot_nhdplus(bbox = sf::st_bbox(basin$basin), flowline_only = TRUE,
             plot_config = list(flowline = list(col = NULL)), zoom = 10)
plot(pc(basin$basin), lwd = 2, add = TRUE)
plot(pc(network), lwd = 0.5, add = TRUE)
plot(pc(water), lwd = 0.5, border = "skyblue", col = "lightblue", add = TRUE)
plot(pc(hydrolocation), pch = "o", col = "#80808026", add = TRUE)
# plot(pc(down_mains), lwd = 3, col = "blue", add = TRUE)


################################################################################
# DE basin outlet
comid = "24903566"

# get basin boundary
basin <- dataRetrieval::findNLDI(comid = comid, find = "basin")
# st_write(basin$basin, paste0(path.root, "data/DE_basin.gpkg"))

# get HUC 12

x = dataRetrieval::findNLDI(comid = comid, find = "nwissite", distance_km=5000)

# subset points along tributaries
# dataRetrieval::get_nldi_sources()$source
upstream_nwis <- navigate_nldi(nldi_feature,
                               mode = "upstreamTributaries",
                               data_source = "nwissite", 
                               distance_km = 1000)

dataRetrieval::findNLDI(comid = comid, find = "nwissite", distance_km=1000)

# Get the bounding box coordinates
bbox <- st_bbox(basin$basin)

# Retrieve all USGS sites within the bounding box
library(dataRetrieval)
huc_gages <- whatNWISdata(huc = "05010007", parameterCd="00060", serive="uv")
head(huc_gages)

huc_gages <- whatNWISdata(huc = "05010007", parameterCd = "00060", service="uv")
head(huc_gages)

# DE huc 8
huc_list <- c("02040101", "02040102", "02040103", "02040104", "02040105",
              "02040106", "02040201", "02040202", "02040203", "02040204",
              "02040205", "02040206", "02040207", "02040303")
huc_gages <- whatNWISdata(huc = huc_list, parameterCd = "00060", service="uv")
head(huc_gages)


statCd <- "00003" 
startDate <- "1990-01-01"
endDate <- "2019-12-31"
states <- stateCd
result <- vector('list', length(huc_list))

for(i in 1:length(huc_list)) {
  print(i)
  huc_gages = whatNWISdata(huc = huc_list[i], parameterCd = "00060")
  result[[i]] <- huc_gages
}

full_data <- do.call(rbind.data.frame, result)

# save gages
write.csv(full_data, paste0(path.root, "data/USGS-gages.csv"), row.names = F)

# get end points for target locations
DE_points = unique(st_cast(DE_flow, "POINT"))

# get centroids of flow lines
st_line_interpolate_point(DE_flow, )


################################################################################
# clean topological errors with sfnetworks


library(sfnetworks)
library(tidygraph)
library(igraph)
library(dbscan)
# https://r-spatial.org/r/2019/09/26/spatial-networks.html
# https://luukvdmeer.github.io/sfnetworks/articles/sfn03_join_filter.html
# https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html
# https://shriv-portfolio.netlify.app/post/network-issues/

# # work with a smaller example
# DE.small.ex = read.csv(paste0(path.root, "data/small-flow-ex.csv"))
# DE.small.ex$nhdpls_ = as.character(DE.small.ex$nhdpls_)
# 
# DE.small = DE.flow %>%
#   filter(nhdpls_ %in% DE.small.ex$nhdpls_)
# 
# DE.small = DE.small %>%
#   filter(!nhdpls_ %in% c("932040213", "932040214", "24903566"))

# round to remove precision errors
st_geometry(DE.flow) <- DE.flow %>% 
  st_geometry() %>% 
  map(~round(., -1)) %>% 
  st_sfc(crs = st_crs(DE.flow))

DE.network = as_sfnetwork(DE.flow)

simple.graph <- DE.network %>%
  activate("edges") %>%
  arrange(edge_length()) %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())

n = length(DE.network)
edge.colors = function(x) rep(sf.colors(10, categorical = TRUE)[-2], n)[c(1:ecount(x))]

par(mfrow=c(1,2))
plot(st_geometry(DE.network, "edges"), col=edge.colors(DE.network), lwd=2)
plot(st_geometry(simple.graph, "edges"), col=edge.colors(simple.graph), lwd=2)
par(mfrow=c(1,1))

node.coords = simple.graph %>% activate("nodes") %>% st_coordinates()
clusters = dbscan(node.coords, eps = 150, minPts = 1)$cluster # original 100
clustered = simple.graph %>% activate("nodes") %>% mutate(cluster = clusters)
clustered = clustered %>% mutate(component = group_components())
contracted = convert(clustered, to_spatial_contracted, cluster, component, simplify=T)

par(mfrow=c(1,2))
plot(st_geometry(simple.graph, "edges"), col=edge.colors(simple.graph), lwd=2)
plot(st_geometry(contracted, "edges"), col=edge.colors(contracted), lwd=2)
par(mfrow=c(1,1))

# # remove non-cut edges
# connected = contracted %>%
#   mutate(is_cut = node_is_cut()) %>%
#   morph(to_linegraph) %>%
#   mutate(is_cut = node_is_cut()) %>%
#   unmorph() %>%
#   activate("edges") %>%
#   filter(!is_cut)

# DE.flow.clean.sf = DE.flow.clean %>% activate("edges") %>% st_as_sf()
# DE.flow.clean.sf = DE.flow.clean.sf %>% select("nhdpls_")

# flow.path <- paste0(path.root, "data/DE-basin-network")

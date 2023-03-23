################################################################################
lat <- 44.3470
lon <- -72.9965
min_gap <- 2  # minimum young forest acreage to be usable habitat
dist <- 100 # maximum distance (ft) between young areas to be lumped
################################################################################

cent <- centroid(lat, lon)
aoi <- aoi(centroid = cent)
ndsm <- get_ndsm(aoi)
clr <- get_clr(aoi)
landcov <- get_landcover(aoi)
canopy <- make_canopy(ndsm, landcov)
shrub <- make_shrub(canopy, size = min_gap, distance = dist)

map_canopy(canopy)
map_shrub(clr, landcov, shrub)
View(land_summary(landcov))

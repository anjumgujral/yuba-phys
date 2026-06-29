library(sf)
library(ggplot2)
library(terra)
library(elevatr)
library(ggnewscale)
library(spatialEco)
library(viridis)
library(dplyr)

# load site information 
site_boundaries <- data.frame(
  id = c("1","2","3","4","5","7","8","9","10","11"),
  lon = c(-121.051720,-121.046958,-121.006063,-121.000819,-121.004718,
          -120.829953,-120.780825,-120.786236,-120.778930,-120.826322),
  lat = c(39.491387,39.490947,39.487019,39.478789,39.472552,
          39.511469,39.504699,39.507502,39.507644,39.513851)
)

# assign elevation to field sites
site_boundaries$elevation <- NA
site_boundaries$elevation[site_boundaries$id %in% c("1","2")] <- "low"
site_boundaries$elevation[site_boundaries$id %in% c("3","4","5")] <- "mid"
site_boundaries$elevation[site_boundaries$id %in% c("7","8","9","10","11")] <- "high"

# convert site coordinates to sf object in WGS84 (EPSG:4326)
sites_sf <- st_as_sf(site_boundaries, coords = c("lon", "lat"), crs = 4326)

# define a bounding box around the north yuba 
bbox_sf <- st_as_sf(st_as_sfc(
  st_bbox(c(xmin=-121.2, xmax=-119.3, ymin=38.5, ymax=40.0), crs=4326)
))
small_box <- ext(-121.2, -119.3, 38.5, 40.0)

# download digital elevation model (DEM) for bounding box
dem <- get_elev_raster(locations = bbox_sf, z = 10, clip = "bbox")
dem <- rast(dem)
dem_crop <- crop(dem, small_box)

# compute terrain variables from DEM in 4326
slope_4326 <- terrain(dem_crop, v = "slope",  unit = "radians")
aspect_rad_4326 <- terrain(dem_crop, v = "aspect", unit = "radians")
aspect_deg_4326 <- terrain(dem_crop, v = "aspect", unit = "degrees")
hli_raster <- spatialEco::hli(dem_crop) # McCune & Dyke heat load index

# extract and compile site data (aspect, northness, slope, hli)
site_data <- data.frame(
  id = site_boundaries$id,
  lon = site_boundaries$lon,
  lat = site_boundaries$lat,
  elevation = site_boundaries$elevation,
  aspect = terra::extract(aspect_deg_4326, vect(sites_sf))[,2],
  northness = terra::extract(cos(aspect_rad_4326), vect(sites_sf))[,2],
  slope = terra::extract(slope_4326, vect(sites_sf))[,2],
  hli = terra::extract(hli_raster, vect(sites_sf))[,2]
)
write.csv(site_data, "site_data.csv", row.names = FALSE)

# load bcm climate data
bcm <- rast("bcm_1981_2010.tif")
names(bcm) <- c("cwd","pck","ppt","rch","run","str","tmn","tmx")

# convert site coordinates to terra vector object in 4326
sites_vect <- vect(site_boundaries, geom = c("lon","lat"), crs = "EPSG:4326")
# project sites to match bcm CRS (EPSG:3310, CA Albers)
sites_3310 <- project(sites_vect, crs(bcm))

# extract all bcm climate variables at each site
bcm_clim <- extract(bcm, sites_3310)
sites_climate <- bind_cols(site_boundaries, bcm_clim %>% select(-ID))

write.csv(sites_climate, "bcm_climate.csv", row.names = FALSE)

# download DEM and reproject to bcm CRS (3310) 
dem_bcm <- get_elev_raster(locations = bbox_sf, z = 10, clip = "bbox")
dem_bcm <- rast(dem_bcm)
dem_bcm <- project(dem_bcm, crs(bcm))

# define map extent from site coordinates with a 5km/5000m buffer 
north_yuba_ext <- ext(sites_3310)
north_yuba_ext <- extend(north_yuba_ext, 5000)

# crop DEM and bcm layers to plotting extent
dem_bcm_crop <- crop(dem_bcm, north_yuba_ext)
ppt_crop     <- crop(bcm[["ppt"]], north_yuba_ext)
cwd_crop     <- crop(bcm[["cwd"]], north_yuba_ext)

# compute terrain variables from reprojected DEM
slope_3310  <- terrain(dem_bcm_crop, v = "slope",  unit = "radians")
aspect_3310 <- terrain(dem_bcm_crop, v = "aspect", unit = "radians")
hill_3310   <- shade(slope_3310, aspect_3310, angle = 45, direction = 315)
north_3310  <- cos(aspect_3310)

# convert rasters to data frames from plotting
hill_df  <- as.data.frame(hill_3310,  xy = TRUE, na.rm = TRUE)
north_df <- as.data.frame(north_3310, xy = TRUE, na.rm = TRUE)
ppt_df   <- as.data.frame(ppt_crop,   xy = TRUE, na.rm = TRUE)
cwd_df   <- as.data.frame(cwd_crop,   xy = TRUE, na.rm = TRUE)
names(hill_df)[3]  <- "hill"
names(north_df)[3] <- "northness"

# convert site coordinates to sf object and set elevation as an ordered factor for plotting
sites_sf_3310 <- sf::st_as_sf(sites_3310)
sites_sf_3310$elevation <- factor(site_boundaries$elevation, levels = c("low","mid","high"))

# PPT map: hillshade + precipitation + northness + site points
ggplot() +
  geom_raster(data = hill_df,  aes(x = x, y = y, fill = hill)) +
  scale_fill_gradient(low = "black", high = "white", guide = "none") +
  ggnewscale::new_scale_fill() +
  geom_raster(data = ppt_df,   aes(x = x, y = y, fill = ppt)) +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "PPT (mm)") +
  ggnewscale::new_scale_fill() +
  geom_raster(data = north_df, aes(x = x, y = y, fill = northness), alpha = 0.25) +
  scale_fill_gradient2(low = "black", mid = "gray70", high = "white", midpoint = 0, name = "Northness") +
  geom_sf(data = sites_sf_3310, aes(shape = elevation), fill = "white", color = "black", size = 3) +
  scale_shape_manual(values = c(low = 21, mid = 22, high = 24)) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf() +
  theme_classic()

# CWD map: hillshade + climatic water deficit + northness + site points
ggplot() +
  geom_raster(data = hill_df,  aes(x = x, y = y, fill = hill)) +
  scale_fill_gradient(low = "black", high = "white", guide = "none") +
  ggnewscale::new_scale_fill() +
  geom_raster(data = cwd_df,   aes(x = x, y = y, fill = cwd)) +
  scale_fill_viridis_c(option = "viridis", direction = 1, limits = c(100, 1200), name = "CWD (mm)") +
  ggnewscale::new_scale_fill() +
  geom_raster(data = north_df, aes(x = x, y = y, fill = northness), alpha = 0.25) +
  scale_fill_gradient2(low = "black", mid = "gray70", high = "white", midpoint = 0, name = "Northness") +
  geom_sf(data = sites_sf_3310, aes(shape = elevation), fill = "white", color = "black", size = 3) +
  scale_shape_manual(values = c(low = 21, mid = 22, high = 24)) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf() +
  theme_classic()

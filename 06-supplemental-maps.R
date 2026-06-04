install.packages('AOI')
install.packages('climateR')
install.packages('terra')
install.packages('ggplot2')
install.packages('tmap')
install.packages('ggspatial')
install.packages('prettymapr')
install.packages('rnaturalearth')
install.packages("rnaturalearthdata")
install.packages('maps')
install.packages('elevatr')
install.packages('spatialEco')
install.packages('tidyterra')
install.packages('viridis')

library(tmap)
library(sf)
library(ggplot2)
library(terra)
library(ggspatial)
library(prettymapr)
library(maps)
library(elevatr)
library(ggnewscale)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)
library(nhdplusTools)
library(spatialEco)
library(tidyterra)
library(viridis)

# load site information 
california <- ne_states(country = "United States of America", returnclass = "sf")
california  <- california[california$name == "California", ]

site_boundaries <- data.frame(
  id  = c("1","2","3","4","5","7","8","9","10","11"),
  lon = c(-121.051720,-121.046958,-121.006063,-121.000819,-121.004718,
          -120.829953,-120.780825,-120.786236,-120.778930,-120.826322),
  lat = c(39.491387,39.490947,39.487019,39.478789,39.472552,
          39.511469,39.504699,39.507502,39.507644,39.513851)
)
site_boundaries$elevation <- NA
site_boundaries$elevation[site_boundaries$id %in% c("1","2")] <- "low"
site_boundaries$elevation[site_boundaries$id %in% c("3","4","5")] <- "mid"
site_boundaries$elevation[site_boundaries$id %in% c("7","8","9","10","11")] <- "high"

sites_sf <- st_as_sf(site_boundaries, coords = c("lon", "lat"), crs = 4326)

# create bounding boxes 
small_box <- ext(-121.2, -119.3, 38.5, 40.0)   # terra extent (for crop)

bbox_sf <- st_as_sf(st_as_sfc(                  
  st_bbox(c(xmin=-121.2, xmax=-119.3, ymin=38.5, ymax=40.0), crs=4326)
))

# DEM, hillsahde/shaded relief, and aspect 
# download DEM raster for our bounding box using 'elevatr' package 
dem <- get_elev_raster(locations = bbox_sf, z = 10, clip = "bbox")
dem <- rast(dem)
dem_crop <- crop(dem, small_box)

# compute terrain characteristics (slope and aspect) from pixels
slope <- terrain(dem_crop, v = "slope",  unit = "radians")
aspect_rad <- terrain(dem_crop, v = "aspect", unit = "radians")
aspect_deg <- terrain(dem_crop, v = "aspect", unit = "degrees")

# calculate hillshade (how light hits terrain based on slope and aspect)
hillshade <- shade(slope, aspect_rad, angle = 45, direction = 315) # sun elevation and sun azimuth 
hill_df <- as.data.frame(hillshade, xy = TRUE, na.rm = TRUE)
colnames(hill_df) <- c("lon", "lat", "shade")

hill_df$shade <- scales::rescale(hill_df$shade)
hill_df$shade <- hill_df$shade^0.4   # contrast boost
aspect_df <- as.data.frame(aspect_deg, xy = TRUE, na.rm = TRUE)
colnames(aspect_df) <- c("lon", "lat", "aspect")

northness <- cos(aspect_rad)

north_df <- as.data.frame(northness, xy = TRUE, na.rm = TRUE)
colnames(north_df) <- c("lon", "lat", "northness")

# load in raster for rivers
rivers_sf   <- ne_download(scale = 10, type = "rivers_lake_centerlines",
                           category = "physical", returnclass = "sf")
rivers_sf   <- st_transform(rivers_sf, 4326)
rivers_clip <- st_crop(rivers_sf, bbox_sf)

# Get NHD flowlines for your area
nhd_lines <- get_nhdplus(AOI = bbox_sf, realization = "flowline")
nhd_lines <- st_transform(nhd_lines, 4326)

# Then filter to only permanent streams to reduce clutter
nhd_lines <- nhd_lines[nhd_lines$ftype != 566, ]  # drop coastlines

# keep only major streams 
nhd_major <- nhd_lines[nhd_lines$streamorde >= 4, ]

# load in and crop climate rasters 
ppt_mean <- rast("ppt_30yr_mean.tif")
ppt_crop <- crop(ppt_mean, small_box)
ppt_df_map <- as.data.frame(ppt_crop, xy = TRUE, na.rm = TRUE)
colnames(ppt_df_map) <- c("lon", "lat", "ppt")

def_mean <- rast("def_30yr_mean.tif")
def_crop <- crop(def_mean, small_box)
def_df_map <- as.data.frame(def_crop, xy = TRUE, na.rm = TRUE)
colnames(def_df_map) <- c("lon", "lat", "def")

vpd_mean <- rast("vpd_30yr_mean.tif")
vpd_crop <- crop(vpd_mean, small_box)
vpd_df_map <- as.data.frame(vpd_crop, xy = TRUE, na.rm = TRUE)
colnames(vpd_df_map) <- c("lon", "lat", "vpd")

aet_mean <- rast("aet_30yr_mean.tif")
aet_crop <- crop(aet_mean, small_box)
aet_df_map <- as.data.frame(aet_crop, xy = TRUE, na.rm = TRUE)
colnames(aet_df_map) <- c("lon", "lat", "aet")

pet_mean <- rast("pet_30yr_mean.tif")
pet_crop <- crop(pet_mean, small_box)
pet_df_map <- as.data.frame(pet_crop, xy = TRUE, na.rm = TRUE)
colnames(pet_df_map) <- c("lon", "lat", "pet")

# maps 
HILLSHADE <- ggplot() +
  # hillshade background
  geom_raster(
    data = hill_df,
    aes(x = lon, y = lat, fill = shade)
  ) +
  scale_fill_gradient(
    low = "grey20",
    high = "white",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  # northness overlay
  geom_raster(
    data = north_df,
    aes(x = lon, y = lat, fill = northness),
    alpha = 0.40
  ) +
  scale_fill_gradient2(
    low = "black",
    mid = "grey70",
    high = "white",
    midpoint = 0,
    name = "Northness"
  ) +
  # streams
  geom_sf(
    data = nhd_major,
    color = "dodgerblue4",
    linewidth = 0.5
  ) +
  # field sites
  geom_sf(
    data = sites_sf,
    aes(shape = elevation),
    size = 3.5,
    fill = "plum",
    color = "black",
    stroke = 0.5
  ) +
  scale_shape_manual(
    values = c(
      "low"  = 21,
      "mid"  = 22,
      "high" = 24
    )
  ) +
  coord_sf(
    xlim = c(-121.08, -120.75),
    ylim = c(39.46, 39.53),
    expand = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

HILLSHADE

PPT <- ggplot() +
  # hillshade background
  geom_raster(
    data = hill_df,
    aes(x = lon, y = lat, fill = shade)
  ) +
  scale_fill_gradient(
    low = "grey20",
    high = "white",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  # northness overlay
  geom_raster(
    data = north_df,
    aes(x = lon, y = lat, fill = northness),
    alpha = 0.40
  ) +
  scale_fill_gradient2(
    low = "black",
    mid = "grey70",
    high = "white",
    midpoint = 0,
    name = "Northness"
  ) +
  ggnewscale::new_scale_fill() +
  # PPT overlay
  geom_raster(
    data = ppt_df_map,
    aes(x = lon, y = lat, fill = ppt),
    alpha = 0.50
  ) + scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    begin = 0.1,
    end = 0.95,
    limits = c(1100, 1800),
    oob = scales::squish,
    name = "PPT"
  ) +
  # streams
  geom_sf(
    data = nhd_major,
    color = "dodgerblue4",
    linewidth = 0.5
  ) +
  # field sites
  geom_sf(
    data = sites_sf,
    aes(shape = elevation),
    size = 3.5,
    color = "black",
    stroke = 0.6
  ) +
  scale_shape_manual(
    values = c(
      "low"  = 21,
      "mid"  = 22,
      "high" = 24
    )
  ) +
  coord_sf(
    xlim = c(-121.12, -120.72),
    ylim = c(39.44, 39.55),
    expand = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
PPT

CWD <- ggplot() +
  geom_raster(
    data = hill_df,
    aes(x = lon, y = lat, fill = shade)
  ) +
  scale_fill_gradient(
    low = "grey20",
    high = "white",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(
    data = north_df,
    aes(x = lon, y = lat, fill = northness),
    alpha = 0.40
  ) +
  scale_fill_gradient2(
    low = "black",
    mid = "grey70",
    high = "white",
    midpoint = 0,
    name = "Northness"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(
    data = def_df_map,
    aes(x = lon, y = lat, fill = def),
    alpha = 0.50
  ) +
  scale_fill_viridis_c(
    option = "magma",
    begin = 0.1,
    end = 0.95,
    limits = c(300, 700),
    oob = scales::squish,
    name = "CWD"
  ) +
  geom_sf(
    data = nhd_major,
    color = "dodgerblue4",
    linewidth = 0.5
  ) +
  geom_sf(
    data = sites_sf,
    aes(shape = elevation),
    size = 3.5,
    color = "black",
    stroke = 0.6
  ) +
  scale_shape_manual(
    values = c("low" = 21,
               "mid" = 22,
               "high" = 24)
  ) +
  coord_sf(
    xlim = c(-121.12, -120.72),
    ylim = c(39.44, 39.55),
    expand = FALSE
  ) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
CWD

VPD <- ggplot() +
  geom_raster(
    data = hill_df,
    aes(x = lon, y = lat, fill = shade)
  ) +
  scale_fill_gradient(
    low = "grey20",
    high = "white",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(
    data = north_df,
    aes(x = lon, y = lat, fill = northness),
    alpha = 0.40
  ) +
  scale_fill_gradient2(
    low = "black",
    mid = "grey70",
    high = "white",
    midpoint = 0,
    name = "Northness"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(
    data = vpd_df_map,
    aes(x = lon, y = lat, fill = vpd),
    alpha = 0.50
  ) +
  scale_fill_viridis_c(
    option = "magma",
    begin = 0.1,
    end = 0.95,
    limits = c(0.7, 1.1),
    oob = scales::squish,
    name = "VPD"
  ) +
  geom_sf(
    data = nhd_major,
    color = "dodgerblue4",
    linewidth = 0.5
  ) +
  geom_sf(
    data = sites_sf,
    aes(shape = elevation),
    size = 3.5,
    color = "black",
    stroke = 0.6
  ) +
  scale_shape_manual(
    values = c("low" = 21,
               "mid" = 22,
               "high" = 24)
  ) +
  coord_sf(
    xlim = c(-121.12, -120.72),
    ylim = c(39.44, 39.55),
    expand = FALSE
  ) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
VPD

PET <- ggplot() +
  geom_raster(
    data = hill_df,
    aes(x = lon, y = lat, fill = shade)
  ) +
  scale_fill_gradient(
    low = "grey20",
    high = "white",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(
    data = north_df,
    aes(x = lon, y = lat, fill = northness),
    alpha = 0.40
  ) +
  scale_fill_gradient2(
    low = "black",
    mid = "grey70",
    high = "white",
    midpoint = 0,
    name = "Northness"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(
    data = pet_df_map,
    aes(x = lon, y = lat, fill = pet),
    alpha = 0.50
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = 1,
    begin = 0.1,
    end = 0.95,
    limits = c(1000, 1400),
    oob = scales::squish,
    name = "PET"
  ) +
  geom_sf(
    data = nhd_major,
    color = "dodgerblue4",
    linewidth = 0.5
  ) +
  geom_sf(
    data = sites_sf,
    aes(shape = elevation),
    size = 3.5,
    color = "black",
    stroke = 0.6
  ) +
  scale_shape_manual(
    values = c("low" = 21,
               "mid" = 22,
               "high" = 24)
  ) +
  coord_sf(
    xlim = c(-121.12, -120.72),
    ylim = c(39.44, 39.55),
    expand = FALSE
  ) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

PET

AET <- ggplot() +
  geom_raster(
    data = hill_df,
    aes(x = lon, y = lat, fill = shade)
  ) +
  scale_fill_gradient(
    low = "grey20",
    high = "white",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(
    data = north_df,
    aes(x = lon, y = lat, fill = northness),
    alpha = 0.40
  ) +
  scale_fill_gradient2(
    low = "black",
    mid = "grey70",
    high = "white",
    midpoint = 0,
    name = "Northness"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(
    data = aet_df_map,
    aes(x = lon, y = lat, fill = aet),
    alpha = 0.50
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = 1,
    begin = 0.1,
    end = 0.95,
    limits = c(600,800),
    oob = scales::squish,
    name = "AET"
  ) +
  geom_sf(
    data = nhd_major,
    color = "dodgerblue4",
    linewidth = 0.5
  ) +
  geom_sf(
    data = sites_sf,
    aes(shape = elevation),
    size = 3.5,
    color = "black",
    stroke = 0.6
  ) +
  scale_shape_manual(
    values = c("low" = 21,
               "mid" = 22,
               "high" = 24)
  ) +
  coord_sf(
    xlim = c(-121.12, -120.72),
    ylim = c(39.44, 39.55),
    expand = FALSE
  ) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
AET


# Heat load index, aspect adjusted climate
# aspect is hard to visualize with climate but lets add a data table with climate
# extracted by aspect
aspect_deg_r <- terrain(dem_crop, v = "aspect", unit = "degrees")

# aspect can be boiled down to northness and eastness
northness <- cos(aspect_rad)   # 1 = true north, -1 = true south
eastness<- sin(aspect_rad)   # 1 = true east,  -1 = true west
northness_df <- as.data.frame(northness, xy = TRUE, na.rm = TRUE)
colnames(northness_df) <- c("lon", "lat", "northness")

# extract aspect, northness, and eastness for each site
sites_sf$aspect <- terra::extract(aspect_deg, vect(sites_sf))[,2]
sites_sf$northness<- terra::extract(northness, vect(sites_sf))[,2]
sites_sf$eastness <- terra::extract(eastness, vect(sites_sf))[,2]
sites_sf$slope <- terra::extract(slope, vect(sites_sf))[,2]

# Extract climate values at each site
sites_sf$ppt <- terra::extract(ppt_crop, vect(sites_sf))[,2]
sites_sf$def <- terra::extract(def_crop, vect(sites_sf))[,2]
sites_sf$vpd <- terra::extract(vpd_crop, vect(sites_sf))[,2]
sites_sf$aet <- terra::extract(aet_crop, vect(sites_sf))[,2]
sites_sf$pet <- terra::extract(pet_crop, vect(sites_sf))[,2]

# McCune & Dyke heat load index 
# i.e. the relative potential direct incident solar radiation on a slope
# determined from slope, aspect, and latitude

# Calculate HLI directly from DEM 
hli_raster <- spatialEco::hli(dem_crop)
sites_sf$hli <- terra::extract(hli_raster, vect(sites_sf))[,2]
  
write.csv(sites_sf, "site_data.csv", row.names = FALSE)

### Basin Characterization Model workflow ###

#load in BCM data 
bcm <- rast("clean_climate_1981_2010.tif")

names(bcm) <- c(
  "cwd",
  "pck",
  "ppt",
  "rch",
  "run",
  "str",
  "tmn",
  "tmx"
)

sites_vect <- vect(
  site_boundaries,  
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

sites_3310 <- project(sites_vect, crs(bcm))

bcm_clim <- extract(bcm, sites_3310)

sites_climate <- bind_cols(
  site_boundaries,
  bcm_clim %>% select(-ID)
)


sites_sf <- st_as_sf(
  site_boundaries,
  coords = c("lon", "lat"),
  crs = 4326
)

dem <- get_elev_raster(
  sites_sf,
  z = 10
)

dem <- rast(dem)
dem <- project(dem, crs(bcm))

# extent of all sites
north_yuba_ext <- ext(sites_3310)

# add 5 km buffer on all sides
north_yuba_ext <- extend(north_yuba_ext, 5000)

north_yuba_ext


dem_crop <- crop(dem, north_yuba_ext)

cwd_crop <- crop(bcm[["cwd"]], north_yuba_ext)

tmx_crop <- crop(bcm[["tmx"]], north_yuba_ext)

ppt_crop <- crop(bcm[["ppt"]], north_yuba_ext)

slope <- terrain(
  dem_crop,
  v = "slope",
  unit = "radians"
)

aspect <- terrain(
  dem_crop,
  v = "aspect",
  unit = "radians"
)

hill <- shade(
  slope,
  aspect,
  angle = 45,
  direction = 315
)


ppt_df <- as.data.frame(ppt_crop, xy = TRUE, na.rm = TRUE)
cwd_df <- as.data.frame(cwd_crop, xy = TRUE, na.rm = TRUE)
dem_df <- as.data.frame(dem_crop, xy = TRUE)
hill_df <- as.data.frame(hill, xy = TRUE, na.rm = TRUE)

names(dem_df)[3] <- "elev"
names(hill_df)[3] <- "hill"

sites_sf <- sf::st_as_sf(sites_3310)

sites_sf$elevation <- factor(
  site_boundaries$elevation,
  levels = c("low", "mid", "high")
)

northness <- cos(aspect)
north_df <- as.data.frame(
  northness,
  xy = TRUE,
  na.rm = TRUE
)

names(north_df)[3] <- "northness"
ggplot() +
  # hillshade first
  geom_raster(
    data = hill_df,
    aes(x = x, y = y, fill = hill)
  ) +
  scale_fill_gradient(
    low = "black",
    high = "white",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  # BCM precipitation on top
  geom_raster(
    data = ppt_df,
    aes(x = x, y = y, fill = ppt),
    alpha = 0.65
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    name = "Mean annual precipitation (mm)"
  ) +
  geom_sf(
    data = sites_sf,
    aes(shape = elevation),
    fill = "white",
    color = "black",
    size = 3
  ) +
  
  scale_shape_manual(
    name = "elevation",
    values = c(
      low = 21,
      mid = 22,
      high = 24
    )
  ) +
  labs(x = "Longitude",
       y = "Latitude") +
  coord_sf() +
  theme_classic()


## plotting ppt with northness
ggplot() +
  
  # hillshade
  geom_raster(
    data = hill_df,
    aes(x = x, y = y, fill = hill)
  ) +
  scale_fill_gradient(
    low = "black",
    high = "white",
    guide = "none"
  ) +
  
  ggnewscale::new_scale_fill() +
  
  # precipitation
  geom_raster(
    data = ppt_df,
    aes(x = x, y = y, fill = ppt)
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    name = "MAP (mm)"
  ) +
  
  ggnewscale::new_scale_fill() +
  
  # northness overlay
  geom_raster(
    data = north_df,
    aes(x = x, y = y, fill = northness),
    alpha = 0.25
  ) +
  
  scale_fill_gradient2(
    low = "black",
    mid = "gray70",
    high = "white",
    midpoint = 0,
    name = "Northness"
  ) +
  
  geom_sf(
    data = sites_sf,
    aes(shape = elevation),
    fill = "white",
    color = "black",
    size = 3
  ) +
  
  scale_shape_manual(
    values = c(
      low = 21,
      mid = 22,
      high = 24
    )
  ) +
  labs(x = "Longitude",
       y = "Latitude") +
  coord_sf() +
  theme_classic()

## plotting cwd with northness
ggplot() +
  
  # hillshade
  geom_raster(
    data = hill_df,
    aes(x = x, y = y, fill = hill)
  ) +
  scale_fill_gradient(
    low = "black",
    high = "white",
    guide = "none"
  ) +
  
  ggnewscale::new_scale_fill() +
  
  # precipitation
  geom_raster(
    data = cwd_df,
    aes(x = x, y = y, fill = cwd)
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    direction = 1,
    limits = c(100, 1200),
    name = "CWD (mm)"
  ) +
  
  ggnewscale::new_scale_fill() +
  
  # northness overlay
  geom_raster(
    data = north_df,
    aes(x = x, y = y, fill = northness),
    alpha = 0.25
  ) +
  
  scale_fill_gradient2(
    low = "black",
    mid = "gray70",
    high = "white",
    midpoint = 0,
    name = "Northness"
  ) +
  
  geom_sf(
    data = sites_sf,
    aes(shape = elevation),
    fill = "white",
    color = "black",
    size = 3
  ) +
  
  scale_shape_manual(
    values = c(
      low = 21,
      mid = 22,
      high = 24
    )
  ) +
  labs(x = "Longitude",
       y = "Latitude") +
  coord_sf() +
  theme_classic()

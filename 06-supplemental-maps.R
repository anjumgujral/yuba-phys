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

library(tmap)
library(AOI)
library(sf)
library(climateR)
library(ggplot2)
library(terra)
library(ggspatial)
library(prettymapr)
library(maps)
library(elevatr)
library(ggnewscale)

library(sf)
library(terra)
library(elevatr)
library(ggplot2)
library(ggnewscale)
library(scales)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(nhdplusTools)

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
site_boundaries$elevation[site_boundaries$id %in% c("1","2")]          <- "low"
site_boundaries$elevation[site_boundaries$id %in% c("3","4","5")]      <- "mid"
site_boundaries$elevation[site_boundaries$id %in% c("7","8","9","10","11")] <- "high"

sites_sf <- st_as_sf(site_boundaries, coords = c("lon", "lat"), crs = 4326)

# create bounding boxes 

small_box <- ext(-121.2, -119.3, 38.5, 40.0)   # terra extent (for crop)

bbox_sf <- st_as_sf(st_as_sfc(                  
  st_bbox(c(xmin=-121.2, xmax=-119.3, ymin=38.5, ymax=40.0), crs=4326)
))

# DEM, hillsahde/shaded relief, and aspect 

dem <- get_elev_raster(locations = bbox_sf, z = 10, clip = "bbox")
dem <- rast(dem)

dem_crop   <- crop(dem, small_box)
slope      <- terrain(dem_crop, v = "slope",  unit = "radians")
aspect_rad <- terrain(dem_crop, v = "aspect", unit = "radians")
aspect_deg <- terrain(dem_crop, v = "aspect", unit = "degrees")

hillshade  <- shade(slope, aspect_rad, angle = 45, direction = 315)

hill_df <- as.data.frame(hillshade, xy = TRUE, na.rm = TRUE)
colnames(hill_df) <- c("lon", "lat", "shade")
hill_df$shade <- scales::rescale(hill_df$shade)
hill_df$shade <- hill_df$shade^0.4   # contrast boost

aspect_df <- as.data.frame(aspect_deg, xy = TRUE, na.rm = TRUE)
colnames(aspect_df) <- c("lon", "lat", "aspect")

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

# keep only major streams — adjust threshold to taste
nhd_major <- nhd_lines[nhd_lines$streamorde >= 4, ]

### load in and crop climate rasters ###

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

# PPT
PPT <- ggplot() +
  geom_tile(data = hill_df, aes(x = lon, y = lat, fill = shade), na.rm = TRUE) +
  scale_fill_gradient(low = "grey25", high = "grey95", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_tile(data = aspect_df, aes(x = lon, y = lat, fill = aspect),
            alpha = 0.8, na.rm = TRUE) +
  scale_fill_gradientn(
    colors = c("#bf812d", "#f6e8c3", "#c7eae5", "#35978f", "#c7eae5", "#f6e8c3", "#bf812d"),
    values = scales::rescale(c(0, 90, 180, 270, 360)),
    limits = c(0, 360), name = "Aspect (°)",
    breaks = c(0, 90, 180, 270, 360), labels = c("N", "E", "S", "W", "N"),
    guide  = guide_colorbar(override.aes = list(alpha = 0.25))
  ) +
  new_scale_fill() +
  geom_tile(data = ppt_df_map, aes(x = lon, y = lat, fill = ppt),
            alpha = 0.55, na.rm = TRUE) +
  scale_fill_viridis_c(
    option = "magma", begin = 0.1, end = 0.95, name = "PPT",
    limits = c(min(ppt_df_map$ppt, na.rm = TRUE), max(ppt_df_map$ppt, na.rm = TRUE)),
    guide  = guide_colorbar(override.aes = list(alpha = 0.55))
  ) +
  geom_sf(data = nhd_major, color = "black", linewidth = 0.6) + 
  geom_sf(data = sites_sf, aes(shape = elevation), size = 3, fill = NA, color = "black") +
  scale_shape_manual(values = c("low" = 21, "mid" = 22, "high" = 24)) +
  coord_sf(xlim = c(min(hill_df$lon), -120.6), ylim = c(39.3, 39.6), expand = FALSE) +
  theme_minimal()
PPT

# CWD
CWD <- ggplot() +
  geom_tile(data = hill_df, aes(x = lon, y = lat, fill = shade), na.rm = TRUE) +
  scale_fill_gradient(low = "grey25", high = "grey95", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_tile(data = aspect_df, aes(x = lon, y = lat, fill = aspect),
            alpha = 0.8, na.rm = TRUE) +
  scale_fill_gradientn(
    colors = c("#bf812d", "#f6e8c3", "#c7eae5", "#35978f", "#c7eae5", "#f6e8c3", "#bf812d"),
    values = scales::rescale(c(0, 90, 180, 270, 360)),
    limits = c(0, 360), name = "Aspect (°)",
    breaks = c(0, 90, 180, 270, 360), labels = c("N", "E", "S", "W", "N"),
    guide  = guide_colorbar(override.aes = list(alpha = 0.25))
  ) +
  new_scale_fill() +
  geom_tile(data = def_df_map, aes(x = lon, y = lat, fill = def),
            alpha = 0.55, na.rm = TRUE) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, begin = 0.1, end = 0.95, name = "CWD",
    limits = c(min(def_df_map$def, na.rm = TRUE), max(def_df_map$def, na.rm = TRUE)),
    guide  = guide_colorbar(override.aes = list(alpha = 0.55))
  ) +
  geom_sf(data = nhd_major, color = "black", linewidth = 0.6) + 
  geom_sf(data = sites_sf, aes(shape = elevation), size = 3, fill = NA, color = "black") +
  scale_shape_manual(values = c("low" = 21, "mid" = 22, "high" = 24)) +
  coord_sf(xlim = c(min(hill_df$lon), -120.6), ylim = c(39.3, 39.6), expand = FALSE) +
  theme_minimal()
CWD

# VPD
VPD <- ggplot() +
  geom_tile(data = hill_df, aes(x = lon, y = lat, fill = shade), na.rm = TRUE) +
  scale_fill_gradient(low = "grey25", high = "grey95", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_tile(data = aspect_df, aes(x = lon, y = lat, fill = aspect),
            alpha = 0.8, na.rm = TRUE) +
  scale_fill_gradientn(
    colors = c("#bf812d", "#f6e8c3", "#c7eae5", "#35978f", "#c7eae5", "#f6e8c3", "#bf812d"),
    values = scales::rescale(c(0, 90, 180, 270, 360)),
    limits = c(0, 360), name = "Aspect (°)",
    breaks = c(0, 90, 180, 270, 360), labels = c("N", "E", "S", "W", "N"),
    guide  = guide_colorbar(override.aes = list(alpha = 0.25))
  ) +
  new_scale_fill() +
  geom_tile(data = vpd_df_map, aes(x = lon, y = lat, fill = vpd),
            alpha = 0.55, na.rm = TRUE) +
  scale_fill_viridis_c(
    option = "magma", begin = 0.1, end = 0.95, name = "VPD",
    limits = c(min(vpd_df_map$vpd, na.rm = TRUE), max(vpd_df_map$vpd, na.rm = TRUE)),
    guide  = guide_colorbar(override.aes = list(alpha = 0.55))
  ) +
  geom_sf(data = nhd_major, color = "black", linewidth = 0.6) + 
  geom_sf(data = sites_sf, aes(shape = elevation), size = 3, fill = NA, color = "black") +
  scale_shape_manual(values = c("low" = 21, "mid" = 22, "high" = 24)) +
  coord_sf(xlim = c(min(hill_df$lon), -120.6), ylim = c(39.3, 39.6), expand = FALSE) +
  theme_minimal()
VPD

# AET
AET <- ggplot() +
  geom_tile(data = hill_df, aes(x = lon, y = lat, fill = shade), na.rm = TRUE) +
  scale_fill_gradient(low = "grey25", high = "grey95", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_tile(data = aspect_df, aes(x = lon, y = lat, fill = aspect),
            alpha = 0.8, na.rm = TRUE) +
  scale_fill_gradientn(
    colors = c("#bf812d", "#f6e8c3", "#c7eae5", "#35978f", "#c7eae5", "#f6e8c3", "#bf812d"),
    values = scales::rescale(c(0, 90, 180, 270, 360)),
    limits = c(0, 360), name = "Aspect (°)",
    breaks = c(0, 90, 180, 270, 360), labels = c("N", "E", "S", "W", "N"),
    guide  = guide_colorbar(override.aes = list(alpha = 0.25))
  ) +
  new_scale_fill() +
  geom_tile(data = aet_df_map, aes(x = lon, y = lat, fill = aet),
            alpha = 0.55, na.rm = TRUE) +
  scale_fill_viridis_c(
    option = "magma", begin = 0.1, end = 0.95, name = "AET",
    limits = c(min(aet_df_map$aet, na.rm = TRUE), max(aet_df_map$aet, na.rm = TRUE)),
    guide  = guide_colorbar(override.aes = list(alpha = 0.55))
  ) +
  geom_sf(data = nhd_major, color = "black", linewidth = 0.6) + 
  geom_sf(data = sites_sf, aes(shape = elevation), size = 3, fill = NA, color = "black") +
  scale_shape_manual(values = c("low" = 21, "mid" = 22, "high" = 24)) +
  coord_sf(xlim = c(min(hill_df$lon), -120.6), ylim = c(39.3, 39.6), expand = FALSE) +
  theme_minimal()
AET

#PET
PET <- ggplot() +
  geom_tile(data = hill_df, aes(x = lon, y = lat, fill = shade), na.rm = TRUE) +
  scale_fill_gradient(low = "grey25", high = "grey95", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_tile(data = aspect_df, aes(x = lon, y = lat, fill = aspect),
            alpha = 0.8, na.rm = TRUE) +
  scale_fill_gradientn(
    colors = c("#bf812d", "#f6e8c3", "#c7eae5", "#35978f", "#c7eae5", "#f6e8c3", "#bf812d"),
    values = scales::rescale(c(0, 90, 180, 270, 360)),
    limits = c(0, 360), name = "Aspect (°)",
    breaks = c(0, 90, 180, 270, 360), labels = c("N", "E", "S", "W", "N"),
    guide  = guide_colorbar(override.aes = list(alpha = 0.25))
  ) +
  new_scale_fill() +
  geom_tile(data = pet_df_map, aes(x = lon, y = lat, fill = pet),
            alpha = 0.55, na.rm = TRUE) +
  scale_fill_viridis_c(
    option = "magma", begin = 0.1, end = 0.95, name = "PET",
    limits = c(min(pet_df_map$pet, na.rm = TRUE), max(pet_df_map$pet, na.rm = TRUE)),
    guide  = guide_colorbar(override.aes = list(alpha = 0.55))
  ) +
  geom_sf(data = nhd_major, color = "black", linewidth = 0.6) + 
  geom_sf(data = sites_sf, aes(shape = elevation), size = 3, fill = NA, color = "black") +
  scale_shape_manual(values = c("low" = 21, "mid" = 22, "high" = 24)) +
  coord_sf(xlim = c(min(hill_df$lon), -120.6), ylim = c(39.3, 39.6), expand = FALSE) +
  theme_minimal()
PET

# Heat load index, aspect adjusted climate
# aspect is hard to visualize with climate but lets add a data table with climate
# extracted by aspect
aspect_deg_r <- terrain(dem_crop, v = "aspect", unit = "degrees")
northness <- cos(aspect_rad)   # 1 = true north, -1 = true south
eastness<- sin(aspect_rad)   # 1 = true east,  -1 = true west

northness_df <- as.data.frame(northness, xy = TRUE, na.rm = TRUE)
colnames(northness_df) <- c("lon", "lat", "northness")

# Extract aspect at each site
sites_sf$aspect <- terra::extract(aspect_deg, vect(sites_sf))[,2]
sites_sf$northness<- terra::extract(northness, vect(sites_sf))[,2]
sites_sf$eastness <- terra::extract(eastness, vect(sites_sf))[,2]


# Pull out as plain data frame for modeling
site_data <- st_drop_geometry(sites_sf)

# McCune & Dyke heat load index — common in Sierra Nevada ecology
site_data <- site_data |>
  mutate(
    lat_rad    = 39.49 * pi / 180,       # your approximate latitude
    aspect_rad = aspect * pi / 180,
    slope_rad  = extract(slope, vect(sites_sf))[,2],  # add slope extraction above
    hli = exp(-1.467 +
                1.582 * cos(lat_rad) * cos(slope_rad) -
                1.5   * cos(aspect_rad) * sin(slope_rad) * sin(lat_rad) -
                0.262 * sin(lat_rad) * sin(slope_rad) +
                0.607 * sin(aspect_rad) * sin(slope_rad))
  )

# lets use extract climate based on topographic position and aspect (HLI)
# extract slope at each site 
sites_sf$slope <- extract(slope, vect(sites_sf))[,2]  # slope in radians

# Heat Load Index (McCune & Dyke 2002) — aspect-adjusted insolation
sites_sf <- sites_sf |>
  mutate(
    lat_rad = 39.49 * pi / 180,   # your study area latitude
    hli = exp(-1.467 +
                1.582 * cos(lat_rad) * cos(slope) -
                1.500 * cos(aspect * pi/180) * sin(slope) * sin(lat_rad) -
                0.262 * sin(lat_rad) * sin(slope) +
                0.607 * sin(aspect * pi/180) * sin(slope))
  )

# use HLI to scale climate 
sites_sf <- sites_sf |>
  mutate(pet_adjusted = pet * hli)

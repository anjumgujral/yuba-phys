install.packages('AOI')
library('AOI')
install.packages('climateR')
library(climateR)
install.packages('terra')
library(terra)
install.packages('tmap')
library(tmap)

library(sf)
install.packages('ggplot2')
library(ggplot2)
install.packages('ggspatial')
library(ggspatial)
install.packages("prettymapr")
library(prettymapr)
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")


calbound = AOI::aoi_get(state = "CA")
small_box <- aoi_ext(xy = c(-121.2, 38.5, -119.3, 40.0))
options(climateR.future = FALSE)

ppt_2023 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2023.nc")
tmax_2023 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_tmax_2023.nc")
tmin_2023 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_tmin_2023.nc")
vpd_2023 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2023.nc")
def_2023 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2023.nc")

ppt_crop <- crop(ppt_2023, ext(-121.2, -119.3, 38.5, 40.0))
tmax_crop <- crop(tmax_2023, ext(-121.2, -119.3, 38.5, 40.0))
tmin_crop <- crop(tmin_2023, ext(-121.2, -119.3, 38.5, 40.0))
vpd_crop <- crop(vpd_2023, ext(-121.2, -119.3, 38.5, 40.0))
def_crop <- crop(def_2023, ext(-121.2, -119.3, 38.5, 40.0))

# load in exact population locations 
site_boundaries <- data.frame(
  id = c("1","2","3", "4", "5", "7", "8", "9", "10", "11"),
  lon = c(-121.051720, -121.046958, -121.006063, -121.000819, -121.004718, -120.829953, -120.780825, -120.786236, -120.778930, -120.826322),
  lat = c(39.491387, 39.490947, 39.487019, 39.478789, 39.472552, 39.511469, 39.504699, 39.507502, 39.507644, 39.513851)
)

sites <- vect(site_boundaries, geom = c("lon", "lat"), crs = "EPSG:4326")
sites_projection <- project(sites, "EPSG:32610")
buffer <- buffer(sites_projection, width = 304.8)
buffer_projection <- project(buffer, "EPSG:4326")

# run the following code for all variables
# ppt
buffer_projection <- project(buffer, crs(ppt_2023))
ppt <- extract(ppt_2023, buffer_projection, fun = mean, na.rm = TRUE)
climate_extraction <- cbind(site_boundaries, ppt)

climate_extraction$ppt_annual <- rowSums(
  climate_extraction[, grep("ppt_", names(climate_extraction))],
  na.rm = TRUE
)

sites_sf <- vect(climate_extraction,
                 geom = c("lon", "lat"),
                 crs = "EPSG:4326")
sites_sf$ppt_annual <- climate_extraction$ppt_annual

sites_sf <- st_as_sf(site_boundaries, coords = c("lon","lat"), crs = 4326)

california <- ne_states(country = "united states of america", returnclass = "sf")

sites_sf$ppt_annual <- climate_extraction$ppt_annual

C1 <- ggplot() + geom_sf(data = california, fill = "gray95", 
                   color = "white") + 
  geom_sf(data = sites_sf, aes(color = ppt_annual), size = 3) + 
  scale_color_viridis_c() + 
  coord_sf(xlim = c(-121.3, -120.6), ylim = c(39.3, 39.6)) + 
  theme_minimal() + labs(color = "Annual PPT (mm)")
C1
# def

# project buffers to match raster CRS
buffer_projection <- project(buffer, crs(def_2023))

# extract DEF (monthly)
def <- extract(def_2023, buffer_projection, fun = mean, na.rm = TRUE)

# combine with site data
climate_extraction <- cbind(site_boundaries, def)

# annual mean DEF across months (or use rowSums if you prefer total)
climate_extraction$def_annual <- rowMeans(
  climate_extraction[, grep("def_", names(climate_extraction))],
  na.rm = TRUE
)

# create sf object (keep consistent!)
sites_sf <- sf::st_as_sf(
  climate_extraction,
  coords = c("lon", "lat"),
  crs = 4326
)

C2 <- ggplot() +
  geom_sf(data = california, fill = "gray95", color = "white") +
  geom_sf(data = sites_sf, aes(color = def_annual), size = 3) +
  scale_color_viridis_c(option = "C") +
  coord_sf(
    xlim = c(-121.3, -120.6),
    ylim = c(39.3, 39.6)
  ) +
  theme_minimal() +
  labs(color = "Annual DEF (mm)")

C2

# tmax

# project buffers to match raster CRS
buffer_projection_tmax <- project(buffer, crs(tmax_2023))

# extract TMAX (monthly)
tmax <- extract(tmax_2023, buffer_projection, fun = mean, na.rm = TRUE)

# combine with site data
climate_extraction <- cbind(site_boundaries, tmax)

# annual mean TMAX across months
climate_extraction$tmax_annual <- rowMeans(
  climate_extraction[, grep("tmax_", names(climate_extraction))],
  na.rm = TRUE
)

# create sf object (keep consistent!)
sites_sf <- sf::st_as_sf(
  climate_extraction,
  coords = c("lon", "lat"),
  crs = 4326
)

# plot
C3 <- ggplot() +
  geom_sf(data = california, fill = "gray95", color = "white") +
  geom_sf(data = sites_sf, aes(color = tmax_annual), size = 3) +
  scale_color_viridis_c(option = "C") +
  coord_sf(
    xlim = c(-121.3, -120.6),
    ylim = c(39.3, 39.6)
  ) +
  theme_minimal() +
  labs(color = "Annual Tmax (°C)")

C3


# vpd


# project buffers to match raster CRS
buffer_projection <- project(buffer, crs(vpd_2023))

# extract VPD (monthly)
vpd <- extract(vpd_2023, buffer_projection, fun = mean, na.rm = TRUE)

# combine with site data
climate_extraction <- cbind(site_boundaries, vpd)

# annual mean VPD across months
climate_extraction$vpd_annual <- rowMeans(
  climate_extraction[, grep("vpd_", names(climate_extraction))],
  na.rm = TRUE
)

# create sf object (keep consistent!)
sites_sf <- sf::st_as_sf(
  climate_extraction,
  coords = c("lon", "lat"),
  crs = 4326
)

# plot
C4 <- ggplot() +
  geom_sf(data = california, fill = "gray95", color = "white") +
  geom_sf(data = sites_sf, aes(color = vpd_annual), size = 3) +
  scale_color_viridis_c(option = "C") +
  coord_sf(
    xlim = c(-121.3, -120.6),
    ylim = c(39.3, 39.6)
  ) +
  theme_minimal() +
  labs(color = "Annual VPD (kPa)")

C4



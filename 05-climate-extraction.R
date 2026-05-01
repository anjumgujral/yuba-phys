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

# load california boundary
california <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
california <- california[california$ID == "california", ]

# create a bounding box for the north yuba
small_box <- ext(-121.2, -119.3, 38.5, 40.0)

# Make an sf bbox with explicit CRS for elevatr and cropping
bbox_sf <- st_as_sfc(
  st_bbox(c(xmin = -121.2, xmax = -119.3, ymin = 38.5, ymax = 40.0),
          crs = 4326)
)
# load in site locations 
site_boundaries <- data.frame(
  id = c("1","2","3", "4", "5", "7", "8", "9", "10", "11"),
  lon = c(-121.051720, -121.046958, -121.006063, -121.000819, -121.004718, -120.829953, -120.780825, -120.786236, -120.778930, -120.826322),
  lat = c(39.491387, 39.490947, 39.487019, 39.478789, 39.472552, 39.511469, 39.504699, 39.507502, 39.507644, 39.513851)
)

# assign elevation to sites for plotting purposes
site_boundaries$elevation <- NA
site_boundaries$elevation[site_boundaries$id %in% c("1","2")] <- "low"
site_boundaries$elevation[site_boundaries$id %in% c("3","4","5")] <- "mid"
site_boundaries$elevation[site_boundaries$id %in% c("7","8","9","10","11")] <- "high"

# convert site table into spatial data points for extracting climate and plotting sites on map
sites_sf <- st_as_sf(site_boundaries, coords = c("lon", "lat"), crs = 4326)
dem_crop  <- crop(dem, small_box)
slope     <- terrain(dem_crop, v = "slope",  unit = "radians")
aspect    <- terrain(dem_crop, v = "aspect", unit = "radians")
hillshade <- shade(slope, aspect, angle = 45, direction = 315)

hill_df        <- as.data.frame(hillshade, xy = TRUE, na.rm = TRUE)  # FIX 1: plain function call
colnames(hill_df) <- c("lon","lat","shade")
hill_df$shade  <- scales::rescale(hill_df$shade)
hill_df$shade  <- hill_df$shade^0.4   # contrast boost

rivers_sf   <- ne_download(scale = 10, type = "rivers_lake_centerlines",
                           category = "physical", returnclass = "sf")
rivers_sf   <- st_transform(rivers_sf, 4326)
rivers_clip <- st_crop(rivers_sf, bbox_sf)


library(rnaturalearth)
rivers_sf <- ne_download(scale = 10,
                         type = "rivers_lake_centerlines",
                         category = "physical",
                         returnclass = "sf")
rivers_sf <- st_transform(rivers_sf, 4326)
rivers_clip <- st_crop(rivers_sf, bbox_sf)
rivers_clip <- st_transform(rivers_clip, 4326)


# download 30 year climate normals for ppt
years <- 1994:2023

ppt_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_", years, ".nc")
)

# calculate total annual precip for each year by taking monthly precip and grouping it by 12 months and summing
# precip across those 12 months. Then create a raster for each year, and stack 30 years 
ppt_annual_by_year <- tapp(
  ppt_stack_stable,
  index = rep(1:30, each = 12),
  fun = sum,
  na.rm = TRUE
)
# take the average of the 30 annual total precip rasters 
ppt_mean <- mean(ppt_annual_by_year, na.rm = TRUE)

# save raster as .tif for loading later if needed
writeRaster(ppt_mean, "ppt_30yr_mean.tif", overwrite = TRUE)

# load in ppt raster
ppt_mean <- rast("ppt_30yr_mean.tif")

# crop ppt raster to study area
ppt_crop <- crop(ppt_mean, small_box)

# create a data frame of grid cell values for the study region
ppt_df_map <- as.data.frame(ppt_crop, xy = TRUE, na.rm = TRUE)
colnames(ppt_df_map) <- c("lon", "lat", "precip")


# plot PPT across the whole region with site locations identified
ppt_map_by_region <- ggplot() +
  geom_raster(data = ppt_df_map,
              aes(x = lon, y = lat, fill = precip)) +
  scale_fill_viridis_c(option = "C",
                       direction = -1,
                       limits = c(800, 1800),
                       name = "Annual Accumulated PPT (mm)") +
  geom_sf(data = sites_sf,
          aes(shape = elevation),
          size = 3,
          stroke = 1,
          fill = NA,
          color = "black") +
  scale_shape_manual(
    values = c(
      "low" = 21,     # circle
      "mid" = 22,     # square
      "high" = 24     # triangle
    )
  ) +
  geom_sf(data = california, fill = NA, color = "black") +
  coord_sf(xlim = c(-121.3, -120.6),
           ylim = c(39.3, 39.6)) +
  theme_minimal()

ppt_map_by_region

# extract climate values for specific site locations, first make sure CRS matches between site points and climate data
sites_vect <- terra::project(vect(sites_sf), crs(ppt_mean))
ppt_vals <- terra::extract(ppt_mean, sites_vect)

ppt_table <- cbind(site_boundaries, ppt_vals[,-1])
colnames(ppt_table)[ncol(ppt_table)] <- "ppt_30yr_mean"

ppt_table

# now run through the same code for climatic water deficit 
def_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_", years, ".nc")
)

def_annual_by_year <- tapp(
  def_stack_stable,
  index = rep(1:30, each = 12),
  fun = sum,
  na.rm = TRUE
)

def_mean <- mean(def_annual_by_year, na.rm = TRUE)
writeRaster(def_mean, "def_30yr_mean.tif", overwrite = TRUE)

def_mean <- rast("def_30yr_mean.tif")

def_crop <- crop(def_mean, small_box)

def_df_map <- as.data.frame(def_crop, xy = TRUE, na.rm = TRUE)
colnames(def_df_map) <- c("lon", "lat", "deficit")

def_map_by_region <- ggplot() +
  geom_raster(data = def_df_map,
              aes(x = lon, y = lat, fill = deficit)) +
  scale_fill_viridis_c(option = "C",
                       direction = 1,
                       limits = c(400,900),
                       name = "Annual Climatic Water Deficit (mm)") +
  geom_sf(data = sites_sf,
          aes(shape = elevation),
          size = 3,
          stroke = 1,
          fill = NA,
          color = "black") +
  scale_shape_manual(
    values = c(
      "low" = 21,     # circle
      "mid" = 22,  # square
      "high" = 24     # triangle
    )
  ) +
  geom_sf(data = california, fill = NA, color = "black") +
  coord_sf(xlim = c(-121.3, -120.6),
           ylim = c(39.3, 39.6)) +
  theme_minimal()

def_map_by_region

sites_vect <- terra::project(vect(sites_sf), crs(def_mean))
def_vals <- terra::extract(def_mean, sites_vect)

def_table <- cbind(site_boundaries, def_vals[,-1])
colnames(def_table)[ncol(def_table)] <- "def_30yr_mean"

def_table


## VPD
vpd_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_", years, ".nc")
)

vpd_annual_by_year <- tapp(
  vpd_stack_stable,
  index = rep(1:30, each = 12),
  fun = mean,
  na.rm = TRUE
)

vpd_mean <- mean(vpd_annual_by_year, na.rm = TRUE)
writeRaster(vpd_mean, "vpd_30yr_mean.tif", overwrite = TRUE)

vpd_mean <- rast("vpd_30yr_mean.tif")

vpd_crop <- crop(vpd_mean, small_box)

vpd_df_map <- as.data.frame(vpd_crop, xy = TRUE, na.rm = TRUE)
colnames(vpd_df_map) <- c("lon", "lat", "VPD")

vpd_map_by_region <- ggplot() +
  geom_raster(data = vpd_df_map,
              aes(x = lon, y = lat, fill = VPD)) +
  scale_fill_viridis_c(option = "C",
                       direction = 1,
                       limits = c(0.7, 1.3),
                       name = "Annual VPD (kPa))") +
  geom_sf(data = sites_sf,
          aes(shape = elevation),
          size = 3,
          stroke = 1,
          fill = NA,
          color = "black") +
  scale_shape_manual(
    values = c(
      "low" = 21,     # circle
      "mid" = 22,  # square
      "high" = 24     # triangle
    )
  ) +
  geom_sf(data = california, fill = NA, color = "black") +
  coord_sf(xlim = c(-121.3, -120.6),
           ylim = c(39.3, 39.6)) +
  theme_minimal()

vpd_map_by_region

sites_vect <- terra::project(vect(sites_sf), crs(vpd_mean))
vpd_vals <- terra::extract(vpd_mean, sites_vect)

vpd_table <- cbind(site_boundaries, vpd_vals[,-1])
colnames(vpd_table)[ncol(def_table)] <- "vpd_30yr_mean"

vpd_table

# AET 
aet_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_aet_", years, ".nc")
)

aet_annual_by_year <- tapp(
  aet_stack_stable,
  index = rep(1:30, each = 12),
  fun = sum,
  na.rm = TRUE
)

aet_mean <- mean(aet_annual_by_year, na.rm = TRUE)
writeRaster(aet_mean, "aet_30yr_mean.tif", overwrite = TRUE)

aet_mean <- rast("aet_30yr_mean.tif")

aet_crop <- crop(aet_mean, small_box)

aet_df_map <- as.data.frame(aet_crop, xy = TRUE, na.rm = TRUE)
colnames(aet_df_map) <- c("lon", "lat", "aet")

aet_map_by_region <- ggplot() +
  geom_raster(data = aet_df_map,
              aes(x = lon, y = lat, fill = aet)) +
  scale_fill_viridis_c(option = "C",
                       direction = 1,
                       limits = c(400,800),
                       name = "Annual AET (mm)") +
  geom_sf(data = sites_sf,
          aes(shape = elevation),
          size = 3,
          stroke = 1,
          fill = NA,
          color = "black") +
  scale_shape_manual(
    values = c(
      "low" = 21,     # circle
      "mid" = 22,  # square
      "high" = 24     # triangle
    )
  ) +
  geom_sf(data = california, fill = NA, color = "black") +
  coord_sf(xlim = c(-121.3, -120.6),
           ylim = c(39.3, 39.6)) +
  theme_minimal()

aet_map_by_region

sites_vect <- terra::project(vect(sites_sf), crs(aet_mean))
aet_vals <- terra::extract(aet_mean, sites_vect)

aet_table <- cbind(site_boundaries, aet_vals[,-1])
colnames(aet_table)[ncol(aet_table)] <- "aet_30yr_mean"

aet_table

# PET
pet_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_pet_", years, ".nc")
)

pet_annual_by_year <- tapp(
  pet_stack_stable,
  index = rep(1:30, each = 12),
  fun = sum,
  na.rm = TRUE
)

pet_mean <- mean(pet_annual_by_year, na.rm = TRUE)
writeRaster(pet_mean, "pet_30yr_mean.tif", overwrite = TRUE)

pet_mean <- rast("pet_30yr_mean.tif")

pet_crop <- crop(pet_mean, small_box)

pet_df_map <- as.data.frame(pet_crop, xy = TRUE, na.rm = TRUE)
colnames(pet_df_map) <- c("lon", "lat", "pet")

pet_map_by_region <- ggplot() +
  geom_raster(data = pet_df_map,
              aes(x = lon, y = lat, fill = pet)) +
  scale_fill_viridis_c(option = "C",
                       direction = 1,
                       name = "Annual PET (mm)") +
  geom_sf(data = sites_sf,
          aes(shape = elevation),
          size = 3,
          stroke = 1,
          fill = NA,
          color = "black") +
  scale_shape_manual(
    values = c(
      "low" = 21,     # circle
      "mid" = 22,  # square
      "high" = 24     # triangle
    )
  ) +
  geom_sf(data = california, fill = NA, color = "black") +
  coord_sf(xlim = c(-121.3, -120.6),
           ylim = c(39.3, 39.6)) +
  theme_minimal()

pet_map_by_region

sites_vect <- terra::project(vect(sites_sf), crs(pet_mean))
pet_vals <- terra::extract(pet_mean, sites_vect)

pet_table <- cbind(site_boundaries, pet_vals[,-1])
colnames(pet_table)[ncol(pet_table)] <- "pet_30yr_mean"

pet_table

# plot with topographic map with rivers
hill_df <- as.data.frame(hillshade, xy = TRUE)
pet_r   <- pet_crop
ext_all <- ext(small_box)

# Resample hillshade to match PET resolution
hill_r <- rast(hill_df, type = "xyz", crs = "EPSG:4326")

# Check what CRS each has
crs(pet_crop)
crs(hill_resamp)

# Reproject hillshade to match PET
hill_resamp <- project(hill_resamp, crs(pet_crop))

# Resample to exactly match PET grid
hill_resamp <- resample(hill_resamp, pet_crop, method = "bilinear")

# Now get max and multiply
hill_max <- global(hill_resamp, "max", na.rm = TRUE)$max
pet_shaded <- pet_crop * (hill_resamp / hill_max)



pet_crop <- crop(pet_mean, ext_all)
dem_crop <- crop(dem, ext_all)

slope  <- terrain(dem_crop, v = "slope", unit = "radians")
aspect <- terrain(dem_crop, v = "aspect", unit = "radians")

hillshade <- shade(slope, aspect, angle = 45, direction = 315)

hill_df <- as.data.frame(hillshade, xy = TRUE)
names(hill_df) <- c("lon", "lat", "shade")

range(hill_df$lon)
range(hill_df$lat)

st_bbox(rivers_clip)

# plot same map with topographic map 
california <- ne_states(country = "United States of America", returnclass = "sf")
california <- california[california$name == "California", ]

california_clip <- st_crop(california, st_bbox(c(xmin=-121.3, xmax=-120.6, 
                                                 ymin=39.3, ymax=39.6), 
                                               crs=4326))
# Force nhd_lines to 4326 explicitly
nhd_lines <- st_transform(nhd_lines, 4326)

# Also confirm your tile data spans the window
range(hill_df$lon)   # should include values in -121.3 to -120.6
range(pet_df_map$lon)

coord_sf(
  xlim = c(-121.3, -120.6),
  ylim = c(39.3, 39.6),
  crs = 4326,
  expand = FALSE
)

### this one worked 
ggplot() +
  geom_tile(data = hill_df, aes(x = lon, y = lat, fill = shade), na.rm = TRUE) +
  scale_fill_gradient(low = "grey25", high = "grey95", guide = "none",
                      na.value = NA) +
  new_scale_fill() +
  geom_tile(data = pet_df_map, aes(x = lon, y = lat, fill = pet), 
            alpha = 0.55, na.rm = TRUE) +
  scale_fill_viridis_c(
    option = "magma",
    begin = 0.1,
    end = 0.95,
    name = "PET",
    limits = c(min(pet_df_map$pet, na.rm = TRUE),
               max(pet_df_map$pet, na.rm = TRUE)),
    guide = guide_colorbar(override.aes = list(alpha = 0.55))
  )+
  geom_sf(data = nhd_lines, color = "grey40", linewidth = 0.8) +
  geom_sf(data = sites_sf, aes(shape = elevation), size = 3, fill = NA, color = "black") +
  scale_shape_manual(values = c("low" = 21, "mid" = 22, "high" = 24)) +
  coord_sf(xlim = c(min(hill_df$lon), -120.6), ylim = c(39.3, 39.6), expand = FALSE) +
  theme_minimal()


# assign climate to seedling physiology data set for each site
climate_df <- merge(
  merge(
    merge(
      merge(pet_table, vpd_table, by = "id", all = TRUE),
      ppt_table, by = "id", all = TRUE
    ),
    def_table, by = "id", all = TRUE
  ),
  aet_table, by = "id", all = TRUE
)

climate_df <- climate_df[, c("id", "pet_30yr_mean", "vpd_30yr_mean", "ppt_30yr_mean", "def_30yr_mean", "aet_30yr_mean")]
write.csv(climate_df, "climate_df.csv", row.names = FALSE)

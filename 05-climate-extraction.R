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

library(tmap)
library(AOI)
library(sf)
library(climateR)
library(ggplot2)
library(terra)
library(ggspatial)
library(prettymapr)
library(maps)

# load california boundary
california <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
california <- california[california$ID == "california", ]

# create a bounding box for the north yuba
small_box <- ext(-121.2, -119.3, 38.5, 40.0)

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

# crop ppt raster to study area
ppt_crop <- crop(ppt_mean, small_box)

# create a data frame of grid cell values for the study region
ppt_df_map <- as.data.frame(ppt_crop, xy = TRUE, na.rm = TRUE)
colnames(ppt_df_map) <- c("lon", "lat", "precip")

# convert site table into spatial data points for extracting climate and plotting sites on map
sites_sf <- st_as_sf(site_boundaries, coords = c("lon", "lat"), crs = 4326)

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

# assign climate to physiology dataset for each site
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

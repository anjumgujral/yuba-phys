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
install.packages('maps')
library(maps)

#save.image("my_workspace.RData")
load("my_workspace.RData")

california <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
california <- california[california$ID == "california", ]
calbound = AOI::aoi_get(state = "CA")
small_box <- ext(-121.2, -119.3, 38.5, 40.0)

# load in exact population locations 
site_boundaries <- data.frame(
  id = c("1","2","3", "4", "5", "7", "8", "9", "10", "11"),
  lon = c(-121.051720, -121.046958, -121.006063, -121.000819, -121.004718, -120.829953, -120.780825, -120.786236, -120.778930, -120.826322),
  lat = c(39.491387, 39.490947, 39.487019, 39.478789, 39.472552, 39.511469, 39.504699, 39.507502, 39.507644, 39.513851)
)
site_boundaries$elevation <- NA

site_boundaries$elevation[site_boundaries$id %in% c("1","2")] <- "low"
site_boundaries$elevation[site_boundaries$id %in% c("3","4","5")] <- "mid"
site_boundaries$elevation[site_boundaries$id %in% c("7","8","9","10","11")] <- "high"

sites <- vect(site_boundaries, geom = c("lon", "lat"), crs = "EPSG:4326")
sites_projection <- project(sites, "EPSG:32610")
buffer <- buffer(sites_projection, width = 304.8)
buffer_projection <- project(buffer, "EPSG:4326")

# download 30 year climate normals for ppt
ppt_1994 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_1994.nc")
ppt_1995 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_1995.nc")
ppt_1996 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_1996.nc")
ppt_1997 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_1997.nc")
ppt_1998 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_1998.nc")
ppt_1999 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_1999.nc")
ppt_2000 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2000.nc")
ppt_2001 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2001.nc")
ppt_2002 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2002.nc")
ppt_2003 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2003.nc")
ppt_2004 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2004.nc")
ppt_2005 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2005.nc")
ppt_2006 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2006.nc")
ppt_2007 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2007.nc")
ppt_2008 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2008.nc")
ppt_2009 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2009.nc")
ppt_2010 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2010.nc")
ppt_2011 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2011.nc")
ppt_2012 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2012.nc")
ppt_2013 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2013.nc")
ppt_2014 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2014.nc")
ppt_2015 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2015.nc")
ppt_2016 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2016.nc")
ppt_2017 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2017.nc")
ppt_2018 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2018.nc")
ppt_2019 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2019.nc")
ppt_2020 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2020.nc")
ppt_2021 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2021.nc")
ppt_2022 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2022.nc")
ppt_2023 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_2023.nc")


ppt_stack <- c(
  ppt_1994, ppt_1995, ppt_1996, ppt_1997,
  ppt_1998, ppt_1999, ppt_2000, ppt_2001, ppt_2002,
  ppt_2003, ppt_2004, ppt_2005, ppt_2006, ppt_2007,
  ppt_2008, ppt_2009, ppt_2010, ppt_2011, ppt_2012,
  ppt_2013, ppt_2014, ppt_2015, ppt_2016, ppt_2017,
  ppt_2018, ppt_2019, ppt_2020, ppt_2021, ppt_2022,
  ppt_2023
)

years <- 1994:2023

ppt_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_", years, ".nc")
)

# take monthly precip, group it by 12 months, sum prcip across those 12 months, and create a raster for each year 
ppt_annual_by_year <- tapp(
  ppt_stack_stable,
  index = rep(1:30, each = 12),
  fun = sum,
  na.rm = TRUE
)
# take the average of the 30 annual total precip rasters 
ppt_mean <- mean(ppt_annual_by_year, na.rm = TRUE)
writeRaster(ppt_mean, "ppt_30yr_mean.tif", overwrite = TRUE)

#ppt_mean <- terra::rast("ppt_30yr_mean.tif")
ppt_crop <- crop(ppt_mean, small_box)
ppt_crop <- mask(ppt_crop, vect(california))

ppt_df_map <- as.data.frame(ppt_crop, xy = TRUE, na.rm = TRUE)
colnames(ppt_df_map) <- c("lon", "lat", "precip")

sites_sf <- st_as_sf(site_boundaries, coords = c("lon", "lat"), crs = 4326)

# make a map of PPT across the whole region with site locations identified
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

#ppt_buffer <- project(buffer, crs(ppt_annual_by_year))

#ppt_vals <- terra::extract(
 # ppt_annual_by_year,
  #ppt_buffer,
 # fun = mean,   # average across buffer area
  #na.rm = TRUE
#)
ppt_vals <- terra::extract(
  ppt_mean,
  vect(sites_sf)
)

#ppt_df$ppt_30yr_mean <- rowMeans(
  #ppt_df[, -(1:ncol(site_boundaries))],
  #na.rm = TRUE
#)

#ppt_sf <- st_as_sf(ppt_df, coords = c("lon", "lat"), crs = 4326)


ppt_table <- cbind(site_boundaries, ppt_vals[,-1])
colnames(ppt_table)[ncol(ppt_table)] <- "ppt_30yr_mean"

#ppt_df$ppt_30yr_mean <- rowMeans(
 # ppt_df[, -(1:ncol(site_boundaries))],
 # na.rm = TRUE
#)

ppt_table


ppt_mean <- mean(ppt_annual_by_year, na.rm = TRUE)
ppt_crop <- crop(ppt_mean, small_box)
ppt_crop <- mask(ppt_crop, vect(california))
ppt_df_map <- as.data.frame(ppt_crop, xy = TRUE, na.rm = TRUE)
colnames(ppt_df_map) <- c("lon", "lat", "precip")

ppt_map <- ggplot() +
  geom_raster(data = ppt_df_map,
              aes(x = lon, y = lat, fill = precip)) +
  scale_fill_viridis_c(option = "C",
                       name = "30-year Mean Precip (mm)") +
  geom_sf(data = ppt_sf, color = "red", size = 3) +
  geom_sf(data = california, fill = NA, color = "black") +
  coord_sf(xlim = c(-121.3, -120.6),
           ylim = c(39.3, 39.6)) +
  theme_minimal()

ppt_map

# download 30 year normals for water deficit 
def_1994 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_1994.nc")
def_1995 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_1995.nc")
def_1996 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_1996.nc")
def_1997 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_1997.nc")
def_1998 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_1998.nc")
def_1999 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_1999.nc")
def_2000 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2000.nc")
def_2001 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2001.nc")
def_2002 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2002.nc")
def_2003 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2003.nc")
def_2004 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2004.nc")
def_2005 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2005.nc")
def_2006 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2006.nc")
def_2007 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2007.nc")
def_2008 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2008.nc")
def_2009 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2009.nc")
def_2010 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2010.nc")
def_2011 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2011.nc")
def_2012 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2012.nc")
def_2013 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2013.nc")
def_2014 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2014.nc")
def_2015 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2015.nc")
def_2016 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2016.nc")
def_2017 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2017.nc")
def_2018 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2018.nc")
def_2019 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2019.nc")
def_2020 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2020.nc")
def_2021 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2021.nc")
def_2022 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2022.nc")
def_2023 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_2023.nc")

def_stack <- c(
  def_1994, def_1995, def_1996, def_1997,
  def_1998, def_1999, def_2000, def_2001, def_2002,
  def_2003, def_2004, def_2005, def_2006, def_2007,
  def_2008, def_2009, def_2010, def_2011, def_2012,
  def_2013, def_2014, def_2015, def_2016, def_2017,
  def_2018, def_2019, def_2020, def_2021, def_2022,
  def_2023
)

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
#def_crop <- mask(def_crop, vect(california))

def_df_map <- as.data.frame(def_crop, xy = TRUE, na.rm = TRUE)
colnames(def_df_map) <- c("lon", "lat", "deficit")


# make a map of DEF across the whole region with site locations identified
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

def_vals <- terra::extract(
  def_mean,
  vect(sites_sf)
)


def_table <- cbind(site_boundaries, def_vals[,-1])
colnames(def_table)[ncol(def_table)] <- "def_30yr_mean"

def_table

#def_buffer <- project(buffer, crs(def_annual_by_year))
#def_vals <- terra::extract(
 # def_annual_by_year,
  #buffer,
  #fun = mean,
  #na.rm = TRUE
#)
#def_df <- cbind(site_boundaries, def_vals)
#def_df$def_annual <- rowMeans(def_df[, -(1:ncol(site_boundaries))], na.rm = TRUE)
#def_sf <- st_as_sf(def_df, coords = c("lon", "lat"), crs = 4326)

#def_df <- cbind(site_boundaries, def_vals[,-1])

#def_df$cwd_30yr_mean <- rowMeans(
 # def_df[, -(1:ncol(site_boundaries))],
#  na.rm = TRUE
#)
#def_table <- def_df[, c("id", "cwd_30yr_mean")]


## vpd next
vpd_1994 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_1994.nc")
vpd_1995 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_1995.nc")
vpd_1996 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_1996.nc")
vpd_1997 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_1997.nc")
vpd_1998 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_1998.nc")
vpd_1999 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_1999.nc")
vpd_2000 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2000.nc")
vpd_2001 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2001.nc")
vpd_2002 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2002.nc")
vpd_2003 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2003.nc")
vpd_2004 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2004.nc")
vpd_2005 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2005.nc")
vpd_2006 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2006.nc")
vpd_2007 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2007.nc")
vpd_2008 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2008.nc")
vpd_2009 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2009.nc")
vpd_2010 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2010.nc")
vpd_2011 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2011.nc")
vpd_2012 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2012.nc")
vpd_2013 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2013.nc")
vpd_2014 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2014.nc")
vpd_2015 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2015.nc")
vpd_2016 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2016.nc")
vpd_2017 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2017.nc")
vpd_2018 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2018.nc")
vpd_2019 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2019.nc")
vpd_2020 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2020.nc")
vpd_2021 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2021.nc")
vpd_2022 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2022.nc")
vpd_2023 <- terra::rast("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_2023.nc")

vpd_stack <- c(
  vpd_1994, vpd_1994, vpd_1995, vpd_1996, vpd_1997,
  vpd_1998, vpd_1999, vpd_2000, vpd_2001, vpd_2002,
  vpd_2003, vpd_2004, vpd_2005, vpd_2006, vpd_2007,
  vpd_2008, vpd_2009, vpd_2010, vpd_2011, vpd_2012,
  vpd_2013, vpd_2014, vpd_2015, vpd_2016, vpd_2017,
  vpd_2018, vpd_2019, vpd_2020, vpd_2021, vpd_2022,
  vpd_2023
)

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
#vpd_crop <- mask(vpd_crop, vect(california))

vpd_df_map <- as.data.frame(vpd_crop, xy = TRUE, na.rm = TRUE)
colnames(vpd_df_map) <- c("lon", "lat", "deficit")


# make a map of VPD across the whole region with site locations identified
vpd_map_by_region <- ggplot() +
  geom_raster(data = vpd_df_map,
              aes(x = lon, y = lat, fill = deficit)) +
  scale_fill_viridis_c(option = "C",
                       direction = 1,
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

vpd_vals <- terra::extract(
  vpd_mean,
  vect(sites_sf)
)


vpd_table <- cbind(site_boundaries, vpd_vals[,-1])
colnames(vpd_table)[ncol(def_table)] <- "vpd_30yr_mean"

vpd_table


#vpd_buffer <- project(buffer, crs(vpd_annual_by_year))

vpd_vals <- terra::extract(
  vpd_annual_by_year,
  vpd_buffer,
  fun = mean,
  na.rm = TRUE
)

vpd_df <- cbind(site_boundaries, vpd_vals[,-1])

vpd_df$vpd_30yr_mean <- rowMeans(
  vpd_df[, -(1:ncol(site_boundaries))],
  na.rm = TRUE
)

vpd_sf <- st_as_sf(vpd_df, coords = c("lon","lat"), crs = 4326)

vpd_df <- cbind(site_boundaries, vpd_vals[,-1])
vpd_df$vpd_30yr_mean <- rowMeans(
  vpd_df[, -(1:ncol(site_boundaries))],
  na.rm = TRUE
)
vpd_table <- vpd_df[, c("id", "vpd_30yr_mean")]

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
#vpd_crop <- mask(vpd_crop, vect(california))

vpd_df_map <- as.data.frame(vpd_crop, xy = TRUE, na.rm = TRUE)
colnames(vpd_df_map) <- c("lon", "lat", "VPD")


# make a map of VPD across the whole region with site locations identified
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

vpd_vals <- terra::extract(
  vpd_mean,
  vect(sites_sf)
)


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
#aet_crop <- mask(aet_crop, vect(california))

aet_df_map <- as.data.frame(aet_crop, xy = TRUE, na.rm = TRUE)
colnames(aet_df_map) <- c("lon", "lat", "aet")


# make a map of VPD across the whole region with site locations identified
aet_map_by_region <- ggplot() +
  geom_raster(data = aet_df_map,
              aes(x = lon, y = lat, fill = aet)) +
  scale_fill_viridis_c(option = "C",
                       direction = 1,
                       name = "Annual AET (mm))") +
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

aet_vals <- terra::extract(
  aet_mean,
  vect(sites_sf)
)


aet_table <- cbind(site_boundaries, aet_vals[,-1])
colnames(aet_table)[ncol(aet_table)] <- "aet_30yr_mean"

aet_table
## run for just 2023 analyses 

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
write.csv(site_boundaries, "site_boundaries.csv", row.names = FALSE)


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


# climate normals
normals <- climateR::getTerraClimNormals(
  AOI = buffer_projection,
  varname = c("ppt", "def", "tmax", "tmin", "vpd"),
  scenario = "19812010"
)

varname = c("ppt", "def", "tmax", "tmin", "vpd")

ppt_norm <- climateR::getTerraClimNormals(
  AOI = buffer_projection,
  varname = "ppt",
  scenario = "19812010"
)




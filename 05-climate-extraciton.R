#### Example code for downloading TerraClimate data and then extracting
# to lat/lons of sample sites



install.packages('AOI')
library('AOI')
install.packages('climateR')
library(climateR)
library(terra)

calbound = aoi_get(state = "CA")
# download terrclimate normals 81-2010
tcn <- getTerraClimNormals(calbound, varname=c("aet","def","pet","ppt","soil","srad","swe","tmax","tmin","vap","ws","vpd"),scenario = "19812010")
# monthly 20yr normals
# download TerraClimate for 2017 and 2018 to make 2018 water year
tcn18wy <- getTerraClim(calbound, varname = c("aet","def","pet","ppt","soil","PDSI","tmax","tmin","vap","ws","vpd"),startDate = "2017-10-01",endDate = "2018-09-01")
# note, the dates are a bit funky, but these correspond to Oct 2017 - Sept 2018 months

## summarize monthly into annual/seasonal variables
tcn_an <- list()
tcn_an$aet <- sum(tcn$aet) # total actual annual evap (mm)
tcn_an$cwd <- sum(tcn$def) # total CWD (mm)
tcn_an$cwd_gs <- mean(tcn$def[[4:9]]) # mean CWD of the growing season (Apr - Sept)
tcn_an$pet <- sum(tcn$pet) # total PET (mm)
tcn_an$ppt <- sum(tcn$ppt) # total precip (mm)
tcn_an$soil_mean <- mean(tcn$soil) # mean annual soil moisture
tcn_an$soil_min <- min(tcn$soil) # min soil moisture
tcn_an$srad <- mean(tcn$srad) # mean shortwave
tcn_an$swe_max <- max(tcn$swe) # max swee
tcn_an$tmax <- max(tcn$tmax) # max T of hotest month
tcn_an$tmin <- min(tcn$tmin) # min T of coldest month
tcn_an$vpd_max <- max(tcn$vpd) # max VPD
tcn_an$vpd_spr <- mean(tcn$vpd[[4:5]]) # mean VPD of spring
tcn_an$vpd_gs <- mean(tcn$vpd[[4:9]]) # mean VPD of growing season
tcn_an$aet_ds <- sum(tcn$aet[[c(1:3,10:12)]]) # dormant season aet for garden ms
tcn_an$cwd_ds <- sum(tcn$cwd[[c(1:3,10:12)]])
tcn_an$pet_ds <- sum(tcn$pet[[c(1:3,10:12)]])
tcn_an$ppt_ds <- sum(tcn$ppt[[c(1:3,10:12)]])
tcn_an$tmax_ds <- sum(tcn$tmax[[c(1:3,10:12)]])
tcn_an$tmin_ds <- sum(tcn$tmin[[c(1:3,10:12)]])

## terra::extract popluation values from the annual/seasonal rasters
pop.terraclim <- popclim.fall %>% select(Site, Code, Lon.dd, Lat.dd)
pop.terraclim$aet <- terra::extract(tcn_an$aet, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$cwd <- terra::extract(tcn_an$cwd, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$cwd_gs <- terra::extract(tcn_an$cwd_gs, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$pet <- terra::extract(tcn_an$pet, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$ppt <- terra::extract(tcn_an$ppt, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$soil_mean <- terra::extract(tcn_an$soil_mean, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$soil_min <- terra::extract(tcn_an$soil_min, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$srad <- terra::extract(tcn_an$srad, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$swe_max <- terra::extract(tcn_an$swe_max, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$tmax <- terra::extract(tcn_an$tmax, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$tmin <- terra::extract(tcn_an$tmin, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$vpd_max <- terra::extract(tcn_an$vpd_max, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$vpd_spr <- terra::extract(tcn_an$vpd_spr, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$vpd_gs <- terra::extract(tcn_an$vpd_gs, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]


# summarise 2018 wy months into annual/seasonal variables
tcn_18 <- list()
tcn_18$aet.2018wy <- sum(tcn18wy$aet) # total actual annual evap (mm)
tcn_18$aet.2018ds <- sum(tcn18wy$aet[[1:6]]) # AET of the dormant season (Oct-March)
tcn_18$aet.2018gs <- sum(tcn18wy$aet[[7:12]]) # AET of the growing season (Apr-Sept)
tcn_18$cwd.2018wy <- sum(tcn18wy$def) # total CWD (mm)
tcn_18$cwd.2018ds <- sum(tcn18wy$def[[1:6]]) # mean CWD of the dormant season
tcn_18$cwd.2018gs <- sum(tcn18wy$def[[7:12]]) # mean CWD of the growing season
tcn_18$pet.2018wy <- sum(tcn18wy$pet) # total PET (mm)
tcn_18$pet.2018ds <- sum(tcn18wy$pet[[1:6]])
tcn_18$pet.2018gs <- sum(tcn18wy$pet[[7:12]])
tcn_18$ppt.2018wy <- sum(tcn18wy$ppt) # total precip (mm)
tcn_18$ppt.2018ds <- sum(tcn18wy$ppt[[1:6]])
tcn_18$ppt.2018gs <- sum(tcn18wy$ppt[[7:12]])
tcn_18$soil_mean.2018wy <- mean(tcn18wy$soil) # mean annual soil moisture
tcn_18$soil_min.2018wy <- min(tcn18wy$soil) # min soil moisture
tcn_18$soil_mean.2018ds <- mean(tcn18wy$soil[[1:6]]) # mean ds soil moisture
tcn_18$soil_min.2018ds <- min(tcn18wy$soil[[1:6]]) # min ds soil moisture
tcn_18$soil_mean.2018gs <- mean(tcn18wy$soil[[7:12]]) # mean gs soil moisture
tcn_18$soil_min.2018gs <- min(tcn18wy$soil[[7:12]]) # min gs soil moisture
tcn_18$tmax.2018wy <- max(tcn18wy$tmax) # max T of hotest month
tcn_18$tmax.2018sp <- max(tcn18wy$tmax[[6:8]]) # max temp of the spring (MAM)
tcn_18$tmin.2018wy <- min(tcn18wy$tmin) # min T of coldest month
tcn_18$tmin.2018sp <- max(tcn18wy$tmin[[6:8]]) # min temp of the spring (MAM)
tcn_18$vpd_max.2018wy <- max(tcn18wy$vpd) # max VPD
tcn_18$vpd.2018sp <- mean(tcn18wy$vpd[[6:8]]) # mean VPD of spring (MAM)
tcn_18$vpd.2018gs <- mean(tcn18wy$vpd[[7:12]]) # mean VPD of growing season


# terra::extract pop points from rasters
pop.terraclim$aet.2018wy <- terra::extract(tcn_18$aet.2018wy, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$aet.2018ds <- terra::extract(tcn_18$aet.2018ds, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$aet.2018gs <- terra::extract(tcn_18$aet.2018gs, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$cwd.2018wy <- terra::extract(tcn_18$cwd.2018wy, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$cwd.2018ds <- terra::extract(tcn_18$cwd.2018ds, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$cwd.2018gs <- terra::extract(tcn_18$cwd.2018gs, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$pet.2018wy <- terra::extract(tcn_18$pet.2018wy, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$pet.2018ds <- terra::extract(tcn_18$pet.2018ds, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$pet.2018gs <- terra::extract(tcn_18$pet.2018gs, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$ppt.2018wy <- terra::extract(tcn_18$ppt.2018wy, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$ppt.2018ds <- terra::extract(tcn_18$ppt.2018ds, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$ppt.2018gs <- terra::extract(tcn_18$ppt.2018gs, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$soil_mean.2018wy <- terra::extract(tcn_18$soil_mean.2018wy, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$soil_min.2018wy <- terra::extract(tcn_18$soil_min.2018wy, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$soil_mean.2018ds <- terra::extract(tcn_18$soil_mean.2018ds, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$soil_min.2018ds <- terra::extract(tcn_18$soil_min.2018ds, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$soil_mean.2018gs <- terra::extract(tcn_18$soil_mean.2018gs, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$soil_min.2018gs <- terra::extract(tcn_18$soil_min.2018gs, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$tmax.2018wy <- terra::extract(tcn_18$tmax.2018wy, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$tmax.2018sp <- terra::extract(tcn_18$tmax.2018sp, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$tmin.2018wy <- terra::extract(tcn_18$tmin.2018wy, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$tmin.2018sp <- terra::extract(tcn_18$tmin.2018sp, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$vpd_max.2018wy <- terra::extract(tcn_18$vpd_max.2018wy, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$vpd.2018sp <- terra::extract(tcn_18$vpd.2018sp, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$vpd.2018gs <- terra::extract(tcn_18$vpd.2018gs, data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]

### Check how much rain fell in June, July, Aug, Sept of 2018 (note, month 12= Sept 2018)
pop.terraclim$ppt.2018june <- terra::extract(tcn18wy$ppt[[9]], data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$ppt.2018jul <- terra::extract(tcn18wy$ppt[[10]], data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$ppt.2018aug <- terra::extract(tcn18wy$ppt[[11]], data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$ppt.2018sep <- terra::extract(tcn18wy$ppt[[12]], data.frame(popclim.fall$Lon.dd,popclim.fall$Lat.dd))[,2]
pop.terraclim$ppt.2018jjas <- pop.terraclim$ppt.2018june + pop.terraclim$ppt.2018jul + pop.terraclim$ppt.2018aug + pop.terraclim$ppt.2018sep

# NOTE: TerraClimate 2018 wy values look MUCH more like climate normals than BCM values do
# is this possibly do to the location changes between BCM extractions? with the 2018 versions being better?
# - The agreement between BCM and TerraClimate isn't great for the normals
# - but it's also real bad for 2018 wy. 
# - for cwd, the normals actually look more similar than the 2018wy data
#write.csv(pop.terraclim, "DerivedData/Population_TerraClimate_230512.csv")
write.csv(pop.terraclim, "DerivedData/Population_TerraClimate_241018.csv") # added in summer ppt 2018 to show that there was none

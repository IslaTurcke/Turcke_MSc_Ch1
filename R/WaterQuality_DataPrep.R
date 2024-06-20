### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script was used to prepare data for Isla Turcke's first MSc chapter 
# in the lab of Dr. Stephanie Green at the University of Alberta (2022-2024). 
# Data are specific to southern Florida and include bottom water conditions 
# such as temperature, salinity, and dissolved oxygen. These data were used for 
# habitat suitability modeling using maximum entropy.

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)


### WATER QUALITY DATA PREP ###

# Data on water temp, DO, and salinity from Florida Keys and Biscayne Bay 
# - filtering for 2014, 2016, 2018, and 2022 sites within study domain
# - calculating the average yearly average for each site
# - calculating the average yearly variance for each site



# Set Up ------------------------------------------------------------------


# load packages
library(easypackages) 

#install_packages("tidyr", "rgdal", "sf", "terra", "raster", "tidyverse", 
#                 "PNWColors", "tibble", "readxl", "dplyr", "conflicted", "ncf", 
#                 "spdep", "maptools")
libraries("here", "tidyr", "rgdal", "sf", "terra", "raster", "tidyverse", 
          "PNWColors", "tibble", "readxl", "dplyr", "conflicted", "ncf", 
          "spdep", "maptools") 
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("xlim", "spex")

# SET WORKING DIRECTORY
setwd("Z:/Isla_MSc_Ch1/")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/Occurence_DataPrep.R")

# save PROJ.4 string for NEW and OLD standard projection 
# EPSG:6346 NAD 1983 2011 UTM Zone 17N
new_crs <- crs("+init=epsg:6346")

# and source data: 
# EPSG:4326 WGS 84/World Geodetic System 1984 (decdeg)
wgs_crs <- crs("+init=epsg:4326")
# EPSG:2236 NAD83 / Florida East (ftUS))
us_ft <- crs("+init=epsg:2236")
# EPSG:3857 WGS 84 / Pseudo-Mercator
bb_crs <- crs("+init=epsg:3857")



# Initial Prep - Water Quality Monitoring Network ----------------------------


# import data from The SERC Water Quality Monitoring Network (WQMN) 
# (http://serc.fiu.edu/wqmnetwork/)
WQMN <- read_excel(here("Source_Data","Water_Conditions","WQFloridaKeys&Shelf (ppm) UPDATED 2-9-2023.xlsx"), 
                   sheet = "Data in ppm") 

# clean up dates, edit names, filter for years of interest
wqmn <- WQMN %>%
  separate(DATE, into = c("YEAR", "MONTH", "DAY"), sep = "-", remove = F) %>% # separate out date data
  filter(YEAR %in% c("2014","2016","2018","2022")) %>% 
  rename(TEMP = `TEMP-B`, SAL = `SAL-B`, DO = `DO-B`) %>% # rename variables because "-" causes problems
  select(SITE, YEAR, MONTH, LONDEC, LATDEC, TEMP, SAL, DO)

# transform to project coordinate system 
wqmn <- wqmn %>%
  st_as_sf(., coords = c("LONDEC", "LATDEC"), crs = wgs_crs) %>% 
  st_transform(., new_crs) %>% # re-project to standard CRS
  mutate(LON_M = sf::st_coordinates(.)[,1], # save LON_M and LAT_M columns
         LAT_M = sf::st_coordinates(.)[,2])

# make a separate dataframe for each variable so I can keep as many sites as possible for each
fk_sal <- wqmn %>% select(SITE, YEAR, MONTH, SAL, geometry, LON_M, LAT_M) %>% 
  filter(!is.na(SAL))
fk_temp <- wqmn %>% select(SITE, YEAR, MONTH, TEMP, geometry, LON_M, LAT_M) %>% 
  filter(!is.na(TEMP))
fk_do <- wqmn %>% select(SITE, YEAR, MONTH, DO, geometry, LON_M, LAT_M) %>% 
  filter(!is.na(DO))


# Biscayne Bay Water Quality (BBWQ) from Miami-Dade County Surface and Groundwater 
# Quality Viewer (https://mdc.maps.arcgis.com/apps/webappviewer/index.html?id=3fd24515ee614f5db63924d7323a4ea7)

# this should be all the data but seems to only be the first 2000 rows...
#BB_data <- read.csv(paste0(source_wd, "Water_Conditions/BiscayneBay_SurfaceWaterSampleResult.csv"))

# salinity
BB_sal_2022 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2022.csv"))
BB_sal_2018 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2018.csv"))
BB_sal_2016 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2016.csv"))
BB_sal_2014 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2014.csv"))

# temperature
BB_temp_2022 <- read.csv(here("Source_Data","Water_Conditions","BB_Temp_2022.csv"))
BB_temp_2018 <- read.csv(here("Source_Data","Water_Conditions","BB_Temp_2018.csv"))
BB_temp_2016 <- read.csv(here("Source_Data","Water_Conditions","BB_Temp_2016.csv"))
BB_temp_2014 <- read.csv(here("Source_Data","Water_Conditions","BB_Temp_2014.csv"))

# dissolved oxygen
BB_do_2122 <- read.csv(here("Source_Data","Water_Conditions","BB_DO_21-22.csv"))
BB_do_1920 <- read.csv(here("Source_Data","Water_Conditions","BB_DO_19-20.csv"))
BB_do_1718 <- read.csv(here("Source_Data","Water_Conditions","BB_DO_17-18.csv"))
BB_do_1516 <- read.csv(here("Source_Data","Water_Conditions","BB_DO_15-16.csv"))
BB_do_14 <- read.csv(here("Source_Data","Water_Conditions","BB_DO_14.csv"))

# append all together
BB_sal <- rbind(BB_sal_2022,BB_sal_2018,BB_sal_2016,BB_sal_2014) %>% 
  rename(SITE = StationUniqueID, DATE = DateCollected, SAL = Value) %>% 
  select(SITE, DATE, x, y, SAL)
BB_temp <- rbind(BB_temp_2022,BB_temp_2018,BB_temp_2016,BB_temp_2014) %>% 
  rename(SITE = StationUniqueID, DATE = DateCollected, TEMP = Value) %>% 
  select(SITE, DATE, x, y, TEMP)
BB_do <- rbind(BB_do_2122,BB_do_1920,BB_do_1718,BB_do_1516,BB_do_14) %>% 
  filter(ParamName != "DO%") %>% # we want values in mg/l not %saturation
  rename(SITE = StationUniqueID, DATE = DateCollected, DO = Value) %>% 
  select(SITE, DATE, x, y, DO)

# clear up some space
rm(WQMN, wqmn)
rm(BB_sal_2022,BB_sal_2018,BB_sal_2016,BB_sal_2014)
rm(BB_temp_2022,BB_temp_2018,BB_temp_2016,BB_temp_2014)
rm(BB_do_2122,BB_do_1920,BB_do_1718,BB_do_1516,BB_do_14)


# make year and month columns, convert to spatial df, and re-project to standard CRS
bb_sal <- BB_sal %>%
  separate(DATE, into = c("MONTH", "DAY", "YEAR"), sep = "/", remove = F) %>%  # separate out date data
  filter(YEAR %in% c("2014","2016","2018","2022")) %>% 
  select(SITE, YEAR, MONTH, x, y, SAL) %>% 
  st_as_sf(., coords = c("x", "y"), crs = bb_crs) %>% 
  st_transform(., new_crs) %>% 
  add_column(LON_M = sf::st_coordinates(.)[,1], # save LON_M and LAT_M columns
             LAT_M = sf::st_coordinates(.)[,2])
bb_temp <- BB_temp %>%
  separate(DATE, into = c("MONTH", "DAY", "YEAR"), sep = "/", remove = F) %>%  # separate out date data
  filter(YEAR %in% c("2014","2016","2018","2022")) %>% 
  select(SITE, YEAR, MONTH, x, y, TEMP) %>% 
  st_as_sf(., coords = c("x", "y"), crs = bb_crs) %>% 
  st_transform(., new_crs) %>% 
  add_column(LON_M = sf::st_coordinates(.)[,1], # save LON_M and LAT_M columns
             LAT_M = sf::st_coordinates(.)[,2])
bb_do <- BB_do %>%
  separate(DATE, into = c("MONTH", "DAY", "YEAR"), sep = "/", remove = F) %>%  # separate out date data
  filter(YEAR %in% c("2014","2016","2018","2022")) %>% # select bottom conditions
  select(SITE, YEAR, MONTH, x, y, DO) %>% 
  st_as_sf(., coords = c("x", "y"), crs = bb_crs) %>% 
  st_transform(., new_crs) %>% 
  add_column(LON_M = sf::st_coordinates(.)[,1], # save LON_M and LAT_M columns
             LAT_M = sf::st_coordinates(.)[,2])

# combining Biscayne Bay data with Florida Keys data
sal_data <- rbind(bb_sal, fk_sal) %>% filter(!is.na(SAL))
temp_data <- rbind(bb_temp, fk_temp) %>% filter(!is.na(TEMP))
do_data <- rbind(bb_do, fk_do) %>% filter(!is.na(DO))

# clean up
rm(BB_do, BB_sal, BB_temp, bb_do, bb_sal, bb_temp, fk_do, fk_sal, fk_temp)



# Calculating Summer and Winter Averages ----------------------------------


# rounding lon and lat values because some coordinates vary by ~0.00000001 m
# and I want those to be considered the same point
sal_data <- sal_data %>% mutate(LON_M = base::round(LON_M, digits = 3),
                                LAT_M = base::round(LAT_M, digits = 3))

temp_data <- temp_data %>% mutate(LON_M = base::round(LON_M, digits = 3),
                                LAT_M = base::round(LAT_M, digits = 3))

do_data <- do_data %>% mutate(LON_M = base::round(LON_M, digits = 3),
                                LAT_M = base::round(LAT_M, digits = 3))

# splitting data into summer (jun, jul, aug) and Winter (dec, jan, feb)
sum_temp <- temp_data %>% filter(MONTH == '6' | MONTH == '7' | MONTH == '8' |
                                   MONTH == '06' | MONTH == '07' | MONTH == '08')
sum_sal <- sal_data %>% filter(MONTH == '6' | MONTH == '7' | MONTH == '8' |
                                 MONTH == '06' | MONTH == '07' | MONTH == '08')
sum_do <- do_data %>% filter(MONTH == '6' | MONTH == '7' | MONTH == '8' |
                               MONTH == '06' | MONTH == '07' | MONTH == '08')

win_temp <- temp_data %>% filter(MONTH == '12' | MONTH == '1' | MONTH == '2' |
                                   MONTH == '01' | MONTH == '02')
win_sal <- sal_data %>% filter(MONTH == '12' | MONTH == '1' | MONTH == '2' |
                                 MONTH == '01' | MONTH == '02')
win_do <- do_data %>% filter(MONTH == '12' | MONTH == '1' | MONTH == '2' |
                               MONTH == '01' | MONTH == '02')

# calculate summer average temp, do, and sal for each site
final_sum_sal <- sum_sal %>%
  group_by(LON_M, LAT_M) %>%
  summarize(SUM_SAL = mean(SAL)) %>% 
  ungroup()
final_sum_temp <- sum_temp %>%
  group_by(LON_M, LAT_M) %>%
  summarize(SUM_TEMP = mean(TEMP)) %>% 
  ungroup()
final_sum_do <- sum_do %>%
  group_by(LON_M, LAT_M) %>%
  summarize(SUM_DO = mean(DO)) %>% 
  ungroup()

# calculate winter average temp, do, and sal for each site
final_win_sal <- win_sal %>%
  group_by(LON_M, LAT_M) %>%
  summarize(WIN_SAL = mean(SAL)) %>% 
  ungroup()
final_win_temp <- win_temp %>%
  group_by(LON_M, LAT_M) %>%
  summarize(WIN_TEMP = mean(TEMP)) %>% 
  ungroup()
final_win_do <- win_do %>%
  group_by(LON_M, LAT_M) %>%
  summarize(WIN_DO = mean(DO)) %>% 
  ungroup()

# clean up storage
rm(do_data, sal_data, temp_data, sum_do, sum_sal, sum_temp, win_do, win_sal, win_temp)



# Writing out datasets ----------------------------------------------------


# write datasets to gis folder
write_csv(final_sum_do, here("Intermediate_Data","Water_Quality","do_summer.csv"), append = FALSE)
write_csv(final_sum_sal, here("Intermediate_Data","Water_Quality","sal_summer.csv"), append = FALSE)
write_csv(final_sum_temp, here("Intermediate_Data","Water_Quality","temp_summer.csv"), append = FALSE)
write_csv(final_win_do, here("Intermediate_Data","Water_Quality","do_winter.csv"), append = FALSE)
write_csv(final_win_sal, here("Intermediate_Data","Water_Quality","sal_winter.csv"), append = FALSE)
write_csv(final_win_temp, here("Intermediate_Data","Water_Quality","temp_winter.csv"), append = FALSE) 

# clean up
rm(final_sum_do, final_sum_sal, final_sum_temp, final_win_do, final_win_sal, final_win_temp)



# Grid for Spatial Interpolation ------------------------------------------


library(gstat)
library(sp)

# make a grid based off of the study region raster
library(raster)
domain_rast <- raster(here::here("Final_Data","Study_Region.tif"))
grid <- domain_rast*0
crs(grid) <- new_crs
plot(grid)
grid <- grid %>% as(., "SpatialPixels")
summary(grid)
rm(domain_rast)

# initiate cluster and divide prediction grid for cores
library(parallel)
n_cores <- 10
cl <- makeCluster(n_cores)
parts <- split(x = 1:length(grid), f = 1:n_cores)
stopCluster(cl)



# SUMMER TEMPERATURE -----------------------------------------------------


### CORRELOGRAMS ###

sum_temp <- read.csv(here("Intermediate_Data","Water_Quality","temp_summer.csv"))
sum_temp_sp <- sum_temp %>% st_drop_geometry()
coordinates(sum_temp_sp) <- ~ LON_M + LAT_M
proj4string(sum_temp_sp) <- new_crs
rm(sum_temp)
proj4string(grid) == proj4string(sum_temp_sp)

# check out summary plots - they are pretty Gaussian! 
hist(sum_temp_sp$SUM_TEMP, nclass=10)
plot(ecdf(sum_temp_sp$SUM_TEMP))

# spatial point plot
spplot(sum_temp_sp, "SUM_TEMP") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
sumtemp_coords <- cbind(sum_temp_sp$LON_M, sum_temp_sp$LAT_M)
colnames(sumtemp_coords) <- c("LON_M","LAT_M")
sumtemp_distmat <- as.matrix(dist(sumtemp_coords))
sumtemp_maxdist <- 2/3 * max(sumtemp_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
sumtemp_corr <- spline.correlog(x = sum_temp_sp$LON_M, y = sum_temp_sp$LAT_M, z = sum_temp_sp$SUM_TEMP,
                                xmax = sumtemp_maxdist, resamp = 100, type = "boot")
 
# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
sumtemp_neigh <- dnearneigh(x = sumtemp_coords, d1 = 0, d2 = 16000, longlat = F)
plot(sumtemp_neigh, coordinates(sumtemp_coords))

# weights matrix for calculating Moran's I value
sumtemp_wts <- nb2listw(neighbours = sumtemp_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(sum_temp_sp$SUM_TEMP, listw = sumtemp_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.25104, p = 1.71e-15 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(sum_temp_sp$SUM_TEMP, listw = sumtemp_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.25104, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of temp correlation

### VARIOGRAMS ###

require(gstat)

# empirical variogram
sumtemp_evgm <- variogram(SUM_TEMP ~ 1, sum_temp_sp, cutoff = sumtemp_maxdist)
plot(sumtemp_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
sumtemp_fvgm <- fit.variogram(sumtemp_evgm, vgm(psill=0.6, model="Sph", range=50000, nugget=0.5))
sumtemp_svgm_plot <- plot(sumtemp_evgm, model = sumtemp_fvgm, xlab = "Distance (m)", pch = 19)
sumtemp_svgm_plot
print(sumtemp_fvgm)

# clean up
rm(sumtemp_coords, sumtemp_corr, sumtemp_distmat, sumtemp_maxdist, sumtemp_neigh, 
   sumtemp_wts, sumtemp_evgm)

### KRIGING ###

cl <- makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("sum_temp_sp", "grid", "parts", "sumtemp_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
sumtemp_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = SUM_TEMP ~ 1, locations = sum_temp_sp, 
        newdata = grid[parts[[x]],], model = sumtemp_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
sumtemp_merge <- rbind(sumtemp_par[[1]], sumtemp_par[[2]])
for (j in 3:length(sumtemp_par)) {
  sumtemp_merge <- rbind(sumtemp_merge, sumtemp_par[[j]])
}
sumtemp_terra <- terra::rast(sumtemp_merge["var1.pred"])
summary(sumtemp_terra)

# plot
terra::plot(sumtemp_terra)
sumtemp_pts <- vect(sum_temp_sp)
terra::points(sumtemp_pts, col = sumtemp_pts$SUM_TEMP)

# save new surface as a .tif and a .asc 
# save points as a .shp
writeRaster(sumtemp_terra, filename = here("Final_Data","Water_Quality","Summer_Temperature.tif"),
            overwrite = T)
sumtemp_raster <- raster::raster(here("Final_Data","Water_Quality","Summer_Temperature.tif"))
raster::writeRaster(sumtemp_raster, filename = here("Final_Data","Predictors_ASCII","Summer_Temperature.asc"), 
            format = "ascii", overwrite = T)
writeVector(sumtemp_pts, filename = here("Final_Data","Water_Quality","Summer_Temperature.shp"),
            overwrite = T)

# clean up 
rm(cl, sumtemp_fvgm, sumtemp_merge, sumtemp_par, sum_temp_sp, sumtemp_terra, sumtemp_pts)

# free unused R memory - garbage collection
gc()



# SUMMER SALINITY --------------------------------------------------------


### CORRELOGRAMS ###

sum_sal <- read.csv(here("Intermediate_Data","Water_Quality","sal_summer.csv"))
sum_sal_sp <- sum_sal %>% st_drop_geometry()
coordinates(sum_sal_sp) <- ~ LON_M + LAT_M
proj4string(sum_sal_sp) <- new_crs
rm(sum_sal)

# check out summary plots - they are NOT very Gaussian! 
hist(sum_sal_sp$SUM_SAL, nclass=10)
plot(ecdf(sum_sal_sp$SUM_SAL))

# spatial point plot
spplot(sum_sal_sp, "SUM_SAL") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
sumsal_coords <- cbind(sum_sal_sp$LON_M, sum_sal_sp$LAT_M)
colnames(sumsal_coords) <- c("LON_M","LAT_M")
sumsal_distmat <- as.matrix(dist(sumsal_coords))
sumsal_maxdist <- 2/3 * max(sumsal_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
sumsal_corr <- spline.correlog(x = sum_sal_sp$LON_M, y = sum_sal_sp$LAT_M, z = sum_sal_sp$SUM_SAL,
                                xmax = sumsal_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
sumsal_neigh <- dnearneigh(x = sumsal_coords, d1 = 0, d2 = 16000, longlat = F)
plot(sumsal_neigh, coordinates(sumsal_coords))

# weights matrix for calculating Moran's I value
sumsal_wts <- nb2listw(neighbours = sumsal_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(sum_sal_sp$SUM_SAL, listw = sumsal_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.50612, p < 2.2e-16 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(sum_sal_sp$SUM_SAL, listw = sumsal_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.50612, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of sal correlation

### VARIOGRAMS ###

# empirical variogram
sumsal_evgm <- variogram(SUM_SAL ~ 1, sum_sal_sp, cutoff = sumsal_maxdist)
plot(sumsal_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
sumsal_fvgm <- fit.variogram(sumsal_evgm, vgm(psill=6, model="Sph", range=150000, nugget=3))
sumsal_svgm_plot <- plot(sumsal_evgm, model = sumsal_fvgm, xlab = "Distance (m)", pch = 19)
sumsal_svgm_plot
print(sumsal_fvgm)

# clean up
rm(sumsal_coords, sumsal_corr, sumsal_distmat, sumsal_maxdist, sumsal_neigh, 
   sumsal_wts, sumsal_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("sum_sal_sp", "grid", "parts", "sumsal_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
sumsal_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = SUM_SAL ~ 1, locations = sum_sal_sp, 
        newdata = grid[parts[[x]],], model = sumsal_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
sumsal_merge <- rbind(sumsal_par[[1]], sumsal_par[[2]])
for (j in 3:length(sumsal_par)) {
  sumsal_merge <- rbind(sumsal_merge, sumsal_par[[j]])
}
sumsal_terra <- terra::rast(sumsal_merge["var1.pred"])
summary(sumsal_terra)

# plot
terra::plot(sumsal_terra)
sumsal_pts <- vect(sum_sal_sp)
terra::points(sumsal_pts, col = sumsal_pts$SUM_SAL)

# save new surface as a .tif and points as a .shp
writeRaster(sumsal_terra, filename = here("Final_Data","Water_Quality","Summer_Salinity.tif"),
            overwrite = T)
sumsal_raster <- raster::raster(here("Final_Data","Water_Quality","Summer_Salinity.tif"))
raster::writeRaster(sumsal_raster, filename = here("Final_Data","Predictors_ASCII","Summer_Salinity.asc"), 
                    format = "ascii", overwrite = T)
writeVector(sumsal_pts, filename = here("Final_Data","Water_Quality","Summer_Salinity.shp"),
            overwrite = T)

# clean up 
rm(cl, sumsal_fvgm, sumsal_merge, sumsal_par, sumsal_terra, sum_sal_sp, sumsal_pts)

# free unused R memory - garbage collection
gc()



# SUMMER DISSOLVED OXYGEN ------------------------------------------------


### CORRELOGRAMS ###

sum_do <- read.csv(here("Intermediate_Data","Water_Quality","do_summer.csv"))
sum_do_sp <- sum_do %>% st_drop_geometry()
coordinates(sum_do_sp) <- ~ LON_M + LAT_M
proj4string(sum_do_sp) <- new_crs
rm(sum_do)

# check out summary plots - they are somewhat Gaussian... 
hist(sum_do_sp$SUM_DO, nclass=10)
plot(ecdf(sum_do_sp$SUM_DO))

# spatial point plot
spplot(sum_do_sp, "SUM_DO") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
sumdo_coords <- cbind(sum_do_sp$LON_M, sum_do_sp$LAT_M)
colnames(sumdo_coords) <- c("LON_M","LAT_M")
sumdo_distmat <- as.matrix(dist(sumdo_coords))
sumdo_maxdist <- 2/3 * max(sumdo_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
sumdo_corr <- spline.correlog(x = sum_do_sp$LON_M, y = sum_do_sp$LAT_M, z = sum_do_sp$SUM_DO,
                              xmax = sumdo_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
sumdo_neigh <- dnearneigh(x = sumdo_coords, d1 = 0, d2 = 16000, longlat = F)
plot(sumdo_neigh, coordinates(sumdo_coords))

# weights matrix for calculating Moran's I value
sumdo_wts <- nb2listw(neighbours = sumdo_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(sum_do_sp$SUM_DO, listw = sumdo_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.203647, p = 6.764e-11 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(sum_do_sp$SUM_DO, listw = sumdo_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.20365, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of do correlation

### VARIOGRAMS ###

# empirical variogram
sumdo_evgm <- variogram(SUM_DO ~ 1, sum_do_sp, cutoff = sumdo_maxdist)
plot(sumdo_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
sumdo_fvgm <- fit.variogram(sumdo_evgm, vgm(psill=0.19, model = "Sph", range=15000, nugget=0.04))
sumdo_svgm_plot <- plot(sumdo_evgm, model = sumdo_fvgm, xlab = "Distance (m)", pch = 19)
sumdo_svgm_plot
print(sumdo_fvgm)

# clean up
rm(sumdo_coords, sumdo_corr, sumdo_distmat, sumdo_maxdist, sumdo_neigh, 
   sumdo_wts, sumdo_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("sum_do_sp", "grid", "parts", "sumdo_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
sumdo_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = SUM_DO ~ 1, locations = sum_do_sp, 
        newdata = grid[parts[[x]],], model = sumdo_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
sumdo_merge <- rbind(sumdo_par[[1]], sumdo_par[[2]])
for (j in 3:length(sumdo_par)) {
  sumdo_merge <- rbind(sumdo_merge, sumdo_par[[j]])
}
sumdo_terra <- terra::rast(sumdo_merge["var1.pred"])
summary(sumdo_terra)

# plot
terra::plot(sumdo_terra)
sumdo_pts <- vect(sum_do_sp)
terra::points(sumdo_pts, col = sumdo_pts$SUM_DO)

# save new surface as a .tif and points as a .shp
writeRaster(sumdo_terra, filename = here("Final_Data","Water_Quality","Summer_Dissolved_Oxygen.tif"),
            overwrite = T)
sumdo_raster <- raster::raster(here("Final_Data","Water_Quality","Summer_Dissolved_Oxygen.tif"))
raster::writeRaster(sumdo_raster, filename = here("Final_Data","Predictors_ASCII","Summer_Dissolved_Oxygen.asc"), 
                    format = "ascii", overwrite = T)
writeVector(sumdo_pts, filename = here("Final_Data","Water_Quality","Summer_Dissolved_Oxygen.shp"),
            overwrite = T)

# clean up 
rm(cl, sumdo_fvgm, sumdo_merge, sumdo_par, sumdo_terra, sum_do_sp, sumdo_pts)

# free unused R memory - garbage collection
gc()



# WINTER TEMPERATURE --------------------------------------------------------


### CORRELOGRAMS ###

win_temp <- read.csv(here("Intermediate_Data","Water_Quality","temp_winter.csv"))
win_temp_sp <- win_temp %>% st_drop_geometry()
coordinates(win_temp_sp) <- ~ LON_M + LAT_M
proj4string(win_temp_sp) <- new_crs
rm(win_temp)

# check out summary plots - they are somewhat Gaussian...
hist(win_temp_sp$WIN_TEMP, nclass=10)
plot(ecdf(win_temp_sp$WIN_TEMP))

# spatial point plot
spplot(win_temp_sp, "WIN_TEMP") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
wintemp_coords <- cbind(win_temp_sp$LON_M, win_temp_sp$LAT_M)
colnames(wintemp_coords) <- c("LON_M","LAT_M")
wintemp_distmat <- as.matrix(dist(wintemp_coords))
wintemp_maxdist <- 2/3 * max(wintemp_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
wintemp_corr <- spline.correlog(x = win_temp_sp$LON_M, y = win_temp_sp$LAT_M, z = win_temp_sp$WIN_TEMP,
                                xmax = wintemp_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
wintemp_neigh <- dnearneigh(x = wintemp_coords, d1 = 0, d2 = 16000, longlat = F)
plot(wintemp_neigh, coordinates(wintemp_coords))

# weights matrix for calculating Moran's I value
wintemp_wts <- nb2listw(neighbours = wintemp_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(win_temp_sp$WIN_TEMP, listw = wintemp_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.60579, p < 2.2e-16 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(win_temp_sp$WIN_TEMP, listw = wintemp_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.60579, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of temp correlation

### VARIOGRAMS ###

# empirical variogram
wintemp_evgm <- variogram(WIN_TEMP ~ 1, win_temp_sp, cutoff = wintemp_maxdist)
plot(wintemp_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
wintemp_fvgm <- fit.variogram(wintemp_evgm, vgm(psill=2.0, model="Sph", range=50000, nugget=0.25))
wintemp_svgm_plot <- plot(wintemp_evgm, model = wintemp_fvgm, xlab = "Distance (m)", pch = 19)
wintemp_svgm_plot
print(wintemp_fvgm)

# clean up
rm(wintemp_coords, wintemp_corr, wintemp_distmat, wintemp_maxdist, wintemp_neigh, 
   wintemp_wts, wintemp_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("win_temp_sp", "grid", "parts", "wintemp_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
wintemp_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = WIN_TEMP ~ 1, locations = win_temp_sp, 
        newdata = grid[parts[[x]],], model = wintemp_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
wintemp_merge <- rbind(wintemp_par[[1]], wintemp_par[[2]])
for (j in 3:length(wintemp_par)) {
  wintemp_merge <- rbind(wintemp_merge, wintemp_par[[j]])
}
wintemp_terra <- terra::rast(wintemp_merge["var1.pred"])
summary(wintemp_terra)

# plot
terra::plot(wintemp_terra)
wintemp_pts <- vect(win_temp_sp)
terra::points(wintemp_pts, col = wintemp_pts$WIN_TEMP)

# save new surface as a .tif and points as a .shp
writeRaster(wintemp_terra, filename = here("Final_Data","Water_Quality","Winter_Temperature.tif"),
            overwrite = T)
wintemp_raster <- raster::raster(here("Final_Data","Water_Quality","Winter_Temperature.tif"))
raster::writeRaster(wintemp_raster, filename = here("Final_Data","Predictors_ASCII","Winter_Temperature.asc"), 
                    format = "ascii", overwrite = T)
writeVector(wintemp_pts, filename = here("Final_Data","Water_Quality","Winter_Temperature.shp"),
            overwrite = T)

# clean up 
rm(cl, wintemp_fvgm, wintemp_merge, wintemp_par, wintemp_terra, win_temp_sp, wintemp_pts)

# free unused R memory - garbage collection
gc()



# WINTER SALINITY ----------------------------------------------------


### CORRELOGRAMS ###

win_sal <- read.csv(here("Intermediate_Data","Water_Quality","sal_winter.csv"))
win_sal_sp <- win_sal %>% st_drop_geometry()
coordinates(win_sal_sp) <- ~ LON_M + LAT_M
proj4string(win_sal_sp) <- new_crs
rm(win_sal)

# check out summary plots - they are NOT AT ALL Gaussian! 
hist(win_sal_sp$WIN_SAL, nclass=10)
plot(ecdf(win_sal_sp$WIN_SAL))

# spatial point plot
spplot(win_sal_sp, "WIN_SAL") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
winsal_coords <- cbind(win_sal_sp$LON_M, win_sal_sp$LAT_M)
colnames(winsal_coords) <- c("LON_M","LAT_M")
winsal_distmat <- as.matrix(dist(winsal_coords))
winsal_maxdist <- 2/3 * max(winsal_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
winsal_corr <- spline.correlog(x = win_sal_sp$LON_M, y = win_sal_sp$LAT_M, z = win_sal_sp$WIN_SAL,
                               xmax = winsal_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
winsal_neigh <- dnearneigh(x = winsal_coords, d1 = 0, d2 = 16000, longlat = F)
plot(winsal_neigh, coordinates(winsal_coords))

# weights matrix for calculating Moran's I value
winsal_wts <- nb2listw(neighbours = winsal_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(win_sal_sp$WIN_SAL, listw = winsal_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.45184, p < 2.2e-16 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(win_sal_sp$WIN_SAL, listw = winsal_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.45184, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of sal correlation

### VARIOGRAMS ###

# empirical variogram
winsal_evgm <- variogram(WIN_SAL ~ 1, win_sal_sp, cutoff = winsal_maxdist)
plot(winsal_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
winsal_fvgm <- fit.variogram(winsal_evgm, vgm(psill=8, model="Sph", range=25000, nugget=2))
winsal_svgm_plot <- plot(winsal_evgm, model = winsal_fvgm, xlab = "Distance (m)", pch = 19)
winsal_svgm_plot
print(winsal_fvgm)

# clean up
rm(winsal_coords, winsal_corr, winsal_distmat, winsal_maxdist, winsal_neigh, 
   winsal_wts, winsal_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("win_sal_sp", "grid", "parts", "winsal_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
winsal_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = WIN_SAL ~ 1, locations = win_sal_sp, 
        newdata = grid[parts[[x]],], model = winsal_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
winsal_merge <- rbind(winsal_par[[1]], winsal_par[[2]])
for (j in 3:length(winsal_par)) {
  winsal_merge <- rbind(winsal_merge, winsal_par[[j]])
}
winsal_terra <- terra::rast(winsal_merge["var1.pred"])
summary(winsal_terra)

# plot
terra::plot(winsal_terra)
winsal_pts <- vect(win_sal_sp)
terra::points(winsal_pts, col = winsal_pts$WIN_SAL)

# save new surface as a .tif and points as a .shp
writeRaster(winsal_terra, filename = here("Final_Data","Water_Quality","Winter_Salinity.tif"),
            overwrite = T)
winsal_raster <- raster::raster(here("Final_Data","Water_Quality","Winter_Salinity.tif"))
raster::writeRaster(winsal_raster, filename = here("Final_Data","Predictors_ASCII","Winter_Salinity.asc"), 
                    format = "ascii", overwrite = T)
writeVector(winsal_pts, filename = here("Final_Data","Water_Quality","Winter_Salinity.shp"),
            overwrite = T)

# clean up 
rm(cl, winsal_fvgm, winsal_merge, winsal_par, winsal_terra, win_sal_sp, winsal_pts)

# free unused R memory - garbage collection
gc()



# WINTER DISSOLVED OXYGEN --------------------------------------------


### CORRELOGRAMS ###

win_do <- read.csv(here("Intermediate_Data","Water_Quality","do_winter.csv"))
win_do_sp <- win_do %>% st_drop_geometry()
coordinates(win_do_sp) <- ~ LON_M + LAT_M
proj4string(win_do_sp) <- new_crs
rm(win_do)

# check out summary plots - they are NOT AT ALL Gaussian! 
hist(win_do_sp$WIN_DO, nclass=10)
plot(ecdf(win_do_sp$WIN_DO))

# spatial point plot
spplot(win_do_sp, "WIN_DO") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
windo_coords <- cbind(win_do_sp$LON_M, win_do_sp$LAT_M)
colnames(windo_coords) <- c("LON_M","LAT_M")
windo_distmat <- as.matrix(dist(windo_coords))
windo_maxdist <- 2/3 * max(windo_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
windo_corr <- spline.correlog(x = win_do_sp$LON_M, y = win_do_sp$LAT_M, z = win_do_sp$WIN_DO,
                              xmax = windo_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
windo_neigh <- dnearneigh(x = windo_coords, d1 = 0, d2 = 16000, longlat = F)
plot(windo_neigh, coordinates(windo_coords))

# weights matrix for calculating Moran's I value
windo_wts <- nb2listw(neighbours = windo_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(win_do_sp$WIN_DO, listw = windo_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.15713, p = 3.581e-7 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(win_do_sp$WIN_DO, listw = windo_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.15713, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of do correlation

### VARIOGRAMS ###

# empirical variogram
windo_evgm <- variogram(WIN_DO ~ 1, win_do_sp, cutoff = windo_maxdist)
plot(windo_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
windo_fvgm <- fit.variogram(windo_evgm, vgm(psill=0.16, model = "Sph", range=15000, nugget=0.02))
windo_svgm_plot <- plot(windo_evgm, model = windo_fvgm, xlab = "Distance (m)", pch = 19)
windo_svgm_plot
print(windo_fvgm)

# clean up
rm(windo_coords, windo_corr, windo_distmat, windo_maxdist, windo_neigh, 
   windo_wts, windo_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("win_do_sp", "grid", "parts", "windo_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
windo_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = WIN_DO ~ 1, locations = win_do_sp, 
        newdata = grid[parts[[x]],], model = windo_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
windo_merge <- rbind(windo_par[[1]], windo_par[[2]])
for (j in 3:length(windo_par)) {
  windo_merge <- rbind(windo_merge, windo_par[[j]])
}
windo_terra <- terra::rast(windo_merge["var1.pred"])
summary(windo_terra)

# plot
terra::plot(windo_terra)
windo_pts <- vect(win_do_sp)
terra::points(windo_pts, col = windo_pts$WIN_DO)

# svar new surface as a .tif 
writeRaster(windo_terra, filename = here("Final_Data","Water_Quality","Winter_Dissolved_Oxygen.tif"),
            overwrite = T)
windo_raster <- raster::raster(here("Final_Data","Water_Quality","Winter_Dissolved_Oxygen.tif"))
raster::writeRaster(windo_raster, filename = here("Final_Data","Predictors_ASCII","Winter_Dissolved_Oxygen.asc"), 
                    format = "ascii", overwrite = T)
writeVector(windo_pts, filename = here("Final_Data","Water_Quality","Winter_Dissolved_Oxygen.shp"),
            overwrite = T)

# clean up 
rm(cl, windo_fvgm, windo_merge, windo_par, windo_terra, win_do_sp)

# free unused R memory - garbage collection
gc()

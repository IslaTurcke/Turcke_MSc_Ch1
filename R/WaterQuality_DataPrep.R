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
library(easypackages) ### gdalUtils is not available for this version of R ###

install_packages("tidyr", "rgdal", "sf", "terra", "tidyverse", 
                 "PNWColors", "tibble", "readxl", "dplyr", "conflicted", "ncf", 
                 "spdep", "maptools")
libraries("here", "tidyr", "rgdal", "sf", "terra", "tidyverse", 
          "PNWColors", "tibble", "readxl", "dplyr", "conflicted", "ncf", 
          "spdep", "maptools") 
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("xlim", "spex")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/MSc_Ch1_DataPrep/R/Occurence_DataPrep.R")

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
  select(SITE, YEAR, LONDEC, LATDEC, TEMP, SAL, DO)

# transform to project coordinate system 
wqmn <- wqmn %>%
  st_as_sf(., coords = c("LONDEC", "LATDEC"), crs = wgs_crs) %>% 
  st_transform(., new_crs) %>% # re-project to standard CRS
  mutate(LON_M = sf::st_coordinates(.)[,1], # save LON_M and LAT_M columns
         LAT_M = sf::st_coordinates(.)[,2])

# make a separate dataframe for each variable so I can keep as many sites as possible for each
fk_sal <- wqmn %>% select(SITE, YEAR, SAL, geometry, LON_M, LAT_M) %>% 
  filter(!is.na(SAL))
fk_temp <- wqmn %>% select(SITE, YEAR, TEMP, geometry, LON_M, LAT_M) %>% 
  filter(!is.na(TEMP))
fk_do <- wqmn %>% select(SITE, YEAR, DO, geometry, LON_M, LAT_M) %>% 
  filter(!is.na(DO))


# Biscayne Bay Water Quality (BBWQ) from Miami-Dade County Surface and Groundwater 
# Quality Viewer (https://mdc.maps.arcgis.com/apps/webappviewer/index.html?id=3fd24515ee614f5db63924d7323a4ea7)

# this should be all the data but seems to only be the first 2000 rows...
#BB_data <- read.csv(paste0(source_wd, "Water_Conditions/BiscayneBay_SurfaceWaterSampleResult.csv"))

# salinity
BB_sal_2022 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2022.csv"))
#BB_sal_2021 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2021.csv"))
#BB_sal_2020 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2020.csv"))
#BB_sal_2019 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2019.csv"))
BB_sal_2018 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2018.csv"))
#BB_sal_2017 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2017.csv"))
BB_sal_2016 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2016.csv"))
#BB_sal_2015 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2015.csv"))
BB_sal_2014 <- read.csv(here("Source_Data","Water_Conditions","BB_Sal_2014.csv"))

# temperature
BB_temp_2022 <- read.csv(here("Source_Data","Water_Conditions","BB_Temp_2022.csv"))
#BB_temp_2021 <- read.csv(paste0(source_wd, "Water_Conditions/BB_Temp_2021.csv"))
#BB_temp_2020 <- read.csv(paste0(source_wd, "Water_Conditions/BB_Temp_2020.csv"))
#BB_temp_2019 <- read.csv(paste0(source_wd, "Water_Conditions/BB_Temp_2019.csv"))
BB_temp_2018 <- read.csv(here("Source_Data","Water_Conditions","BB_Temp_2018.csv"))
#BB_temp_2017 <- read.csv(paste0(source_wd, "Water_Conditions/BB_Temp_2017.csv"))
BB_temp_2016 <- read.csv(here("Source_Data","Water_Conditions","BB_Temp_2016.csv"))
#BB_temp_2015 <- read.csv(paste0(source_wd, "Water_Conditions/BB_Temp_2015.csv"))
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
rm(BB_sal_2022,BB_sal_2021,BB_sal_2020,BB_sal_2019,BB_sal_2018,
   BB_sal_2017,BB_sal_2016,BB_sal_2015,BB_sal_2014)
rm(BB_temp_2022,BB_temp_2021,BB_temp_2020,BB_temp_2019,BB_temp_2018,
   BB_temp_2017,BB_temp_2016,BB_temp_2015,BB_temp_2014)
rm(BB_do_2122,BB_do_1920,BB_do_1718,BB_do_1516,BB_do_14)


# make year column, convert to spatial df, and re-project to standard CRS
bb_sal <- BB_sal %>%
  separate(DATE, into = c("MONTH", "DAY", "YEAR"), sep = "/", remove = F) %>%  # separate out date data
  filter(YEAR %in% c("2014","2016","2018","2022")) %>% 
  select(SITE, YEAR, x, y, SAL) %>% 
  st_as_sf(., coords = c("x", "y"), crs = bb_crs) %>% 
  st_transform(., new_crs) %>% 
  add_column(LON_M = sf::st_coordinates(.)[,1], # save LON_M and LAT_M columns
             LAT_M = sf::st_coordinates(.)[,2])
bb_temp <- BB_temp %>%
  separate(DATE, into = c("MONTH", "DAY", "YEAR"), sep = "/", remove = F) %>%  # separate out date data
  filter(YEAR %in% c("2014","2015","2016","2017","2018","2019","2020","2021","2022")) %>% # select bottom conditions
  select(SITE, YEAR, x, y, TEMP) %>% 
  st_as_sf(., coords = c("x", "y"), crs = bb_crs) %>% 
  st_transform(., new_crs) %>% 
  add_column(LON_M = sf::st_coordinates(.)[,1], # save LON_M and LAT_M columns
             LAT_M = sf::st_coordinates(.)[,2])
bb_do <- BB_do %>%
  separate(DATE, into = c("MONTH", "DAY", "YEAR"), sep = "/", remove = F) %>%  # separate out date data
  filter(YEAR %in% c("2014","2015","2016","2017","2018","2019","2020","2021","2022")) %>% # select bottom conditions
  select(SITE, YEAR, x, y, DO) %>% 
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



# Calculating Summary Stats -----------------------------------------------


# calculate the overall average temp, do, and sal for each site
final_aves_sal <- sal_data %>%
  group_by(LON_M, LAT_M) %>%
  summarize(AVE_SAL = mean(SAL)) %>% 
  ungroup()
final_aves_temp <- temp_data %>%
  group_by(LON_M, LAT_M) %>%
  summarize(AVE_TEMP = mean(TEMP)) %>% 
  ungroup()
final_aves_do <- do_data %>%
  group_by(LON_M, LAT_M) %>%
  summarize(AVE_DO = mean(DO)) %>% 
  ungroup()

# calculate the yearly variance in temp, do, and sal for each site
yr_var_sal <- sal_data %>%
  group_by(LON_M, LAT_M, YEAR) %>%
  summarize(YR_VAR_SAL = var(SAL)) %>% 
  filter(!is.na(YR_VAR_SAL))
yr_var_temp <- temp_data %>%
  group_by(LON_M, LAT_M, YEAR) %>%
  summarize(YR_VAR_TEMP = var(TEMP)) %>% 
  filter(!is.na(YR_VAR_TEMP))
yr_var_do <- do_data %>%
  group_by(LON_M, LAT_M, YEAR) %>%
  summarize(YR_VAR_DO = var(DO)) %>% 
  filter(!is.na(YR_VAR_DO))

# calculate the average yearly standard deviation in temp, do, and sal for each site
final_var_sal <- yr_var_sal %>%
  group_by(LON_M, LAT_M) %>%
  summarize(VAR_SAL = mean(YR_VAR_SAL))
final_var_temp <- yr_var_temp %>%
  group_by(LON_M, LAT_M) %>%
  summarize(VAR_TEMP = mean(YR_VAR_TEMP))
final_var_do <- yr_var_do %>%
  group_by(LON_M, LAT_M) %>%
  summarize(VAR_DO = mean(YR_VAR_DO))

# clean up storage
rm(yr_var_do, yr_var_sal, yr_var_temp, do_data, sal_data, temp_data)



# Writing out datasets ----------------------------------------------------


# write datasets to gis folder
write_csv(final_aves_do, here("Intermediate_Data","Water_Quality","do_ave.csv"), append = FALSE)
write_csv(final_aves_sal, here("Intermediate_Data","Water_Quality","sal_ave.csv"), append = FALSE)
write_csv(final_aves_temp, here("Intermediate_Data","Water_Quality","temp_ave.csv"), append = FALSE)
write_csv(final_var_do, here("Intermediate_Data","Water_Quality","do_var.csv"), append = FALSE)
write_csv(final_var_sal, here("Intermediate_Data","Water_Quality","sal_var.csv"), append = FALSE)
write_csv(final_var_temp, here("Intermediate_Data","Water_Quality","temp_var.csv"), append = FALSE) 



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



# AVERAGE TEMPERATURE -----------------------------------------------------


### CORRELOGRAMS ###

aves_temp <- read.csv(here("Intermediate_Data","Water_Quality","temp_ave.csv"))
temp_ave_sp <- aves_temp %>% st_drop_geometry()
coordinates(temp_ave_sp) <- ~ LON_M + LAT_M
proj4string(temp_ave_sp) <- new_crs
rm(aves_temp)
proj4string(grid) == proj4string(temp_ave_sp)

# check out summary plots - they are pretty Gaussian! 
hist(temp_ave_sp$AVE_TEMP, nclass=10)
plot(ecdf(temp_ave_sp$AVE_TEMP))

# spatial point plot
spplot(temp_ave_sp, "AVE_TEMP") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
tempave_coords <- cbind(temp_ave_sp$LON_M, temp_ave_sp$LAT_M)
colnames(tempave_coords) <- c("LON_M","LAT_M")
tempave_distmat <- as.matrix(dist(tempave_coords))
tempave_maxdist <- 2/3 * max(tempave_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
tempave_corr <- spline.correlog(x = temp_ave_sp$LON_M, y = temp_ave_sp$LAT_M, z = temp_ave_sp$AVE_TEMP,
                                xmax = tempave_maxdist, resamp = 100, type = "boot")
 
# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
tempave_neigh <- dnearneigh(x = tempave_coords, d1 = 0, d2 = 16000, longlat = F)
plot(tempave_neigh, coordinates(tempave_coords))

# weights matrix for calculating Moran's I value
tempave_wts <- nb2listw(neighbours = tempave_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(temp_ave_sp$AVE_TEMP, listw = tempave_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.444684, p < 2.2e-16 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(temp_ave_sp$AVE_TEMP, listw = tempave_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.444680, p = 0.00111 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of temp correlation

### VARIOGRAMS ###

require(gstat)

# empirical variogram
tempave_evgm <- variogram(AVE_TEMP ~ 1, temp_ave_sp, cutoff = tempave_maxdist)
plot(tempave_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
tempave_fvgm <- fit.variogram(tempave_evgm, vgm(psill=0.6, model="Sph", range=150000, nugget=0))
tempave_svgm_plot <- plot(tempave_evgm, model = tempave_fvgm, xlab = "Distance (m)", pch = 19)
tempave_svgm_plot
print(tempave_fvgm)

# clean up
rm(tempave_coords, tempave_corr, tempave_distmat, tempave_maxdist, tempave_neigh, 
   tempave_wts, tempave_evgm, tempave_svgm_plot)

### KRIGING ###

cl <- makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("temp_ave_sp", "grid", "parts", "tempave_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
tempave_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = AVE_TEMP ~ 1, locations = temp_ave_sp, 
        newdata = grid[parts[[x]],], model = tempave_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
tempave_merge <- rbind(tempave_par[[1]], tempave_par[[2]])
for (j in 3:length(tempave_par)) {
  tempave_merge <- rbind(tempave_merge, tempave_par[[j]])
}
tempave_terra <- terra::rast(tempave_merge["var1.pred"])
summary(tempave_terra)

# save new surface as a .tif 
writeRaster(tempave_terra, filename = here("Final_Data","Water_Quality","Temperature_Ave.tif"),
            overwrite = T)

# clean up 
rm(cl, tempave_fvgm, tempave_merge, tempave_par, temp_ave_sp)



# AVERAGE SALINITY --------------------------------------------------------


### CORRELOGRAMS ###

aves_sal <- read.csv(here("Intermediate_Data","Water_Quality","sal_ave.csv"))
sal_ave_sp <- aves_sal %>% st_drop_geometry()
coordinates(sal_ave_sp) <- ~ LON_M + LAT_M
proj4string(sal_ave_sp) <- new_crs
rm(aves_sal)

# check out summary plots - they are NOT very Gaussian! 
hist(sal_ave_sp$AVE_SAL, nclass=10)
plot(ecdf(sal_ave_sp$AVE_SAL))

# spatial point plot
spplot(sal_ave_sp, "AVE_SAL") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
salave_coords <- cbind(sal_ave_sp$LON_M, sal_ave_sp$LAT_M)
colnames(salave_coords) <- c("LON_M","LAT_M")
salave_distmat <- as.matrix(dist(salave_coords))
salave_maxdist <- 2/3 * max(salave_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
salave_corr <- spline.correlog(x = sal_ave_sp$LON_M, y = sal_ave_sp$LAT_M, z = sal_ave_sp$AVE_SAL,
                                xmax = salave_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
salave_neigh <- dnearneigh(x = salave_coords, d1 = 0, d2 = 16000, longlat = F)
plot(salave_neigh, coordinates(salave_coords))

# weights matrix for calculating Moran's I value
salave_wts <- nb2listw(neighbours = salave_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(sal_ave_sp$AVE_SAL, listw = salave_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.452918, p < 2.2e-16 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(sal_ave_sp$AVE_SAL, listw = salave_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.45292, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of sal correlation

### VARIOGRAMS ###

# empirical variogram
salave_evgm <- variogram(AVE_SAL ~ 1, sal_ave_sp, cutoff = salave_maxdist)
plot(salave_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
salave_fvgm <- fit.variogram(salave_evgm, vgm(psill=6, model="Sph", range=150000, nugget=0))
salave_svgm_plot <- plot(salave_evgm, model = salave_fvgm, xlab = "Distance (m)", pch = 19)
salave_svgm_plot
print(salave_fvgm)

# clean up
rm(salave_coords, salave_corr, salave_distmat, salave_maxdist, salave_neigh, 
   salave_wts, salave_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("sal_ave_sp", "grid", "parts", "salave_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
salave_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = AVE_SAL ~ 1, locations = sal_ave_sp, 
        newdata = grid[parts[[x]],], model = salave_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
salave_merge <- rbind(salave_par[[1]], salave_par[[2]])
for (j in 3:length(salave_par)) {
  salave_merge <- rbind(salave_merge, salave_par[[j]])
}
salave_terra <- terra::rast(salave_merge["var1.pred"])
summary(salave_terra)

# save new surface as a .tif 
writeRaster(salave_terra, filename = here("Final_Data","Water_Quality","Salinity_Ave.tif"),
            overwrite = T)

# clean up 
rm(cl, salave_fvgm, salave_merge, salave_par, salave_terra, sal_ave_sp)



# AVERAGE DISSOLVED OXYGEN ------------------------------------------------


### CORRELOGRAMS ###

aves_do <- read.csv(here("Intermediate_Data","Water_Quality","do_ave.csv"))
do_ave_sp <- aves_do %>% st_drop_geometry()
coordinates(do_ave_sp) <- ~ LON_M + LAT_M
proj4string(do_ave_sp) <- new_crs
rm(aves_do)

# check out summary plots - they are somewhat Gaussian... 
hist(do_ave_sp$AVE_DO, nclass=10)
plot(ecdf(do_ave_sp$AVE_DO))

# spatial point plot
spplot(do_ave_sp, "AVE_DO") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
doave_coords <- cbind(do_ave_sp$LON_M, do_ave_sp$LAT_M)
colnames(doave_coords) <- c("LON_M","LAT_M")
doave_distmat <- as.matrix(dist(doave_coords))
doave_maxdist <- 2/3 * max(doave_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
doave_corr <- spline.correlog(x = do_ave_sp$LON_M, y = do_ave_sp$LAT_M, z = do_ave_sp$AVE_DO,
                              xmax = doave_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
doave_neigh <- dnearneigh(x = doave_coords, d1 = 0, d2 = 16000, longlat = F)
plot(doave_neigh, coordinates(doave_coords))

# weights matrix for calculating Moran's I value
doave_wts <- nb2listw(neighbours = doave_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(do_ave_sp$AVE_DO, listw = doave_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.198228, p = 1.445e-11 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(do_ave_sp$AVE_DO, listw = doave_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.19823, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of do correlation

### VARIOGRAMS ###

# empirical variogram
doave_evgm <- variogram(AVE_DO ~ 1, do_ave_sp, cutoff = doave_maxdist)
plot(doave_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
doave_fvgm <- fit.variogram(doave_evgm, vgm(psill=0.06, model = "Sph", range=10000, nugget=0.05))
doave_svgm_plot <- plot(doave_evgm, model = doave_fvgm, xlab = "Distance (m)", pch = 19)
doave_svgm_plot
print(doave_fvgm)

# clean up
rm(doave_coords, doave_corr, doave_distmat, doave_maxdist, doave_neigh, 
   doave_wts, doave_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("do_ave_sp", "grid", "parts", "doave_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
doave_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = AVE_DO ~ 1, locations = do_ave_sp, 
        newdata = grid[parts[[x]],], model = doave_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
doave_merge <- rbind(doave_par[[1]], doave_par[[2]])
for (j in 3:length(doave_par)) {
  doave_merge <- rbind(doave_merge, doave_par[[j]])
}
doave_terra <- terra::rast(doave_merge["var1.pred"])
summary(doave_terra)

# save new surface as a .tif 
writeRaster(doave_terra, filename = here("Final_Data","Water_Quality","Dissolved_Oxygen_Ave.tif"),
            overwrite = T)

# clean up 
rm(cl, doave_fvgm, doave_merge, doave_par, doave_terra, do_ave_sp)



# VARIANCE IN TEMP --------------------------------------------------------


### CORRELOGRAMS ###

var_temp <- read.csv(here("Intermediate_Data","Water_Quality","temp_var.csv"))
temp_var_sp <- var_temp %>% st_drop_geometry()
coordinates(temp_var_sp) <- ~ LON_M + LAT_M
proj4string(temp_var_sp) <- new_crs
rm(var_temp)

# check out summary plots - they are somewhat Gaussian...
hist(temp_var_sp$VAR_TEMP, nclass=10)
plot(ecdf(temp_var_sp$VAR_TEMP))

# spatial point plot
spplot(temp_var_sp, "VAR_TEMP") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
tempvar_coords <- cbind(temp_var_sp$LON_M, temp_var_sp$LAT_M)
colnames(tempvar_coords) <- c("LON_M","LAT_M")
tempvar_distmat <- as.matrix(dist(tempvar_coords))
tempvar_maxdist <- 2/3 * max(tempvar_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
tempvar_corr <- spline.correlog(x = temp_var_sp$LON_M, y = temp_var_sp$LAT_M, z = temp_var_sp$VAR_TEMP,
                                xmax = tempvar_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
tempvar_neigh <- dnearneigh(x = tempvar_coords, d1 = 0, d2 = 16000, longlat = F)
plot(tempvar_neigh, coordinates(tempvar_coords))

# weights matrix for calculating Moran's I value
tempvar_wts <- nb2listw(neighbours = tempvar_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(temp_var_sp$VAR_TEMP, listw = tempvar_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.359817, p < 2.2e-16 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(temp_var_sp$VAR_TEMP, listw = tempvar_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.35982, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of temp correlation

### VARIOGRAMS ###

# empirical variogram
tempvar_evgm <- variogram(VAR_TEMP ~ 1, temp_var_sp, cutoff = tempvar_maxdist)
plot(tempvar_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
tempvar_fvgm <- fit.variogram(tempvar_evgm, vgm(psill=15, model="Sph", range=50000, nugget=3))
tempvar_svgm_plot <- plot(tempvar_evgm, model = tempvar_fvgm, xlab = "Distance (m)", pch = 19)
tempvar_svgm_plot
print(tempvar_fvgm)

# clean up
rm(tempvar_coords, tempvar_corr, tempvar_distmat, tempvar_maxdist, tempvar_neigh, 
   tempvar_wts, tempvar_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("temp_var_sp", "grid", "parts", "tempvar_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
tempvar_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = VAR_TEMP ~ 1, locations = temp_var_sp, 
        newdata = grid[parts[[x]],], model = tempvar_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
tempvar_merge <- rbind(tempvar_par[[1]], tempvar_par[[2]])
for (j in 3:length(tempvar_par)) {
  tempvar_merge <- rbind(tempvar_merge, tempvar_par[[j]])
}
tempvar_terra <- terra::rast(tempvar_merge["var1.pred"])
summary(tempvar_terra)

# svar new surface as a .tif 
writeRaster(tempvar_terra, filename = here("Final_Data","Water_Quality","Temperature_Var.tif"),
            overwrite = T)

# clean up 
rm(cl, tempvar_fvgm, tempvar_merge, tempvar_par, tempvar_terra, temp_var_sp)



# VARIANCE IN SALINITY ----------------------------------------------------


### CORRELOGRAMS ###

var_sal <- read.csv(here("Intermediate_Data","Water_Quality","sal_var.csv"))
sal_var_sp <- var_sal %>% st_drop_geometry()
coordinates(sal_var_sp) <- ~ LON_M + LAT_M
proj4string(sal_var_sp) <- new_crs
rm(var_sal)

# check out summary plots - they are NOT AT ALL Gaussian! 
hist(sal_var_sp$VAR_SAL, nclass=10)
plot(ecdf(sal_var_sp$VAR_SAL))

# spatial point plot
spplot(sal_var_sp, "VAR_SAL") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
salvar_coords <- cbind(sal_var_sp$LON_M, sal_var_sp$LAT_M)
colnames(salvar_coords) <- c("LON_M","LAT_M")
salvar_distmat <- as.matrix(dist(salvar_coords))
salvar_maxdist <- 2/3 * max(salvar_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
salvar_corr <- spline.correlog(x = sal_var_sp$LON_M, y = sal_var_sp$LAT_M, z = sal_var_sp$VAR_SAL,
                               xmax = salvar_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
salvar_neigh <- dnearneigh(x = salvar_coords, d1 = 0, d2 = 16000, longlat = F)
plot(salvar_neigh, coordinates(salvar_coords))

# weights matrix for calculating Moran's I value
salvar_wts <- nb2listw(neighbours = salvar_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(sal_var_sp$VAR_SAL, listw = salvar_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.337261, p < 2.2e-16 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(sal_var_sp$VAR_SAL, listw = salvar_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.33726, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of sal correlation

### VARIOGRAMS ###

# empirical variogram
salvar_evgm <- variogram(VAR_SAL ~ 1, sal_var_sp, cutoff = salvar_maxdist)
plot(salvar_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
salvar_fvgm <- fit.variogram(salvar_evgm, vgm(psill=50, model="Sph", range=10000, nugget=50))
salvar_svgm_plot <- plot(salvar_evgm, model = salvar_fvgm, xlab = "Distance (m)", pch = 19)
salvar_svgm_plot
print(salvar_fvgm)

# clean up
rm(salvar_coords, salvar_corr, salvar_distmat, salvar_maxdist, salvar_neigh, 
   salvar_wts, salvar_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("sal_var_sp", "grid", "parts", "salvar_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
salvar_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = VAR_SAL ~ 1, locations = sal_var_sp, 
        newdata = grid[parts[[x]],], model = salvar_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
salvar_merge <- rbind(salvar_par[[1]], salvar_par[[2]])
for (j in 3:length(salvar_par)) {
  salvar_merge <- rbind(salvar_merge, salvar_par[[j]])
}
salvar_terra <- terra::rast(salvar_merge["var1.pred"])
summary(salvar_terra)

# svar new surface as a .tif 
writeRaster(salvar_terra, filename = here("Final_Data","Water_Quality","Salinity_Var.tif"),
            overwrite = T)

# clean up 
rm(cl, salvar_fvgm, salvar_merge, salvar_par, salvar_terra, sal_var_sp)



# VARIANCE IN DISSOLVED OXYGEN --------------------------------------------


### CORRELOGRAMS ###

var_do <- read.csv(here("Intermediate_Data","Water_Quality","do_var.csv"))
do_var_sp <- var_do %>% st_drop_geometry()
coordinates(do_var_sp) <- ~ LON_M + LAT_M
proj4string(do_var_sp) <- new_crs
rm(var_do)

# check out summary plots - they are NOT AT ALL Gaussian! 
hist(do_var_sp$VAR_DO, nclass=10)
plot(ecdf(do_var_sp$VAR_DO))

# spatial point plot
spplot(do_var_sp, "VAR_DO") # can add: cuts = cuts to choose colour groups

# look at correlograms and Moran's I value to ensure there is spatial dependence
dovar_coords <- cbind(do_var_sp$LON_M, do_var_sp$LAT_M)
colnames(dovar_coords) <- c("LON_M","LAT_M")
dovar_distmat <- as.matrix(dist(dovar_coords))
dovar_maxdist <- 2/3 * max(dovar_distmat) # max distance to consider

# spline correlograms with 95% pointwise bootstrap CIs
dovar_corr <- spline.correlog(x = do_var_sp$LON_M, y = do_var_sp$LAT_M, z = do_var_sp$VAR_DO,
                              xmax = dovar_maxdist, resamp = 100, type = "boot")

# neighbourhood list (neighbours within 16 km so every site has >=1 neighbour)
dovar_neigh <- dnearneigh(x = dovar_coords, d1 = 0, d2 = 16000, longlat = F)
plot(dovar_neigh, coordinates(dovar_coords))

# weights matrix for calculating Moran's I value
dovar_wts <- nb2listw(neighbours = dovar_neigh, style = "W", zero.policy = T)

# Moran's I test under assumption of normality 
moran.test(do_var_sp$VAR_DO, listw = dovar_wts, randomisation = F, zero.policy = T)
# results: Moran's I stat = 0.392182, p < 2.2e-16 --> sig. spatial dependence

# Moran's I with Monte Carlo permutations
moran.mc(do_var_sp$VAR_DO, listw = dovar_wts, nsim = 1000, zero.policy = T)
# results: Moran's I stat = 0.39218, p = 0.000999 --> sig. spatial dependence

# based on Moran's I results we can move on to...
# variogram modeling to capture spatial structure(s) of do correlation

### VARIOGRAMS ###

# empirical variogram
dovar_evgm <- variogram(VAR_DO ~ 1, do_var_sp, cutoff = dovar_maxdist)
plot(dovar_evgm, xlab = "Distance (m)", pch = 19)

# fit variogram
dovar_fvgm <- fit.variogram(dovar_evgm, vgm(psill=0.3, model="Sph", range=1e10, nugget=0.45))
dovar_svgm_plot <- plot(dovar_evgm, model = dovar_fvgm, xlab = "Distance (m)", pch = 19)
dovar_svgm_plot
print(dovar_fvgm)

# clean up
rm(dovar_coords, dovar_corr, dovar_distmat, dovar_maxdist, dovar_neigh, 
   dovar_wts, dovar_evgm)

### KRIGING ###

cl = makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("do_var_sp", "grid", "parts", "dovar_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
dovar_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = VAR_DO ~ 1, locations = do_var_sp, 
        newdata = grid[parts[[x]],], model = dovar_fvgm))
stopCluster(cl)
showConnections()

# combine the resulting part from each parallel core
dovar_merge <- rbind(dovar_par[[1]], dovar_par[[2]])
for (j in 3:length(dovar_par)) {
  dovar_merge <- rbind(dovar_merge, dovar_par[[j]])
}
dovar_terra <- terra::rast(dovar_merge["var1.pred"])
summary(dovar_terra)

# svar new surface as a .tif 
writeRaster(dovar_terra, filename = here("Final_Data","Water_Quality","Dissolved_Oxygen_Var.tif"),
            overwrite = T)

# clean up 
rm(cl, dovar_fvgm, dovar_merge, dovar_par, dovar_terra, do_var_sp)



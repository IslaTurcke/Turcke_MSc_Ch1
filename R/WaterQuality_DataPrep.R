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

install_packages("tidyr", "rgdal", "gdalUtils", "sf", "terra", "tidyverse", 
                 "PNWColors", "tibble", "readxl", "dplyr", "conflicted", "ncf", "spdep")
libraries("here", "tidyr", "rgdal", "gdalUtils", "sf", "terra", "tidyverse", 
          "PNWColors", "tibble", "readxl", "dplyr", "conflicted", "ncf", "spdep") 
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("xlim", "spex")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/MSc_Ch1_DataPrep/R/Occurence_DataPrep.R")

# save PROJ.4 string for NEW and OLD standard projection 
# EPSG:6346 NAD 1983 2011 UTM Zone 17N
new_crs <- crs("+init=epsg:6346")
# ESPG:26958 NAD 83/Florida East (meters)
old_crs <- crs("+init=epsg:26958")

# and source data: 
# EPSG:4326 WGS 84/World Geodetic System 1984 (decdeg)
wgs_crs <- crs("+init=epsg:4326")
# EPSG:2236 NAD83 / Florida East (ftUS))
us_ft <- crs("+init=epsg:2236")
# EPSG:3857 WGS 84 / Pseudo-Mercator
bb_crs <- crs("+init=epsg:3857")

# import study domain shapefile
domain <- vect(here("Final_Data","Study_Region.shp"))



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

# reading in data sets (shortcut for working on next part of script)
aves_do <- read.csv(here("Intermediate_Data","Water_Quality","do_ave.csv"))
aves_sal <- read.csv(here("Intermediate_Data","Water_Quality","sal_ave.csv"))
aves_temp <- read.csv(here("Intermediate_Data","Water_Quality","temp_ave.csv"))
var_do <- read.csv(here("Intermediate_Data","Water_Quality","do_var.csv"))
var_sal <- read.csv(here("Intermediate_Data","Water_Quality","sal_var.csv"))
var_temp <- read.csv(here("Intermediate_Data","Water_Quality","temp_var.csv")) 

# clean up
rm(final_aves_do, final_aves_sal, final_aves_temp, final_var_do, final_var_sal, final_var_temp)



# Beginning of Spatial Interpolation Process ------------------------------


library(gstat)
library(sp)

# convert dataframes to SPDF for correlograms and variogram modeling
# (Spatial Points Data Frame)
sal_ave_sp <- aves_sal %>% st_drop_geometry()
coordinates(sal_ave_sp) <- ~ LON_M + LAT_M
proj4string(sal_ave_sp) <- new_crs
summary(sal_ave_sp)

temp_ave_sp <- aves_temp %>% st_drop_geometry()
coordinates(temp_ave_sp) <- ~ LON_M + LAT_M
proj4string(temp_ave_sp) <- new_crs
summary(temp_ave_sp)

do_ave_sp <- aves_do %>% st_drop_geometry()
coordinates(do_ave_sp) <- ~ LON_M + LAT_M
proj4string(do_ave_sp) <- new_crs
summary(do_ave_sp)

sal_var_sp <- var_sal %>% st_drop_geometry()
coordinates(sal_var_sp) <- ~ LON_M + LAT_M
proj4string(sal_var_sp) <- new_crs
summary(sal_var_sp)

temp_var_sp <- var_temp %>% st_drop_geometry()
coordinates(temp_var_sp) <- ~ LON_M + LAT_M
proj4string(temp_var_sp) <- new_crs
summary(temp_var_sp)

do_var_sp <- var_do %>% st_drop_geometry()
coordinates(do_var_sp) <- ~ LON_M + LAT_M
proj4string(do_var_sp) <- new_crs
summary(do_var_sp)


### AVE TEMP ###

# check out summary plots - they are pretty Gaussian! 
hist(temp_ave_sp$AVE_TEMP, nclass=10)
plot(ecdf(temp_ave_sp$AVE_TEMP))

# set colour thresholds
# cuts <- c() idk how he chose these values

# bubble plot
bubble(temp_ave_sp, "AVE_TEMP", fill = F, maxsize = 2, identify = F)

# spatial point plot
spplot(temp_ave_sp, "AVE_TEMP") # can add: cuts = cuts to choose colour groups

## CORRELOGRAMS & MORAN'S I VALUE ##

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
rm(aves_do, aves_sal, aves_temp, var_do, var_sal, var_temp, cl, domain, domain_rast, tempave_coords, 
   tempave_corr, tempave_distmat, tempave_maxdist, tempave_neigh, tempave_wts,
   tempave_evgm, tempave_svgm_plot)



# Kriging Interpolation ---------------------------------------------------


# make a grid based off of the study region raster
library(raster)
domain_rast <- raster(here::here("Final_Data","Study_Region.tif"))
grid <- domain_rast*0
grid
grid <- grid %>% as(., "SpatialPixels")
proj4string(grid) = proj4string(new_crs)
grid

# initiate cluster and divide prediction grid for cores
library(parallel)
n_cores <- 10
cl <- makeCluster(n_cores)
parts <- split(x = 1:length(grid), f = 1:n_cores)
stopCluster(cl)

### TEMP AVE ###

cl <- makeCluster(n_cores)
clusterExport(cl = cl, varlist = c("temp_ave_sp", "grid", "parts", "tempave_fvgm"),
              envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
tempave_par <- parLapply(cl = cl, X = 1:n_cores, fun = function(x)
  krige(formula = AVE_TEMP ~ 1, locations = temp_ave_sp, 
        newdata = grid[parts[[x]],], model = tempave_fvgm))
stopCluster(cl)
showConnections()

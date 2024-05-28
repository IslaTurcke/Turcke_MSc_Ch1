### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script was used to prepare data for Isla Turcke's first MSc chapter 
# in the lab of Dr. Stephanie Green at the University of Alberta (2022-2024). 
# Data are specific to southern Florida and include bottom water conditions 
# such as temperature, salinity, and dissolved oxygen. These data were used for 
# habitat suitability modeling using maximum entropy.

### TO USE THIS FILE ###
# Before running this R script make sure you have run 
# - the R files "Spatial_DataPrep.R", "WaterQuality_DataPrep.R"
# - and the arcpy file "Seafloor_Morphology.py".

# This file generates the final study region, based on the overlapping 
# non-NA cells of all 17 predictor datasets.
# This file converts 11 of them from .tif files to ASCII files for use in MaxEnt.
# The 6 water quality datasets were already converted in the "WaterQuality_DataPrep.R".

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)



# Set Up ------------------------------------------------------------------


library(easypackages)
libraries("terra","raster","here","sf","sp")

here::i_am("GitHub_Repositories/MSc_Ch1_DataPrep/R/Convert_to_ASCII.R")




# Final Study Region ------------------------------------------------------


# read in initial study region
study_region <- rast(here("Final_Data","Study_Region.tif"))

# read in water quality data sets
sum_temp <- rast(here("Final_Data","Water_Quality","Summer_Temperature.tif"))
sum_sal <- rast(here("Final_Data","Water_Quality","Summer_Salinity.tif"))
sum_do <- rast(here("Final_Data","Water_Quality","Summer_Dissolved_Oxygen.tif"))
win_temp <- rast(here("Final_Data","Water_Quality","Winter_Temperature.tif"))
win_sal <- rast(here("Final_Data","Water_Quality","Winter_Salinity.tif"))
win_do <- rast(here("Final_Data","Water_Quality","Winter_Dissolved_Oxygen.tif"))

# plotting interpolated rasters for initial assessment
terra::plot(sum_temp)
terra::plot(sum_sal, add = T)
terra::plot(sum_do)
terra::plot(win_temp, add = T)
terra::plot(win_sal)
terra::plot(win_do, add = T)
terra::plot(sum_temp)

# some basic checks
ext(study_region)
ext(sum_do)
crs(study_region)
crs(sum_do)

# crop study region raster to have the same extent as the water quality data
study_crop <- terra::crop(study_region, ext(sum_temp))

# mask study region using water quality datasets
final_region <- terra::mask(study_crop, sum_temp)

crs(final_region)
ext(final_region)
CRS <- crs(final_region)
EXT <- ext(final_region)

terra::plot(final_region)
terra::plot(sum_temp, add = T)



# Masking all Rasters using Final Study Region ----------------------------


# read in rasters
depth <- rast(here("Final_Data","Depth.tif"))
habitat <- rast(here("Final_Data","Habitat.tif"))
mg_dist <- rast(here("Final_Data","Mangrove_Distance.tif"))
slope <- rast(here("Final_Data","SlopeDeg.tif"))
curv <- rast(here("Final_Data","Curv.tif"))
plan_curv <- rast(here("Final_Data","PlanCurv.tif"))
prof_curv <- rast(here("Final_Data","ProfileCurv.tif"))
rug_acr <- rast(here("Final_Data","RugosityACR.tif"))
rug_vrm <- rast(here("Final_Data","RugosityVRM.tif"))
bpi_b <- rast(here("Final_Data","BPI_Broad.tif"))
bpi_f <- rast(here("Final_Data","BPI_Fine.tif"))

# set crs just to double check they all match
crs(depth) <- CRS
crs(habitat) <- CRS
crs(mg_dist) <- CRS
crs(slope) <- CRS
crs(curv) <- CRS
crs(plan_curv) <- CRS
crs(prof_curv) <- CRS
crs(rug_acr) <- CRS
crs(rug_vrm) <- CRS
crs(bpi_b) <- CRS
crs(bpi_f) <- CRS

# for some reason Rugosity (ACR) is not aligned with the others
rug_acr <- resample(rug_acr, depth, method = "near")

# crop and mask predictors using final study region raster
DEPTH <- terra::crop(depth, final_region, mask = T)
HABITAT <- terra::crop(habitat, final_region, mask = T)
MG_DIST <- terra::crop(mg_dist, final_region, mask = T)
SLOPE <- terra::crop(slope, final_region, mask = T)
CURV <- terra::crop(curv, final_region, mask = T)
PLAN_CURV <- terra::crop(plan_curv, final_region, mask = T)
PROF_CURV <- terra::crop(prof_curv, final_region, mask = T)
RUG_ACR <- terra::crop(rug_acr, final_region, mask = T)
RUG_VRM <- terra::crop(rug_vrm, final_region, mask = T)
BPI_B <- terra::crop(bpi_b, final_region, mask = T)
BPI_F <- terra::crop(bpi_f, final_region, mask = T)



# Save as ASCII -----------------------------------------------------------


terra::writeRaster(DEPTH, here("Final_Data","Final_ascii","Depth.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(HABITAT, here("Final_Data","Final_ascii","Habitat_Type.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(MG_DIST, here("Final_Data","Final_ascii","Mangrove_Distance.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(SLOPE, here("Final_Data","Final_ascii","Slope.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(CURV, here("Final_Data","Final_ascii","Curvature.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(PLAN_CURV, here("Final_Data","Final_ascii","Planar_Curvature.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(PROF_CURV, here("Final_Data","Final_ascii","Profile_Curvature.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(RUG_ACR, here("Final_Data","Final_ascii","Rugosity_ACR.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(RUG_VRM, here("Final_Data","Final_ascii","Rugosity_VRM.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(BPI_B, here("Final_Data","Final_ascii","BPI_Broad.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(BPI_F, here("Final_Data","Final_ascii","BPI_Fine.asc"),
                   overwrite = T, NAflag = -9999)
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

here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/Convert_to_ASCII.R")

# save PROJ.4 string for NEW projection 
# EPSG:6346 NAD 1983 2011 UTM Zone 17N
new_crs <- crs("+init=epsg:6346")



# Final Study Region ------------------------------------------------------


# read in initial study region and sampling bias file
study_region <- rast(here("Final_Data","Study_Region.tif"))

# read in all predictor data sets
depth <- rast(here("Final_Data","Predictors_GeoTIFFs","Seafloor_Morphology","Depth.tif"))
habitat <- rast(here("Final_Data","Predictors_GeoTIFFs","Habitat.tif"))
mg_dist <- rast(here("Final_Data","Predictors_GeoTIFFs","Mangrove_Distance.tif"))
slope <- rast(here("Final_Data","Predictors_GeoTIFFs","Seafloor_Morphology","SlopeDeg.tif"))
curv <- rast(here("Final_Data","Predictors_GeoTIFFs","Seafloor_Morphology","Curv.tif"))
plan_curv <- rast(here("Final_Data","Predictors_GeoTIFFs","Seafloor_Morphology","PlanCurv.tif"))
prof_curv <- rast(here("Final_Data","Predictors_GeoTIFFs","Seafloor_Morphology","ProfileCurv.tif"))
rug_acr <- rast(here("Final_Data","Predictors_GeoTIFFs","Seafloor_Morphology","RugosityACR.tif"))
rug_vrm <- rast(here("Final_Data","Predictors_GeoTIFFs","Seafloor_Morphology","RugosityVRM.tif"))
bpi_b <- rast(here("Final_Data","Predictors_GeoTIFFs","Seafloor_Morphology","BPI_Broad.tif"))
bpi_f <- rast(here("Final_Data","Predictors_GeoTIFFs","Seafloor_Morphology","BPI_Fine.tif"))
sum_temp <- rast(here("Final_Data","Predictors_GeoTIFFs","Water_Quality","Summer_Temperature.tif"))
sum_sal <- rast(here("Final_Data","Predictors_GeoTIFFs","Water_Quality","Summer_Salinity.tif"))
sum_do <- rast(here("Final_Data","Predictors_GeoTIFFs","Water_Quality","Summer_Dissolved_Oxygen.tif"))
win_temp <- rast(here("Final_Data","Predictors_GeoTIFFs","Water_Quality","Winter_Temperature.tif"))
win_sal <- rast(here("Final_Data","Predictors_GeoTIFFs","Water_Quality","Winter_Salinity.tif"))
win_do <- rast(here("Final_Data","Predictors_GeoTIFFs","Water_Quality","Winter_Dissolved_Oxygen.tif"))

# crop initial study region to smallest extent
study_crop <- terra::crop(study_region, ext(sum_temp))
CRS <- crs(study_crop)
EXT <- ext(study_crop)

# set CRS just to ensure they all match
crs(habitat) <- CRS
crs(mg_dist) <- CRS
crs(depth) <- CRS
crs(slope) <- CRS
crs(curv) <- CRS
crs(plan_curv) <- CRS
crs(prof_curv) <- CRS
crs(rug_acr) <- CRS
crs(rug_vrm) <- CRS
crs(bpi_f) <- CRS
crs(bpi_b) <- CRS
crs(sum_temp) <- CRS
crs(sum_sal) <- CRS
crs(sum_do) <- CRS
crs(win_temp) <- CRS
crs(win_sal) <- CRS
crs(win_do) <- CRS

# resample rasters to match study_crop
habitat <- resample(habitat, study_crop, method = "near")
mg_dist <- resample(mg_dist, study_crop, method = "near")
depth <- resample(depth, study_crop, method = "near")
slope <- resample(slope, study_crop, method = "near")
curv <- resample(curv, study_crop, method = "near")
plan_curv <- resample(plan_curv, study_crop, method = "near")
prof_curv <- resample(prof_curv, study_crop, method = "near")
rug_acr <- resample(rug_acr, study_crop, method = "near")
rug_vrm <- resample(rug_vrm, study_crop, method = "near")
bpi_b <- resample(bpi_b, study_crop, method = "near")
bpi_f <- resample(bpi_f, study_crop, method = "near")
sum_temp <- resample(sum_temp, study_crop, method = "near")
sum_sal <- resample(sum_sal, study_crop, method = "near")
sum_do <- resample(sum_do, study_crop, method = "near")
win_temp <- resample(win_temp, study_crop, method = "near")
win_sal <- resample(win_sal, study_crop, method = "near")
win_do <- resample(win_do, study_crop, method = "near")

# create a stack of all spatial layers that we need to match exactly
stack <- c(habitat, mg_dist, depth, slope, curv, plan_curv, prof_curv, rug_acr,
           rug_vrm, bpi_b, bpi_f, sum_temp, sum_sal, sum_do, win_temp, win_sal, win_do)

names(stack) <- c("Habitat","Mangrove_Dist","Depth","Slope","Curvature","Plan_Curv",
                  "Profile_Curv","ACR_Rugosity","Terrain_Ruggedness","BPI_Broad","BPI_Fine",
                  "Sum_Temp","Sum_Sal","Sum_DO","Win_Temp","Win_Sal","Win_DO")

rm(habitat, mg_dist, depth, slope, curv, plan_curv, prof_curv, rug_acr, rug_vrm,
   bpi_b, bpi_f, sum_temp, sum_sal, sum_do, win_temp, win_sal, win_do, study_region)
gc()

# create a mask layer that has an NA in each cell where ANY layer has NA
my_mask <- any(is.na(stack))
my_mask2 <- sum(stack)

# mask each layer in the stack so all NA cells align
stack_masked <- mask(stack, my_mask, maskvalue = TRUE)
stack_masked2 <- mask(stack, my_mask2)

global(stack_masked, fun = "isNA")
global(stack_masked2, fun = "isNA")



# Final Study Region ------------------------------------------------------


# mask study region using mask
ext(my_mask) == ext(study_crop)
final_region <- mask(study_crop, my_mask, maskvalue = TRUE)

crs(final_region) == CRS
ext(final_region) == EXT

# save final study region
writeRaster(final_region, here("Final_Data","Final_Study_Region.tif"),
            overwrite = T)



# Save as ASCII -----------------------------------------------------------


# extract each layer from the stack and save as ASCII file
terra::writeRaster(DEPTH, here("Final_Data","Predictors_ASCII","Depth.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(HABITAT, here("Final_Data","Predictors_ASCII","Habitat_Type.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(MG_DIST, here("Final_Data","Predictors_ASCII","Mangrove_Distance.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(SLOPE, here("Final_Data","Predictors_ASCII","Slope.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(CURV, here("Final_Data","Predictors_ASCII","Curvature.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(PLAN_CURV, here("Final_Data","Predictors_ASCII","Planar_Curvature.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(PROF_CURV, here("Final_Data","Predictors_ASCII","Profile_Curvature.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(RUG_ACR, here("Final_Data","Predictors_ASCII","Rugosity_ACR.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(RUG_VRM, here("Final_Data","Predictors_ASCII","Rugosity_VRM.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(BPI_B, here("Final_Data","Predictors_ASCII","BPI_Broad.asc"),
                   overwrite = T, NAflag = -9999)
terra::writeRaster(BPI_F, here("Final_Data","Predictors_ASCII","BPI_Fine.asc"),
                   overwrite = T, NAflag = -9999)



# Ensuring Bias file matches crs, ext, res --------------------------------


# read in bias file
bias <- rast(here("Final_Data","Sampling_Bias.tif"))

# set crs and check that extents match
crs(bias) <- CRS
ext(bias) == ext(final_region)

# save bias file as ASCII
writeRaster(bias, here("Final_Data","Sampling_Bias.tif"))








# Masking all Rasters using Final Study Region ----------------------------


# for some reason Rugosity (ACR) is not aligned with the others


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




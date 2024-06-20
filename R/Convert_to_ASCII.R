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

# Set working directory
setwd("Z:/Isla_MSc_Ch1/")

# change where large temporary rasters are stored
terraOptions(tempdir = "Z:/Isla_MSc_Ch1/Temp/")
rasterOptions(tmpdir = "Z:/Isla_MSc_Ch1/Temp/")

# Set up for relative paths
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/Convert_to_ASCII.R")

# save PROJ.4 string for NEW projection 
# EPSG:6346 NAD 1983 2011 UTM Zone 17N
new_crs <- crs("+init=epsg:6346")

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



# Aligning NA Cells -------------------------------------------------------


# find smallest extent
xmin <- max(ext(habitat)[1],ext(mg_dist)[1],ext(depth)[1],ext(slope)[1],ext(curv)[1],
            ext(plan_curv)[1],ext(prof_curv)[1],ext(rug_acr)[1],ext(rug_vrm)[1],
            ext(bpi_b)[1],ext(bpi_f)[1],ext(sum_temp)[1],ext(sum_sal)[1],ext(sum_do)[1],
            ext(win_temp)[1],ext(win_sal)[1],ext(win_do)[1])
xmax <- min(ext(habitat)[2],ext(mg_dist)[2],ext(depth)[2],ext(slope)[2],ext(curv)[2],
            ext(plan_curv)[2],ext(prof_curv)[2],ext(rug_acr)[2],ext(rug_vrm)[2],
            ext(bpi_b)[2],ext(bpi_f)[2],ext(sum_temp)[2],ext(sum_sal)[2],ext(sum_do)[2],
            ext(win_temp)[2],ext(win_sal)[2],ext(win_do)[2])
ymin <- max(ext(habitat)[3],ext(mg_dist)[3],ext(depth)[3],ext(slope)[3],ext(curv)[3],
            ext(plan_curv)[3],ext(prof_curv)[3],ext(rug_acr)[3],ext(rug_vrm)[3],
            ext(bpi_b)[3],ext(bpi_f)[3],ext(sum_temp)[3],ext(sum_sal)[3],ext(sum_do)[3],
            ext(win_temp)[3],ext(win_sal)[3],ext(win_do)[3])
ymax <- min(ext(habitat)[4],ext(mg_dist)[4],ext(depth)[4],ext(slope)[4],ext(curv)[4],
            ext(plan_curv)[4],ext(prof_curv)[4],ext(rug_acr)[4],ext(rug_vrm)[4],
            ext(bpi_b)[4],ext(bpi_f)[4],ext(sum_temp)[4],ext(sum_sal)[4],ext(sum_do)[4],
            ext(win_temp)[4],ext(win_sal)[4],ext(win_do)[4])
EXT <- ext(xmin, xmax, ymin, ymax)
rm(xmin, xmax, ymin, ymax)

# crop initial study region to smallest extent
study_crop <- terra::crop(study_region, EXT)
CRS <- crs(study_crop)

# # project to match CRS, resolution, and origin 
# habitat <- terra::project(habitat, study_crop, method = "near", align = T)
# mg_dist <- terra::project(mg_dist, study_crop, method = "near", align = T)
# depth <- terra::project(depth, study_crop, method = "near", align = T)
# slope <- terra::project(slope, study_crop, method = "near", align = T)
# curv <- terra::project(curv, study_crop, method = "near", align = T)
# plan_curv <- terra::project(plan_curv, study_crop, method = "near", align = T)
# prof_curv <- terra::project(prof_curv, study_crop, method = "near", align = T)
# rug_acr <- terra::project(rug_acr, study_crop, method = "near", align = T)
# rug_vrm <- terra::project(rug_vrm, study_crop, method = "near", align = T)
# bpi_b <- terra::project(bpi_b, study_crop, method = "near", align = T)
# bpi_f <- terra::project(bpi_f, study_crop, method = "near", align = T)
# sum_temp <- terra::project(sum_temp, study_crop, method = "near", align = T)
# sum_sal <- terra::project(sum_sal, study_crop, method = "near", align = T)
# sum_do <- terra::project(sum_do, study_crop, method = "near", align = T)
# win_temp <- terra::project(win_temp, study_crop, method = "near", align = T)
# win_sal <- terra::project(win_sal, study_crop, method = "near", align = T)
# win_do <- terra::project(win_do, study_crop, method = "near", align = T)

# crop rasters to match extent of study_crop
habitat <- terra::crop(habitat, study_crop)
mg_dist <- terra::crop(mg_dist, study_crop)
depth <- terra::crop(depth, study_crop)
slope <- terra::crop(slope, study_crop)
curv <- terra::crop(curv, study_crop)
plan_curv <- terra::crop(plan_curv, study_crop)
prof_curv <- terra::crop(prof_curv, study_crop)
rug_acr <- terra::crop(rug_acr, study_crop)
rug_vrm <- terra::crop(rug_vrm, study_crop)
bpi_b <- terra::crop(bpi_b, study_crop)
bpi_f <- terra::crop(bpi_f, study_crop)
sum_temp <- terra::crop(sum_temp, study_crop)
sum_sal <- terra::crop(sum_sal, study_crop)
sum_do <- terra::crop(sum_do, study_crop)
win_temp <- terra::crop(win_temp, study_crop)
win_sal <- terra::crop(win_sal, study_crop)
win_do <- terra::crop(win_do, study_crop)

crs(habitat) == CRS
crs(mg_dist) == CRS
crs(depth) == CRS
crs(slope) == CRS
crs(curv) == CRS
crs(plan_curv) == CRS
crs(prof_curv) == CRS
crs(rug_acr) == CRS
crs(rug_vrm) == CRS
crs(bpi_b) == CRS
crs(bpi_f) == CRS
crs(sum_temp) == CRS
crs(sum_sal) == CRS
crs(sum_do) == CRS
crs(win_temp) == CRS
crs(win_sal) == CRS
crs(win_do) == CRS


# free unused R memory
gc()

# ### WRITING OUT AND READING BACK IN AS A SCRIPT SHORTCUT ###
# terra::writeRaster(habitat, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Habitat_Aligned.tif"))
# terra::writeRaster(mg_dist, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Mg_Dist_Aligned.tif"))
# terra::writeRaster(depth, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Depth_Aligned.tif"))
# terra::writeRaster(slope, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Slope_Aligned.tif"))
# terra::writeRaster(curv, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Curvature_Aligned.tif"))
# terra::writeRaster(plan_curv, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Plan_Curv_Aligned.tif"))
# terra::writeRaster(prof_curv, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Profile_Curv_Aligned.tif"))
# terra::writeRaster(rug_acr, here("Final_Data", "Predictors_GeoTIFFs_Aligned","ACR_Rugosity_Aligned.tif"))
# terra::writeRaster(rug_vrm, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Terrain_Ruggedness_Aligned.tif"))
# terra::writeRaster(bpi_b, here("Final_Data", "Predictors_GeoTIFFs_Aligned","BPI_Broad_Aligned.tif"))
# terra::writeRaster(bpi_f, here("Final_Data", "Predictors_GeoTIFFs_Aligned","BPI_Fine_Aligned.tif"))
# terra::writeRaster(sum_temp, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Summer_Temp_Aligned.tif"))
# terra::writeRaster(sum_sal, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Summer_Sal_Aligned.tif"))
# terra::writeRaster(sum_do, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Summer_DO_Aligned.tif"))
# terra::writeRaster(win_temp, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Winter_Temp_Aligned.tif"))
# terra::writeRaster(win_sal, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Winter_Sal_Aligned.tif"))
# terra::writeRaster(win_do, here("Final_Data", "Predictors_GeoTIFFs_Aligned","Winter_DO_Aligned.tif"))
# 
# habitat <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Habitat_Aligned.tif"))
# mg_dist <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Mg_Dist_Aligned.tif"))
# depth <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Depth_Aligned.tif"))
# slope <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Slope_Aligned.tif"))
# curv <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Curvature_Aligned.tif"))
# plan_curv <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Plan_Curv_Aligned.tif"))
# prof_curv <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Profile_Curv_Aligned.tif"))
# rug_acr <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","ACR_Rugosity_Aligned.tif"))
# rug_vrm <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Terrain_Ruggedness_Aligned.tif"))
# bpi_b <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","BPI_Broad_Aligned.tif"))
# bpi_f <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","BPI_Fine_Aligned.tif"))
# sum_temp <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Summer_Temp_Aligned.tif"))
# sum_sal <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Summer_Sal_Aligned.tif"))
# sum_do <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Summer_DO_Aligned.tif"))
# win_temp <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Winter_Temp_Aligned.tif"))
# win_sal <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Winter_Sal_Aligned.tif"))
# win_do <- terra::rast(here("Final_Data", "Predictors_GeoTIFFs_Aligned","Winter_DO_Aligned.tif"))

# create a stack of all spatial layers that we need to match exactly
stack <- c(habitat, mg_dist, depth, slope, curv, plan_curv, prof_curv, rug_acr,
           rug_vrm, bpi_b, bpi_f, sum_temp, sum_sal, sum_do, win_temp, win_sal, win_do)

rm(habitat, mg_dist, depth, slope, curv, plan_curv, prof_curv, rug_acr, rug_vrm,
   bpi_b, bpi_f, sum_temp, sum_sal, sum_do, win_temp, win_sal, win_do, study_region)
gc()

# create a mask layer that has an NA in each cell where ANY layer has NA
#my_mask <- any(is.na(stack))
my_mask <- terra::app(stack, fun = sum)
terra::writeRaster(my_mask, here("Final_Data","NA_Predictor_Mask.tif"))

# mask each layer in the stack so all NA cells align
stack_masked <- mask(stack, my_mask)

global(stack_masked, fun = "isNA")
rm(stack)



# Final GeoTIFFs ----------------------------------------------------------


terra::writeRaster(stack_masked[[1]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Habitat_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[2]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Mg_Dist_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[3]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Depth_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[4]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Slope_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[5]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Curvature_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[6]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Plan_Curv_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[7]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Profile_Curv_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[8]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","ACR_Rugosity_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[9]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Terrain_Ruggedness_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[10]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","BPI_Broad_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[11]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","BPI_Fine_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[12]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Summer_Temp_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[13]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Summer_Sal_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[14]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Summer_DO_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[15]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Winter_Temp_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[16]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Winter_Sal_Aligned_NAmatch.tif"))
terra::writeRaster(stack_masked[[17]], here("Final_Data", "Predictors_GeoTIFFs_Aligned_NAmatch","Winter_DO_Aligned_NAmatch.tif"))



# Final Study Region ------------------------------------------------------


# mask study region using mask
ext(my_mask) == ext(study_crop)
final_region <- mask(study_crop, my_mask)
rm(study_crop)

crs(final_region) == CRS
ext(final_region) == EXT

# save final study region
terra::writeRaster(final_region, here("Final_Data","Final_Study_Region.tif"),
            overwrite = T)
rm(final_region, my_mask)



# Save as ASCII -----------------------------------------------------------


# extract each layer from the stack and save as ASCII file
HABITAT <- raster::raster(stack_masked[[1]])
raster::writeRaster(HABITAT, here("Final_Data","Predictors_ASCII","Habitat_Type.asc"),
                    format = "ascii", overwrite = T)
MG_DIST <- raster::raster(stack_masked[[2]])
raster::writeRaster(MG_DIST, here("Final_Data","Predictors_ASCII","Mangrove_Distance.asc"),
                    format = "ascii", overwrite = T)
DEPTH <- raster::raster(stack_masked[[3]])
raster::writeRaster(DEPTH, here("Final_Data","Predictors_ASCII","Depth.asc"),
                    format = "ascii", overwrite = T)
SLOPE <- raster::raster(stack_masked[[4]])
raster::writeRaster(SLOPE, here("Final_Data","Predictors_ASCII","Slope.asc"),
                    format = "ascii", overwrite = T)
CURV <- raster::raster(stack_masked[[5]])
raster::writeRaster(CURV, here("Final_Data","Predictors_ASCII","Curvature.asc"),
                    format = "ascii", overwrite = T)
PLAN <- raster::raster(stack_masked[[6]])
raster::writeRaster(PLAN, here("Final_Data","Predictors_ASCII","Plan_Curvature.asc"),
                    format = "ascii", overwrite = T)
PROF <- raster::raster(stack_masked[[7]])
raster::writeRaster(PROF, here("Final_Data","Predictors_ASCII","Profile_Curvature.asc"),
                    format = "ascii", overwrite = T)
RUGO <- raster::raster(stack_masked[[8]])
raster::writeRaster(RUGO, here("Final_Data","Predictors_ASCII","Rugosity_ACR.asc"),
                    format = "ascii", overwrite = T)
TERR <- raster::raster(stack_masked[[9]])
raster::writeRaster(TERR, here("Final_Data","Predictors_ASCII","Terrain_Ruggedness.asc"),
                    format = "ascii", overwrite = T)
BPI_B <- raster::raster(stack_masked[[10]])
raster::writeRaster(BPI_B, here("Final_Data","Predictors_ASCII","BPI_Broad.asc"),
                    format = "ascii", overwrite = T)
BPI_F <- raster::raster(stack_masked[[11]])
raster::writeRaster(BPI_F, here("Final_Data","Predictors_ASCII","BPI_Fine.asc"),
                    format = "ascii", overwrite = T)
S_TEMP <- raster::raster(stack_masked[[12]])
raster::writeRaster(S_TEMP, here("Final_Data","Predictors_ASCII","Summer_Temperature.asc"),
                    format = "ascii", overwrite = T)
S_SAL <- raster::raster(stack_masked[[13]])
raster::writeRaster(S_SAL, here("Final_Data","Predictors_ASCII","Summer_Salinity.asc"),
                    format = "ascii", overwrite = T)
S_DO <- raster::raster(stack_masked[[14]])
raster::writeRaster(S_DO, here("Final_Data","Predictors_ASCII","Summer_Dissolved_Oxygen.asc"),
                    format = "ascii", overwrite = T)
W_TEMP <- raster::raster(stack_masked[[15]])
raster::writeRaster(W_TEMP, here("Final_Data","Predictors_ASCII","Winter_Temperature.asc"),
                    format = "ascii", overwrite = T)
W_SAL <- raster::raster(stack_masked[[16]])
raster::writeRaster(W_SAL, here("Final_Data","Predictors_ASCII","Winter_Salinity.asc"),
                    format = "ascii", overwrite = T)
W_DO <- raster::raster(stack_masked[[17]])
raster::writeRaster(W_DO, here("Final_Data","Predictors_ASCII","Winter_Dissolved_Oxygen.asc"),
                    format = "ascii", overwrite = T)


# Ensuring Bias file matches crs, ext, res --------------------------------


# read in bias file
bias <- rast(here("Final_Data","Sampling_Bias.tif"))

# project to correct crs
crs(bias)
bias_project <- terra::project(bias, final_region, method = "near", align = T)
crs(bias_project) == CRS

# crop to match extents
bias_crop <- terra::crop(bias_project, final_region)
ext(bias) == EXT

# check resolution
res(bias_crop)

# save bias file
writeRaster(bias, here("Final_Data","Final_Sampling_Bias.tif"))

# convert to ASCII
bias <- raster(here("Final_Data","Final_Sampling_Bias.tif"))
raster::writeRaster(bias, here("Final_Data","Final_Sampling_Bias.asc"),
                     format = "ascii", overwrite = T)



# Convert to GRD files ----------------------------------------------------


# read in final geotiff rasters
habitat <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Habitat_Aligned_NAmatch.tif"))
mg_dist <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Mg_Dist_Aligned_NAmatch.tif"))
depth <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Depth_Aligned_NAmatch.tif"))
slope <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Slope_Aligned_NAmatch.tif"))
curvature <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Curvature_Aligned_NAmatch.tif"))
rug_acr <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","ACR_Rugosity_Aligned_NAmatch.tif"))
bpi_fine <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","BPI_Fine_Aligned_NAmatch.tif"))
bpi_broad <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","BPI_Broad_Aligned_NAmatch.tif"))
sum_temp <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Summer_Temp_Aligned_NAmatch.tif"))
sum_do <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Summer_DO_Aligned_NAmatch.tif"))
win_temp <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Winter_Temp_Aligned_NAmatch.tif"))
win_sal <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Winter_Sal_Aligned_NAmatch.tif"))
win_do <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Winter_DO_Aligned_NAmatch.tif"))

# write out as GRD files
raster::writeRaster(habitat, here("Final_Data","Predictors_GRD","Habitat_Type.grd"),
                    overwrite = T)
raster::writeRaster(mg_dist, here("Final_Data","Predictors_GRD","Mangrove_Distance.grd"),
                    overwrite = T)
raster::writeRaster(depth, here("Final_Data","Predictors_GRD","Depth.grd"),
                    overwrite = T)
raster::writeRaster(slope, here("Final_Data","Predictors_GRD","Slope.grd"),
                    overwrite = T)
raster::writeRaster(curvature, here("Final_Data","Predictors_GRD","Curvature.grd"),
                    overwrite = T)
raster::writeRaster(rug_acr, here("Final_Data","Predictors_GRD","Rugosity_ACR.grd"),
                    overwrite = T)
raster::writeRaster(bpi_broad, here("Final_Data","Predictors_GRD","BPI_Broad.grd"),
                    overwrite = T)
raster::writeRaster(bpi_fine, here("Final_Data","Predictors_GRD","BPI_Fine.grd"),
                    overwrite = T)
raster::writeRaster(sum_temp, here("Final_Data","Predictors_GRD","Summer_Temperature.grd"),
                    overwrite = T)
raster::writeRaster(sum_do, here("Final_Data","Predictors_GRD","Summer_Dissolved_Oxygen.grd"),
                    overwrite = T)
raster::writeRaster(win_temp, here("Final_Data","Predictors_GRD","Winter_Temperature.grd"),
                    overwrite = T)
raster::writeRaster(win_sal, here("Final_Data","Predictors_GRD","Winter_Salinity.grd"),
                    overwrite = T)
raster::writeRaster(win_do, here("Final_Data","Predictors_GRD","Winter_Dissolved_Oxygen.grd"),
                    overwrite = T)

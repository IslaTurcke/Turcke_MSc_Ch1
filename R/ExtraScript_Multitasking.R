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
rm(habitat, mg_dist, depth, slope, curv, plan_curv, prof_curv, rug_acr, rug_vrm,
   bpi_b, bpi_f, sum_temp, sum_sal, sum_do, win_temp, win_sal, win_do)

# crop initial study region to smallest extent
study_crop <- terra::crop(study_region, EXT)
CRS <- crs(study_crop)

# read in mask file, created using all 17 predictor rasters
my_mask <- rast(here("Final_Data","NA_Predictor_Mask.tif"))

# mask study region using mask
ext(my_mask) == ext(study_crop)
final_region <- mask(study_crop, my_mask)
rm(study_crop)

crs(final_region) == CRS
ext(final_region) == EXT

global(final_region, fun = "isNA")

# save final study region
terra::writeRaster(final_region, here("Final_Data","Final_Study_Region.tif"),
            overwrite = T)

# read in bias file
bias <- rast(here("Final_Data","Sampling_Bias.tif"))

# project to correct crs and match extents
crs(bias)
bias_project <- terra::project(bias, final_region, method = "near", align = T)
crs(bias_project) == CRS

bias_crop <- terra::crop(bias_project, final_region)
ext(bias_crop) == EXT

res(bias_crop)

# save bias file as ASCII
writeRaster(bias_crop, here("Final_Data","Final_Sampling_Bias.tif"))

### WELCOME ###

# This script was used to prepare a bathymetry dataset for Isla Turcke's first 
# MSc chapter in the lab of Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida and were subsequently used for habitat
# suitability modeling using maximum entropy models.

### TO USE THIS FILE ###
# This is script 1 of __ in Isla's data analysis pipeline

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)



# Set Up and Organization -------------------------------------------------


# LOAD PACKAGES
library(easypackages)
libraries(here, terra)

# SET RELATIVE PATHS TO DIRECTORIES
gis_wd <- "G:/Isla_HSMs/GIS_Files/" # path to shapefiles
source_wd <- "G:/Isla_HSMs/Source_Data/" # path to source data
cudem_wd <- "G:/Isla_HSMs/GIS_Files/1mCUDEM/" # path to NOAA's CUDEM tiles
lidar_wd <- "G:/Isla_HSMs/GIS_Files/1mDEM/" # path to lidar topography tiles 
spatial_wd <- "G:/Isla_HSMs/Spatial_Predictors/" # path to spatial predictors
model_wd <- "G:/Isla_HSMs/Model_Datasets/" # path to save final data for HSM
species_wd <- "G:/Isla_HSMs/Species_Datasets/" # path to output any final datasets
temp_wd <- "G:/Isla_HSMs/Temporary/"# path to save temporary datasets

# change where large temp rasters are saved
rasterOptions(tmpdir = "G:/Isla_HSMs/Temporary/")

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
# EPSG:3512 NAD83(NSRS2007) / Florida East (US foot)
cudem_crs <- CRS("+init=epsg:3512")
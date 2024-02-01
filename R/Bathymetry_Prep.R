### WELCOME ###

# This is script 1 of __ in Isla's data analysis pipeline.

# This script is used to prepare a bathymetry dataset for Isla Turcke's first 
# MSc chapter in the lab of Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida and were subsequently used for habitat
# suitability modeling using maximum entropy models.

### TO USE THIS FILE ###
# Before running this R script:
# - run the ArcPy model which prepares the two separate DEMs to be merged
# - save the resulting DEMs in your google drive

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)



# Set Up and Organization -------------------------------------------------


# LOAD PACKAGES
library(easypackages)
libraries("here", "terra", "googledrive")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/MSc_Ch1_DataPrep/R/Bathymetry_Prep.R")

# DEFINE COORDINATE SYSTEMS 
# project CRS - EPSG:6346 NAD 1983 (2011) UTM Zone 17N
new_crs <- crs("+init=epsg:6346")

# ACCESS INPUT DEMs FROM GOOGLE DRIVE
# use file ID (ID is in the share link URL)
# lidar_full_1m_m.tif = "paste your file ID here"
# cudem_clip_5m_m.tif = "paste your file ID here"

# NOAA's 1x1m Lidar DEM post Hurricane Irma
drive_download(as_id("1kPwO_2p6OCY1Vgk9YuwrGAgOgQ_HweHO"), overwrite = TRUE, 
               path = "lidar_full_1m_m.tif")
lidar_1x1 <- rast("lidar_full_1m_m.tif")
#file.remove(here("GitHub_Repositories","MSc_Ch1_DataPrep","lidar_full_1m_m.tif"))

# NOAA's 1/9th arc-sec CUDEM resampled to 5x5m
drive_download(as_id("1y1AudWdX_KymFpBYpwcRsAng8Z6F4OPL"), overwrite = TRUE, 
               path = "cudem_clip_5m_m.tif")
cudem_5x5 <- rast("cudem_clip_5m_m.tif")
#file.remove(here("GitHub_Repositories","MSc_Ch1_DataPrep","cudem_clip_5m_m.tif"))



# Resampling LIDAR raster to 5x5m -----------------------------------------


# Set the desired cell size for the output raster
cell_size_5x5 <- c(5, 5)

# Perform resampling with the "mean" method
lidar_5x5 <- aggregate(lidar_1x1, cell_size_5x5, fun = "mean", na.rm = TRUE)

# Save the resampled raster to a new file
writeRaster(lidar_5x5, here("Intermediate_Data","lidar_5x5_terra.tif"), overwrite = TRUE, 
            filetype = "GTiff")

# clear up some space
rm(lidar_1x1)


# Combine with the 5x5m CUDEM ---------------------------------------------


# Find the union of the extents
union_extent <- union(ext(lidar_5x5), ext(cudem_5x5))

# Create an empty raster with the union extent and desired resolution
depth_template <- rast(union_extent, resolution = cell_size_5x5)

# Project and resample the first raster to the output raster
lidar_5x5_resampled <- resample(lidar_5x5, depth_template, method = "bilinear")

# Project and resample the second raster to the output raster
cudem_5x5_resampled <- resample(cudem_5x5, depth_template, method = "bilinear")

# Combine dems into one SpatRast with two layers
dems_5x5 <- c(lidar_5x5_resampled, cudem_5x5_resampled)

# take the average of the two layers
# replace NA values with the corresponding value from the other layer
depth_5x5 <- terra::app(dems_5x5, fun = "mean", na.rm = TRUE)

# Save the combined raster to a new file
writeRaster(depth_5x5, "depth_5x5m_terra_mean.tif", overwrite = TRUE, filetype = "GTiff")
writeRaster(mean_merge, "depth_5x5m_terra_merge.tif", overwrite = TRUE, filetype = "GTiff")

# Print summary information
print(cudem_5x5)
print(lidar_5x5)
print(depth_5x5)
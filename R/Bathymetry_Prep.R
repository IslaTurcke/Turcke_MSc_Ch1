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
#drive_download(as_id("1kPwO_2p6OCY1Vgk9YuwrGAgOgQ_HweHO"), overwrite = TRUE, 
#               path = "lidar_full_1m_m.tif")
#lidar_1x1 <- rast("lidar_full_1m_m.tif")

# NOAA's 1/9th arc-sec CUDEM resampled to 5x5m
#drive_download(as_id("1y1AudWdX_KymFpBYpwcRsAng8Z6F4OPL"), overwrite = TRUE, 
#               path = "cudem_clip_5m_m.tif")
#cudem_5x5 <- rast("cudem_clip_5m_m.tif")



# Build NOAA Lidar Post-Irma DEM from Tiles ----------------------------------

# list of block folders in directory
block_folders_list <- list.files(here("Source_Data","NOAA_IrmaDEM"))

# create an empty list to store DEM blocks as SpatRaster objects
lidar_blocks <- list()

# loop through each block --> read in files, merge into block, add block to list

for (i in seq_along(block_folders_list)){
  
  print(i)
  
  # list all the .tif files in the ith block
  block_files <- list.files(here("Source_Data","NOAA_IrmaDEM",paste0("Block",i)), 
                            pattern = "\\.tif$")
  
  # create an empty list to store tiles as SpatRaster objects
  block_tiles <- list()
  
  # loop through each .tif file --> read, name, add to list
  for (j in seq_along(block_files)) {
    tile <- rast(here("Source_Data","NOAA_IrmaDEM",paste0("Block",i),block_files[j]))
    block_tiles[[j]] <- tile
  }
  rm(tile)
  
  # turn the list of SpatRasters into a SpatRasterCollection (sprc)
  block_sprc <- sprc(block_tiles)
  
  # merge the tiles into one Block and add the Block to the list
  block <- merge(block_sprc)
  lidar_blocks[[i]] <- block
  rm(block_files, block_tiles, block_sprc)
}
rm(block, i, j, block_folders_list)

# turn the list of SpatRasters into a SpatRasterCollection (sprc)
lidar_sprc <- sprc(lidar_blocks)

# merge the five tiles into one raster
lidar_1x1 <- merge(lidar_sprc)

# clean up storage
rm(lidar_blocks, lidar_sprc)

# save the combined raster to a new file
writeRaster(lidar_1x1, here("Intermediate_Data","lidarDEM_full_1x1.tif"), 
            overwrite = TRUE, filetype = "GTiff")



# Resample NOAA's Lidar Post-Irma raster to 5x5m -----------------------------


# set the desired cell size for the output raster
cell_size_5x5 <- c(5, 5)

# perform resampling with the "mean" method
lidar_5x5 <- aggregate(lidar_1x1, cell_size_5x5, fun = "mean", na.rm = TRUE)

# save the resampled raster to a new file
writeRaster(lidar_5x5, here("Intermediate_Data","lidarDEM_full_5x5.tif"), 
            overwrite = TRUE, filetype = "GTiff")

# clear up some space
rm(lidar_1x1)



# Build NOAA's CUDEM from Tiles  -----------------------------------------------

# list all the .tif files in the directory
cudem_files <- list.files(here("Source_Data","NOAA_cudem"), pattern = "\\.tif$")

# create an empty list to store cudem tiles as SpatRaster objects
cudem_tiles <- list()

# loop through each .tif file --> read, name, add to list
for (i in seq_along(cudem_files)) {
  tile <- rast(here("Source_Data","NOAA_cudem",cudem_files[i]))
  cudem_tiles[[i]] <- tile
}
rm(tile)

# turn the list of SpatRasters into a SpatRasterCollection (sprc)
cudem_sprc <- sprc(cudem_tiles)

# merge the five tiles into one raster
cudem_full <- merge(cudem_sprc)

# clean up storage
rm(cudem_files, cudem_tiles, cudem_sprc)

# project to the same coord system as the NOAA post-Irma DEM
cudem_project <- project(cudem_full, lidar_5x5, align = TRUE)

cudem_resample <- resample(cudem_full, lidar_5x5, threads = TRUE)

# Save the combined raster to a new file
writeRaster(cudem_5x5, here("Intermediate_Data","cudem_full_5x5.tif"), 
            overwrite = TRUE, filetype = "GTiff")

# clean up storage
rm(cudem_full)



# Combine NOAA's CUDEM with the Resampled Post-Irma DEM ----------------------


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
writeRaster(depth_5x5, here("Intermediate_Data","depth_5x5.tif"), 
                            overwrite = TRUE, filetype = "GTiff")

# Print summary information
print(cudem_5x5)
print(lidar_5x5)
print(depth_5x5)

# clean up google drive files
#file.remove(here("GitHub_Repositories","MSc_Ch1_DataPrep","cudem_clip_5m_m.tif"))
#file.remove(here("GitHub_Repositories","MSc_Ch1_DataPrep","lidar_full_1m_m.tif"))

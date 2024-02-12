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



# Build NOAA Lidar Post-Irma DEM from Tiles ------------------------------------


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
rm(tile, i)

# turn the list of SpatRasters into a SpatRasterCollection (sprc)
cudem_sprc <- sprc(cudem_tiles)

# merge the five tiles into one raster
cudem_full <- merge(cudem_sprc)

# clean up storage
rm(cudem_files, cudem_tiles, cudem_sprc)

# project to the same coord system as the NOAA post-Irma DEM
cudem_5x5 <- project(cudem_full, lidar_5x5, align = TRUE)

# Save the combined raster to a new file
writeRaster(cudem_5x5, here("Intermediate_Data","cudem_full_5x5.tif"), 
            overwrite = TRUE, filetype = "GTiff")

# clean up storage
rm(cudem_full)



# Combine NOAA's CUDEM with the Resampled post-Irma DEM -----------------------


# combine the partly overlapping SpatRasters to form a single new SpatRaster
# values in overlapping cells are averaged
depth_5x5 <- mosaic(cudem_5x5, lidar_5x5)
print(depth_5x5)

# Save the combined raster to a new file
writeRaster(depth_5x5, here("Intermediate_Data","depth_5x5.tif"), 
                            overwrite = TRUE, filetype = "GTiff")



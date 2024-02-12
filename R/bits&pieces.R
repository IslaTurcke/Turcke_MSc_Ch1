### Bits and pieces of code that I've written but not used in final scripts ###

# Combine NOAA's CUDEM with the Resampled Post-Irma DEM ----------------------


# Find the union of the SpatRaster extents
union_extent <- union(ext(lidar_5x5), ext(cudem_5x5))

# Create an empty raster with the union extent and desired resolution
depth_template <- rast(union_extent, resolution = c(5,5))

# Project and resample the first raster to the output raster
lidar_5x5_resampled <- resample(lidar_5x5, depth_template, method = "bilinear")

# Project and resample the second raster to the output raster
cudem_5x5_resampled <- resample(cudem_5x5, depth_template, method = "bilinear")

# Combine dems into one SpatRast with two layers
dems_5x5 <- c(lidar_5x5_resampled, cudem_5x5_resampled)

# take the average of the two layers
# replace NA values with the corresponding value from the other layer
depth_5x5 <- terra::app(dems_5x5, fun = "mean", na.rm = TRUE)



# GoogleDrive - used through R -------------------------------------------


# ACCESS INPUT DEMs FROM GOOGLE DRIVE
# use file ID (ID is in the share link URL)
# lidar_full_1m_m.tif = "paste your file ID here"
# cudem_clip_5m_m.tif = "paste your file ID here"

# NOAA's 1x1m Lidar DEM post Hurricane Irma
drive_download(as_id("1kPwO_2p6OCY1Vgk9YuwrGAgOgQ_HweHO"), overwrite = TRUE, 
               path = "lidar_full_1m_m.tif")
lidar_1x1 <- rast("lidar_full_1m_m.tif")

# NOAA's 1/9th arc-sec CUDEM resampled to 5x5m
drive_download(as_id("1y1AudWdX_KymFpBYpwcRsAng8Z6F4OPL"), overwrite = TRUE, 
               path = "cudem_clip_5m_m.tif")
cudem_5x5 <- rast("cudem_clip_5m_m.tif")


# remove files downloaded from google drive
file.remove(here("GitHub_Repositories","MSc_Ch1_DataPrep","cudem_clip_5m_m.tif"))
file.remove(here("GitHub_Repositories","MSc_Ch1_DataPrep","lidar_full_1m_m.tif"))

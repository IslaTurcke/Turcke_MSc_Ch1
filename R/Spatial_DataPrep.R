### WELCOME ###

# This is script 1 of __ in Isla's data analysis pipeline.

# This script is used to prepare spatial predictor datasets for Isla Turcke's first 
# MSc chapter in the lab of Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida and were subsequently used for habitat
# suitability modeling using maximum entropy models.
# Specific Spatial Predictors: Bathymetry, Habitat type

### TO USE THIS FILE ###
# Before running this R script:
# - Open the Florida Unified Reef Map geodatabase in ArcGIS (https://myfwc.com/research/gis/regional-projects/unified-reef-map/)
#   and export the "UnifiedReefMap" layer as a shapefile.
# - Save this file as "FLKeys_UnifiedReefMap.shp" in the "Unified_Reef_Map" folder
#   in the "Source_Data" folder.

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)



# Set Up and Organization -------------------------------------------------


# LOAD PACKAGES
library(easypackages)
libraries("here", "terra", "sf", "PNWColors", "googledrive")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/MSc_Ch1_DataPrep/R/Bathymetry_Prep.R")

# DEFINE COORDINATE SYSTEMS 
# project CRS - EPSG:6346 NAD 1983 (2011) UTM Zone 17N
new_crs <- crs("+init=epsg:6346")
old_crs <- crs("+init=epsg:26958")



# Park Boundaries ---------------------------------------------------------


# Read in park/marine sanctuary polygons
# Florida Keys National Marine Sanctuary (FKNMS) shapefile 
# (https://sanctuaries.noaa.gov/library/imast_gis.html)

fknms_vect <- terra::vect(here("Source_Data","Parks","fknms_py.shp"))
crs(fknms_vect) == new_crs # check projection
fknms_vect <- terra::project(fknms_vect, new_crs)
crs(fknms_vect) == new_crs # check projection again

# National Park Service shapefile (for Biscayne National Park (BNP))
# (https://public-nps.opendata.arcgis.com/datasets/nps-boundary-1/data)
nps_vect <- terra::vect(here("Source_Data","Parks","NPS_-_Land_Resources_Division_Boundary_and_Tract_Data_Service.shp"))
bnp_vect <- nps_vect[nps_vect$UNIT_NAME == "Biscayne National Park",] # extract BNP
crs(bnp_vect) == new_crs
bnp_vect <- terra::project(bnp_vect, new_crs)
crs(bnp_vect) == new_crs

# how do the shapefiles line up
#plot(fknms_vect, col = "turquoise")
#plot(bnp_vect, col = "purple", add = T)

# there are gaps at the interior seams where FKNMS and BNP should meet, here is 
# an arbitrary polygon that I constructed in a GIS to fill the interior gaps 
# while still respecting the outer boundaries of the two parks
fill_gaps <- data.frame(
  lon = c(291433.147, 292034.632, 292232.734, 292246.008, 285348.704, 279047.242,
          275554.735, 275369.526, 275054.142, 274774.742, 274393.741, 272105.355,
          270054.830, 269509.126, 268907.462, 268944.004, 269174.427, 270765.899,
          272195.446, 274481.451, 275137.619, 275518.619, 276132.454, 279709.628,
          284056.211, 290049.036, 290684.037),
  lat = c(145564.616, 146169.449, 146170.187, 132196.017, 103890.571, 110144.275, 
          110223.650, 111678.862, 112384.771, 112841.972, 113121.373, 114417.569, 
          114864.055, 115029.419, 114478.159, 114673.867, 115390.709, 115045.427,
          114723.957, 113390.455, 113030.621, 112247.452, 111570.118, 111675.951, 
          107164.796, 137644.857, 145185.497)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = old_crs) %>%
  dplyr::summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
gaps_vect <- terra::vect(fill_gaps)
crs(gaps_vect) == crs(fknms_vect)
gaps_vect <- terra::project(gaps_vect, new_crs)
crs(gaps_vect) == crs(fknms_vect)

# take a look now
#plot(gaps_vect, col = "orange")
#plot(fknms_vect, col = "turquoise", add = T)
#plot(bnp_vect, col = "purple", add = T)

# union the parks and gap fill data to produce one complete polygon
parks_union <- terra::union(fknms_vect, bnp_vect)
parks_union <- terra::union(parks_union, gaps_vect)
parks_vect <- terra::aggregate(parks_union, by = NULL, dissolve = T)

# we now have an outline now of the parks' outer borders! 
#plot(parks_union)
#plot(parks_vect)

# output new parks shapefile to intermediate data folder
writeVector(parks_vect, here("Intermediate_Data","parks_border.shp"), 
            filetype = "ESRI Shapefile", overwrite = TRUE)

# remove temporary data
rm("fknms_vect", "bnp_vect", "nps_vect", "fill_gaps", "gaps_vect", "parks_union", "old_crs")



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
cudem_full_feet <- merge(cudem_sprc)

# convert from feet to meters
cudem_full_m <- cudem_full_feet * 0.3048

# clean up storage
rm(cudem_files, cudem_tiles, cudem_sprc, cudem_full_feet)

# project to the same coord system as the NOAA post-Irma DEM
cudem_5x5 <- project(cudem_full_m, lidar_5x5, align = TRUE)

# Save the combined raster to a new file
writeRaster(cudem_5x5, here("Intermediate_Data","cudem_full_5x5.tif"), 
            overwrite = TRUE, filetype = "GTiff")

# clean up storage
rm(cudem_full_m)



# Combine NOAA's CUDEM with the Resampled post-Irma DEM -----------------------


# combine the partly overlapping SpatRasters to form a single new SpatRaster
# values in overlapping cells are averaged
depth_5x5 <- mosaic(cudem_5x5, lidar_5x5)

# crop the raster to fall within the extent of the parks boundary
depth <- terra::crop(depth_5x5, ext(parks_vect))
plot(depth)

# Save the combined raster to a new file
writeRaster(depth, here("Intermediate_Data","depth_5x5.tif"), 
            overwrite = TRUE, filetype = "GTiff")

# clean up
rm(cudem_5x5, lidar_5x5, depth_5x5, depth)

### END OF BATHYMETRY PREPARATION ... UNTIL FINAL CROPPING LATER



# Habitat Type ------------------------------------------------------------


# Read in Unified Reef Map (URM) feature class
urm <- terra::vect(here("Source_Data","Unified_Reef_Map","FLKeys_UnifiedReefMap.shp"))
crs(urm) == new_crs # check projection
reef_map <- terra::project(urm, new_crs)
crs(reef_map) == new_crs 

# check benthic habitat classes
unique(reef_map$ClassLv1) 

# create IDs because MaxEnt requires categorical variables to be defined 
# numerically, not with words
ClassLv1_list <- unique(reef_map$ClassLv1) # list categories
# create a data.frame of IDs
ClassLv1_df <- data.frame(ID = 1:length(ClassLv1_list), ClassLv1 = ClassLv1_list) 
# match IDs
reef_map$ClassLv1_ID <- ClassLv1_df$ID[match(reef_map$ClassLv1, ClassLv1_df$ClassLv1)] 
unique(reef_map$ClassLv1_ID)   
# save index table for later
write.csv(ClassLv1_df, here("Source_Data", "Unified_Reef_Map", "URM_ClassLv1_IDs.csv"), row.names = F)

# aggregate polygons so there is only 1 per habitat type
reef_map_agg <- terra::aggregate(reef_map, by = "ClassLv1_ID")
#terra::plot(reef_map_agg, "ClassLv1")

# crop unified reef map data to parks shapefile
reef_crop <- terra::crop(reef_map_agg, parks_vect)

# create a palette for plotting benthic habitat classes
pal_benthic <- pnw_palette("Bay", 14, type = "continuous") 

#plot(parks_vect, col = "purple")
#plot(reef_crop, "ClassLv1", add = T)

# output reef habitat shapefile for mapping
writeVector(reef_crop, here("Intermediate_Data","reef_map.shp"), 
            filetype = "ESRI Shapefile", overwrite = TRUE)

# clean up
rm(urm, reef_map, reef_map_agg, ClassLv1_list)


# supplementary shoreline mangrove habitat data
# (https://geodata.myfwc.com/datasets/mangrove-habitat-in-florida-1/explore)
mg_shore <- vect(here("Source_Data","Mangrove_Habitat","Mangrove_Habitat_in_Florida.shp"))
crs(mg_shore) == new_crs
mg_shore <- terra::project(mg_shore, new_crs)
crs(mg_shore) == new_crs

mg_keys <- terra::crop(mg_shore, parks_vect)

# output cropped mangrove data for mapping
writeVector(mg_keys, here("Intermediate_Data","mangrove_map.shp"), 
            filetype = "ESRI Shapefile", overwrite = TRUE)

# there is a small gap between the mangrove data and the reef tract map along 
# the mainland coast, and these missing areas are important coastline mangroves 
# (visible in satellite imagery) add a 100 m buffer around the mangrove data and 
# then use gDifference to keep only the non-overlapping areas (AKA, respect the 
# boundaries of the reef map).
mg_buff <- terra::buffer(mg_keys, width = 100)

# add column to match reef map data, assigning the value 11 for mangrove
mg_buff$ClassLv1_ID <- rep(11, nrow(mg_buff))

rm(mg_shore, mg_keys)

plot(parks_vect, col = "turquoise", border = NULL)
polys(mg_buff, col = "darkgreen", border = "darkgreen")



# Rasterize and Combine Habitat Layers ---------------------------------------


# read in the depth DEM to use as a raster template
depth <- rast(here("Intermediate_Data","depth_5x5.tif"))

# rasterize the reef map and mangrove data sets
reef_rast <- terra::rasterize(reef_crop, depth, field = "ClassLv1_ID")
mg_rast <- terra::rasterize(mg_buff, depth, field = "ClassLv1_ID")


# now merge the reef map and the mangrove data
# reef map data takes precedence over mangrove buffer
reef_mg <- terra::merge(reef_rast, mg_rast, first = T, na.rm = T)

# clean up
rm(reef_crop, reef_rast, mg_buff, mg_rast)

# plot the new reef mangrove raster
plot(reef_mg)
polys(parks_vect, border = "blue", col = NULL)

# filtering out classes "Land" (9) and "Not Classified" (7)
reef_mg[reef_mg == 7 | reef_mg == 9] <- NA

# output combined mangrove and reef raster (not yet cropped to study region)
writeRaster(reef_mg, here("Intermediate_Data","habitat_type_full.tif"), 
            overwrite = TRUE)

### END OF HABITAT TYPE PREPARATION - except for final cropping



# Make Distance to Mangrove Raster ----------------------------------------


# set all values != 11 (mangrove) to NA
mangrove <- reef_mg
mangrove[mangrove != 11] <- NA
mangrove
mangrove_dist <- distance(mangrove)
rm(mangrove)

### END OF DISTANCE TO MANGROVE PREPARATION - except for final cropping



# Final Study Region Raster and Polygon ---------------------------------------


# add depth and habitat rasters together to get all NA values from both
study_rast <- depth + reef_mg

# give all non-NA cells a value of 1
study_rast[!is.na(study_rast)] <- 1
plot(study_rast)

# convert study region raster to a polygon
study_poly <- as.polygons(study_rast, aggregate = T, values = T, na.rm = T)
plot(study_poly)

# save the crs for these final datasets (it seems it changed slightly?)
# I think this one is a compound CRS - horizontal and vertical components
final_crs <- crs(study_poly)

# output final study domain raster and polygon to Final_Data folder
writeRaster(study_rast, here("Final_Data","Study_Region.tif"), 
            overwrite = TRUE)
writeVector(study_poly, here("Final_Data","Study_Region.shp"), 
            filetype = "ESRI Shapefile", overwrite = TRUE)

### END OF STUDY REGION PREPARATION



# Final Cropping for Spatial Datasets -------------------------------------


# cropping depth, habitat type, and distance to mangrove to final study region
depth_crop <- depth * study_rast
habitat_crop <- reef_mg * study_rast
mg_dist_crop <- mangrove_dist * study_rast

# take a look
plot(mg_dist_crop)
polys(study_poly)
polys(parks_vect, border = "red")

# save cropped datasets to Final_Data folder
writeRaster(depth_crop, here("Final_Data","Depth.tif"), 
            overwrite = TRUE)
writeRaster(habitat_crop, here("Final_Data","Habitat.tif"), 
            overwrite = TRUE)
writeRaster(mg_dist_crop, here("Final_Data","Mangrove_Distance.tif"), 
            overwrite = TRUE)
  
# clean up - remove all variables from the environment
rm(list = ls())

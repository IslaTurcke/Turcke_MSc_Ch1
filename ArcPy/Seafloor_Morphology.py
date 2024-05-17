# -*- coding: utf-8 -*-
# ----------------------------------------------------------------
# Created on : 2024-04-30 14:46:57
# Description:
#    Generating metrics of seafloor morphology from a bathymetry 
#    raster for Isla Turcke's first chapter of her MSc.
#    Generated metrics are used a predictors in HSMs for five
#    reef fish species in the Florida Keys.
# Note:
#    Make sure to adapt all paths to your computer.
# Contact:
#    turcke@ualberta.ca
# ----------------------------------------------------------------

# Import modules
import arcpy
from arcpy.sa import *
import os 

# Load required toolboxes
arcpy.ImportToolbox("Z:/Isla_MSc_Ch1/btm-3.0-final/toolbox/btm.pyt")

# Set geoprocessing environments 
arcpy.env.workspace = "Z:/Isla_MSc_Ch1/Geodatabases/Isla_MSc_Ch1.gdb"
print(arcpy.env.workspace)
arcpy.env.scratchWorkspace = "Z:/Isla_MSc_Ch1/Geodatabases/scratch.gdb"
print(arcpy.env.scratchGDB)

# Set output directories
out_tif = "Z:/Isla_MSc_Ch1/Final_Data"
out_gdb = "Z:/Isla_MSc_Ch1/Geodatabases/Isla_MSc_Ch1.gdb"

# Local variables
Isla_gdb = "Z:/Isla_MSc_Ch1/Geodatabases/Isla_MSc_Ch1.gdb"
scratch_gdb = "Z:/Isla_MSc_Ch1/Geodatabases/scratch.gdb"
DEM_tif = "Z:/Isla_MSc_Ch1/Intermediate_Data/depth_5x5.tif"
Rugosity = "Z:/Isla_MSc_Ch1/Final_Data/Rugosity.tif"
Area_Raster = "Z:/Isla_MSc_Ch1/Intermediate_Data/rugosity_area.tif"

# Convert full depth raster tif file to a file in a geodatabase
DEM_gdb = arcpy.RasterToGeodatabase_conversion(DEM_tif, scratch_gdb, "")

# Define spatial reference (projected coordinate system)
proj_crs = arcpy.SpatialReference(6346)
arcpy.DefineReference_management(DEM_tif, proj_crs)

# Process: Slope
Slope = arcpy.Slope(DEM_gdb, "DEGREE", "1", "PLANAR", "METER")
Slope.save(out_gdb)
Slope = Slope + ".tif"
Slope.save(out_tif)

# Process: Surface Area to Planar Area
tempEnvironment0 = arcpy.env.parallelProcessingFactor
arcpy.env.parallelProcessingFactor = "10"
arcpy.surfacetoplanar_btm(DEM_gdb, Rugosity, "true", Area_Raster)
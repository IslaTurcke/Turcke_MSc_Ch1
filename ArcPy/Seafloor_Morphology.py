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

# Import arcpy module
import arcpy

# Import other necessary modules
import os 

# Load required toolboxes
arcpy.ImportToolbox("Z:/Isla_MSc_Ch1/btm-3.0-final/toolbox/btm.pyt")

# Set geoprocessing environments 
arcpy.workspace = "Z:/Isla_MSc_Ch1"
print(arcpy.env.scratchGDB)
arcpy.env.scratchWorkspace = "Z:\\Isla_MSc_Ch1\\Geodatabases\\scratch.gdb"

# Local variables
Isla_scratchGDB = "Z:\\Isla_MSc_Ch1\\Geodatabases\\scratch.gdb"
DEM_tif = "Z:/Isla_MSc_Ch1/Intermediate_Data/depth_5x5.tif"

# Convert full depth raster tif file to a file in a geodatabase
arcpy.RasterToGeodatabase_conversion(DEM_tif, Isla_scratchGDB, "")


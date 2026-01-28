# -*- coding: utf-8 -*-
# ----------------------------------------------------------------
# Created on : 2024-05-22 12:32:55
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
arcpy.ImportToolbox(r"c:\program files\arcgis\pro\Resources\ArcToolbox\toolboxes\Conversion Tools.tbx")
arcpy.ImportToolbox("Z:/Isla_MSc_Ch1/Software/btm-3.0-final/toolbox/btm.pyt")

# Create geodatabase 
Isla_gdb = "Z:/Isla_MSc_Ch1/Geodatabases/Isla_MSc_Ch1.gdb"
if not arcpy.Exists(Isla_gdb) :
    arcpy.CreateFileGDB_management("Z:/Isla_MSc_Ch1/Geodatabases","Isla_MSc_Ch1.gdb")
else :
    print("Geodatabase already exists.")

# Set geoprocessing workspaces
arcpy.env.workspace = "Z:/Isla_MSc_Ch1/Geodatabases/Isla_MSc_Ch1.gdb"
print(arcpy.env.workspace)
arcpy.env.scratchWorkspace = "Z:/Isla_MSc_Ch1/Geodatabases/scratch.gdb"
print(arcpy.env.scratchGDB)

# Set output directories - change to where you want your .tif files and temporary files to go
out_tif = "Z:/Isla_MSc_Ch1/Final_Data/Predictors_GeoTIFFs/Seafloor_Morphology"
scratch_gdb = "Z:\\Isla_MSc_Ch1\\Geodatabases\\scratch.gdb"

# To allow overwriting outputs 
arcpy.env.overwriteOutput = True 

# Check out any necessary licenses.
arcpy.CheckOutExtension("3D")
arcpy.CheckOutExtension("spatial")

# Local variables 
# - change first path to where your depth.tif raster is
# - change second path to where you want this file to go in a geodatabase
depth_tif = "Z:\\Isla_MSc_Ch1\\Intermediate_Data\\depth_5x5.tif"
depth = "Z:\\Isla_MSc_Ch1\\Geodatabases\\scratch.gdb\\depth_5x5"

# Process: Raster To Geodatabase (Raster To Geodatabase) (conversion)
if not arcpy.Exists(depth) :
    Updated_Geodatabase = arcpy.conversion.RasterToGeodatabase(Input_Rasters=[depth_tif], Output_Geodatabase=scratch_gdb)[0]
else :
    print("Depth raster already added to geodatabase.")

# Process: Slope (Slope) (sa)
SlopeDeg = "Z:\\Isla_MSc_Ch1\\Geodatabases\\Isla_MSc_Ch1.gdb\\SlopeDeg"
Slope = SlopeDeg
SlopeDeg = arcpy.sa.Slope(depth, "DEGREE", 1, "PLANAR", "METER", "GPU_THEN_CPU")
SlopeDeg.save(Slope)

# Process: Curvature (Curvature) (sa)
Curv = "Z:\\Isla_MSc_Ch1\\Geodatabases\\Isla_MSc_Ch1.gdb\\Curv"
Curvature = Curv
ProfileCurv = "Z:\\Isla_MSc_Ch1\\Geodatabases\\Isla_MSc_Ch1.gdb\\ProfileCurv"
PlanCurv = "Z:\\Isla_MSc_Ch1\\Geodatabases\\Isla_MSc_Ch1.gdb\\PlanCurv"
Curv = arcpy.sa.Curvature(depth, 1, ProfileCurv, PlanCurv)
Curv.save(Curvature)

ProfileCurv = arcpy.Raster(ProfileCurv)
PlanCurv = arcpy.Raster(PlanCurv)

# Process: Surface Area to Planar Area (Surface Area to Planar Area) (btm)
RugosityACR = "Z:\\Isla_MSc_Ch1\\Geodatabases\\Isla_MSc_Ch1.gdb\\RugosityACR"
area_ACR = "Z:\\Isla_MSc_Ch1\\Geodatabases\\scratch.gdb\\area_ACR"
arcpy.btm.surfacetoplanar(Bathymetry_Raster=depth, Output_Raster=RugosityACR, ACR_Correction=True, Area_Raster=area_ACR)
area_ACR = arcpy.Raster(area_ACR)

# Process: Terrain Ruggedness (VRM) (Terrain Ruggedness (VRM)) (btm)
RugosityVRM = "Z:\\Isla_MSc_Ch1\\Geodatabases\\Isla_MSc_Ch1.gdb\\RugosityVRM"
arcpy.btm.terrainruggedness(Bathymetry_Raster=depth, Neighborhood_Size=3, Output_Raster=RugosityVRM)

# Process: Build Broad Scale BPI (Build Broad Scale BPI) (btm)
BPI_Broad_UnStd = "Z:\\Isla_MSc_Ch1\\Geodatabases\\scratch.gdb\\BPI_Broad_UnStd"
arcpy.btm.broadscalebpi(Input_bathymetric_raster=depth, Inner_radius=25, Outer_radius=250, Scale_factor=1250, Output_raster=BPI_Broad_UnStd)

# Process: Build Fine Scale BPI (Build Fine Scale BPI) (btm)
BPI_Fine_UnStd = "Z:\\Isla_MSc_Ch1\\Geodatabases\\scratch.gdb\\BPI_Fine_UnStd"
arcpy.btm.finescalebpi(Input_bathymetric_raster=depth, Inner_radius=1, Outer_radius=25, Scale_factor=125, Output_raster=BPI_Fine_UnStd)

# Process: Standardize BPIs
BPI_Broad = "Z:\\Isla_MSc_Ch1\\Geodatabases\\Isla_MSc_Ch1.gdb\\BPI_Broad"
BPI_Fine = "Z:\\Isla_MSc_Ch1\\Geodatabases\\Isla_MSc_Ch1.gdb\\BPI_Fine"
arcpy.btm.standardizebpi(Broad_BPI_raster=BPI_Broad_UnStd, Broad_BPI_Mean=0.155891366051146, Broad_BPI_Standard_deviation=1.19084979397663, Output_broad_raster=BPI_Broad, 
                         Fine_BPI_raster=BPI_Fine_UnStd, Fine_BPI_Mean=2.11827029720677E-02, Fine_BPI_Standard_deviation=0.197285409260073, Output_fine_raster=BPI_Fine)


### Convert files to .tif and save in Final Data folder

# list all rasters in geodatabase
raster_list = arcpy.ListRasters("*")

# iterate over each raster
for raster_name in raster_list :
    # construct full path to raster
    raster_path = os.path.join(Isla_gdb, raster_name)
    # construct full output path for .tif file
    output_path = os.path.join(out_tif, raster_name + ".tif")
    # save .tif to folder
    arcpy.CopyRaster_management(raster_path, output_path, "", "", "", "NONE", "NONE", "")

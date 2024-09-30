### Calculating Overlap Metrics for HSM Rasters ###


# Set Up ------------------------------------------------------------------


# install ENMTools, sf, and terra, if necessary
# install.packages(c("devtools", "sf", "terra")); library(devtools); devtools::install_github("danlwarren/ENMTools", force = TRUE)

# set memory available to the java virtual machine
options(java.parameters = "-Xmx128000m")

# load packages
library(easypackages)
libraries("ENMTools","sf","terra","here","tidyverse","dplyr","rJava")
ENMTools::install.extras()

# set working directory
setwd("Z:/Isla_MSc_Ch1/")

# set the Isla_MSc_Ch1 folder as the root directory for relative paths
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/HSM_Analysis.R")

# call on my script of functions based on the ENMTools 
source(here::here("GitHub_Repositories","Turcke_MSc_Ch1","R","My_ENMTools_Functions.R"))

# change where large temporary rasters are saved
terraOptions(tempdir = "Z:/Isla_MSc_Ch1/Temp/")

# set crs
my_crs <- "epsg:6346"

# read in suitability rasters
#bp_suit <- rast(here("HSM_Results","Subadult_BlueParrotfish","SCA_COER_avg.asc"))
#mp_suit <- rast(here("HSM_Results","Subadult_MidnightParrotfish","SCA_COEL_avg.asc"))
rp_suit <- rast(here("HSM_Results","Subadult_RainbowParrotfish","SCA_GUAC_avg.asc"))
#gs_suit <- rast(here("HSM_Results","Subadult_GraySnapper","LUT_GRIS_avg.asc"))
bg_suit <- rast(here("HSM_Results","Subadult_BluestripedGrunt","HAE_SCIU_avg.asc"))

# set crs
#terra::crs(bp_suit) <- my_crs
#terra::crs(mp_suit) <- my_crs
terra::crs(rp_suit) <- my_crs
#terra::crs(gs_suit) <- my_crs
terra::crs(bg_suit) <- my_crs

# make sure min and max are available
# if (hasMinMax(bp_suit) == FALSE) {
#   setMinMax(bp_suit)
# }
# if (hasMinMax(mp_suit) == FALSE) {
#   setMinMax(mp_suit)
# }
if (hasMinMax(rp_suit) == FALSE) {
  setMinMax(rp_suit)
}
# if (hasMinMax(gs_suit) == FALSE) {
#   setMinMax(gs_suit)
# }
if (hasMinMax(bg_suit) == FALSE) {
  setMinMax(bg_suit)
}



# Calculate Raster Overlap ------------------------------------------------


# bp.mp <- my.raster.overlap(bp_suit, mp_suit)
# bp.rp <- my.raster.overlap(bp_suit, rp_suit)
# bp.gs <- my.raster.overlap(bp_suit, gs_suit)
# bp.bg <- my.raster.overlap(bp_suit, bg_suit)
# 
# mp.rp <- my.raster.overlap(mp_suit, rp_suit)
# mp.gs <- my.raster.overlap(mp_suit, gs_suit)
# mp.bg <- my.raster.overlap(mp_suit, bg_suit)
# 
# rp.gs <- my.raster.overlap(rp_suit, gs_suit)
rp.bg <- my.raster.overlap(rp_suit, bg_suit)
# 
# gs.bg <- my.raster.overlap(gs_suit, bg_suit)



# Save Results ----------------------------------------------------------


write.csv(rp.bg, here("HSM_Analysis","Suitability_Overlap","RP_BG_RasterOverlap.csv"), row.names = F)


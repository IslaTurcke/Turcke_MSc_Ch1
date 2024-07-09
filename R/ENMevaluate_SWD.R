### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script was used to prepare data for Isla Turcke's first MSc chapter 
# in the lab of Dr. Stephanie Green at the University of Alberta (2022-2024). 
# Data are specific to southern Florida and include bottom water conditions 
# such as temperature, salinity, and dissolved oxygen. These data were used for 
# habitat suitability modeling using maximum entropy.

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)

### TUNING MAXENT MODEL SETTINGS ###
# This script also uses ENMevaluate() to build models iteratively 
# across a range of user-specified tuning settings.
# These evaluation statistics should help the user identify 
# model settings that balance fit and predictive ability.



# Set Up ------------------------------------------------------------------


# install packages
# install.packages("usdm")
# install.packages("sdmpredictors")

# load packages
library(ENMeval)
library(raster)
library(dplyr)
library(here)

# set working directory
setwd("Z:/Isla_MSc_Ch1/")

# change where large temporary rasters are saved
#rasterOptions(tmpdir = "Z:/Isla_MSc_Ch1/Temp/")
#terraOptions(tempdir = "Z:/Isla_MSc_Ch1/Temp/")
print(tempdir())

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/ENMevaluate_SWD.R")

# set random seed to reproduce results
set.seed(123)



# Load Predictor Rasters --------------------------------------------------


# load from ASCII files (didn't work for ENMevaluate)
habitat <- raster(here("Final_Data","Predictors_ASCII","Habitat_Type.asc"))
mg_dist <- raster(here("Final_Data","Predictors_ASCII","Mangrove_Distance.asc"))
depth <- raster(here("Final_Data","Predictors_ASCII","Depth.asc"))
slope <- raster(here("Final_Data","Predictors_ASCII","Slope.asc"))
curvature <- raster(here("Final_Data","Predictors_ASCII","Curvature.asc"))
rug_acr <- raster(here("Final_Data","Predictors_ASCII","Rugosity_ACR.asc"))
bpi_fine <- raster(here("Final_Data","Predictors_ASCII","BPI_Fine.asc"))
bpi_broad <- raster(here("Final_Data","Predictors_ASCII","BPI_Broad.asc"))
sum_temp <- raster(here("Final_Data","Predictors_ASCII","Summer_Temperature.asc"))
sum_do <- raster(here("Final_Data","Predictors_ASCII","Summer_Dissolved_Oxygen.asc"))
win_temp <- raster(here("Final_Data","Predictors_ASCII","Winter_Temperature.asc"))
win_sal <- raster(here("Final_Data","Predictors_ASCII","Winter_Salinity.asc"))
win_do <- raster(here("Final_Data","Predictors_ASCII","Winter_Dissolved_Oxygen.asc"))

envs <- raster::stack(x = list(habitat, mg_dist, depth, slope, curvature, rug_acr, bpi_broad, bpi_fine,
                      sum_temp, sum_do, win_temp, win_sal, win_do))



# ENM Evaluate ------------------------------------------------------------


# Evaluate what the best settings are for MaxEnt

### FIND/INSTALL JAVA PACKAGE
#install.packages("rJava", dependencies = T)
#install.packages("ENMeval", dependencies = T)
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-22/")

library("rJava")

# need rJava first, find the dismo directory and move the maxent.jar file there (manually)
system.file("java", package = "dismo")

#n_cores <- 10

# set partition settings to k = 10 folds
ps <- list(kfolds = 10)

# read in the background points file
# bg_pts <- read.csv(here("Final_Data","Final_Background_Points.csv"))
# bg.z <- cbind(bg_pts, raster::extract(envs, bg_pts))
# 
# write.csv(bg.z, paste0("Final_Data","Predictors_DataFrame_10000bgpts.csv"))
bg.z <- read.csv(here("Final_Data","Predictors_DataFrame_10000bgpts.csv"), row.names = c(1))



# Midnight Parrotfish ENMeval ----------------------------------------------------


# read in presence only data with only three columns: species, longitude, latitude (in that order)
mp_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_MidnightParrotfish_PO_Full.csv"))[,-1]

# set up occurrence data frame
mp_occs.z <- cbind(mp_PO_full, raster::extract(envs, mp_PO_full))
write.csv(mp_occs.z, here("Final_Data","Predictors_DataFrame_Presencepts.csv"))

# run model evaluation on full PO data with random-k-fold partitioning
mp_enm_eval_swd <- ENMevaluate(mp_occs.z, bg = bg.z, categoricals = "Habitat_Type", 
                               algorithm = "maxent.jar",
                               tune.args = list(fc = c("L","LQ","LQH","LQHP"), 
                                                 rm = c(0.5, 1.0, 2.0, 5.0, 6.0)),
                               partitions = "randomkfold", partition.settings = ps)

write.csv(mp_enm_eval_swd@results, here("ENMevaluate","Subadult_MidnightParrotfish_ENMeval_SWD.csv"))



# Blue Parrotfish ENMeval ----------------------------------------------------


# read in presence only data with only three columns: species, longitude, latitude (in that order)
bp_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BlueParrotfish_PO_Full.csv"))[,-1]

# set up occurrence data frame
bp_occs.z <- cbind(bp_PO_full, raster::extract(envs, bp_PO_full))

# run model evaluation on full PO data with random-k-fold partitioning
bp_enm_eval_swd <- ENMevaluate(bp_occs.z, bg = bg.z, categoricals = "Habitat_Type", 
                               algorithm = "maxent.jar",
                               tune.args = list(fc = c("L","LQ","LQH","LQHP"), 
                                                rm = c(0.5, 1.0, 2.0, 5.0, 6.0)),
                               partitions = "randomkfold", partition.settings = ps)

write.csv(bp_enm_eval_swd@results, here("ENMevaluate","Subadult_BlueParrotfish_ENMeval_SWD.csv"))



# Rainbow Parrotfish ENMeval ----------------------------------------------------


# read in presence only data with only three columns: species, longitude, latitude (in that order)
rp_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_RainbowParrotfish_PO_Full.csv"))[,-1]

# set up occurrence data frame
rp_occs.z <- cbind(rp_PO_full, raster::extract(envs, rp_PO_full))

# run model evaluation on full PO data with random-k-fold partitioning
rp_enm_eval_swd <- ENMevaluate(rp_occs.z, bg = bg.z, categoricals = "Habitat_Type", 
                               algorithm = "maxent.jar",
                               tune.args = list(fc = c("L","LQ","LQH","LQHP"), 
                                                rm = c(0.5, 1.0, 2.0, 5.0, 6.0)),
                               partitions = "randomkfold", partition.settings = ps)

write.csv(rp_enm_eval_swd@results, here("ENMevaluate","Subadult_RainbowParrotfish_ENMeval_SWD.csv"))



# Gray Snapper ENMeval ----------------------------------------------------


# read in presence only data with only three columns: species, longitude, latitude (in that order)
gs_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_GraySnapper_PO_Full.csv"))[,-1]

# set up occurrence data frame
gs_occs.z <- cbind(gs_PO_full, raster::extract(envs, gs_PO_full))

# run model evaluation on full PO data with random-k-fold partitioning
gs_enm_eval_swd <- ENMevaluate(gs_occs.z, bg = bg.z, categoricals = "Habitat_Type", 
                               algorithm = "maxent.jar",
                               tune.args = list(fc = c("L","LQ","LQH","LQHP"), 
                                                rm = c(0.5, 1.0, 2.0, 5.0, 6.0)),
                               partitions = "randomkfold", partition.settings = ps)

write.csv(gs_enm_eval_swd@results, here("ENMevaluate","Subadult_GraySnapper_ENMeval_SWD.csv"))



# Bluestriped Grunt ENMeval -----------------------------------------------


# read in presence only data with only three columns: species, longitude, latitude (in that order)
bg_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BluestripedGrunt_PO_Full.csv"))[,-1]

# set up occurrence data frame
bg_occs.z <- cbind(bg_PO_full, raster::extract(envs, bg_PO_full))

# run model evaluation on full PO data with random-k-fold partitioning
bg_enm_eval_swd <- ENMevaluate(bg_occs.z, bg = bg.z, categoricals = "Habitat_Type", 
                               algorithm = "maxent.jar",
                               tune.args = list(fc = c("L","LQ","LQH","LQHP"), 
                                                rm = c(0.5, 1.0, 2.0, 5.0, 6.0)),
                               partitions = "randomkfold", partition.settings = ps)

write.csv(bg_enm_eval_swd@results, here("ENMevaluate","Subadult_BluestripedGrunt_ENMeval_SWD.csv"))

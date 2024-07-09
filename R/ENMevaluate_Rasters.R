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
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/ENMevaluate_Rasters.R")

# set random seed to reproduce results
set.seed(123)



# Load Predictor Rasters --------------------------------------------------


# load from ASCII files (didn't work for ENMevaluate)
habitat <- raster(here("Final_Data","Predictors_ASCII","Habitat_Type.asc"))
mg_dist <- raster(here("Final_Data","Predictors_ASCII","Mangrove_Distance.asc"))
depth <- raster(here("Final_Data","Predictors_ASCII","Depth.asc"))
slope <- raster(here("Final_Data","Predictors_ASCII","Slope.asc"))
curv <- raster(here("Final_Data","Predictors_ASCII","Curvature.asc"))
rug_acr <- raster(here("Final_Data","Predictors_ASCII","Rugosity_ACR.asc"))
bpi_fine <- raster(here("Final_Data","Predictors_ASCII","BPI_Fine.asc"))
bpi_broad <- raster(here("Final_Data","Predictors_ASCII","BPI_Broad.asc"))
sum_temp <- raster(here("Final_Data","Predictors_ASCII","Summer_Temperature.asc"))
sum_do <- raster(here("Final_Data","Predictors_ASCII","Summer_Dissolved_Oxygen.asc"))
win_temp <- raster(here("Final_Data","Predictors_ASCII","Winter_Temperature.asc"))
win_sal <- raster(here("Final_Data","Predictors_ASCII","Winter_Salinity.asc"))
win_do <- raster(here("Final_Data","Predictors_ASCII","Winter_Dissolved_Oxygen.asc"))

envs <- raster::stack(x = list(habitat, mg_dist, depth, slope, curv, rug_acr, bpi_broad, bpi_fine,
                               sum_temp, sum_do, win_temp, win_sal, win_do))

# envs.files <- list.files(path = here("Final_Data","Predictors_ASCII"), pattern = "asc", full.names = TRUE)
# envs <- raster::stack(envs.files)

# declare habitat as a categorical variable (factor)
# envs$lyr.1.5 <- raster::as.factor(envs$lyr.1.5)

# names(envs) <- c("BPI_Broad","BPI_Fine","Curvature","Depth","Habitat","Mangrove_Dist","Slope",
                        #"ACR_Rugosity","Sum_Temp","Sum_DO",
                        #"Win_Temp","Win_Sal","Win_DO")



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

# set directory for results
enm_wd <- here("ENMevaluate")

# set partition settings to k = 10 folds
ps <- list(kfolds = 10)

# read in the survey bias file 
# this will guide the selection of background points by MaxEnt
# bias <- raster(here("Final_Data","Final_Sampling_Bias.tif"))

# study domain is very large, so select 10,000 background points 
# (the base settings for MaxEnt)
# bg_pts <- as.data.frame(xyFromCell(bias, sample(which(!is.na(values(subset(pred_select, 1)))), 10000,
#                                                prob = values(bias)[!is.na(values(subset(pred_select, 1)))])))
# bg_pts <- bg_pts %>% mutate(longitude = x, latitude = y) %>% select("longitude","latitude")

# save to final data folder just in case
# write.csv(bg_pts, here("Final_Data","Final_Background_Points.csv"), row.names = FALSE)
# rm(bias)

bg_pts <- read.csv(here("Final_Data","Final_Background_Points.csv"))



# Midnight Parrotfish ENMeval ----------------------------------------------------


# read in presence only data with only three columns: species, longitude, latitude (in that order)
mp_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_MidnightParrotfish_PO_Full.csv"))[,-1]

# run model evaluation on full PO data with random-k-fold partitioning
mp_enm_eval_full <- ENMevaluate(mp_PO_full, envs, bg = bg_pts, algorithm = "maxent.jar",
                                tune.args = list(fc = c("L","LQ"), 
                                                 rm = 1:2),
                                partitions = "randomkfold", partition.settings = ps)

write.csv(mp_enm_eval_full@results, paste(enm_wd, "Subadult_MidnightParrotfish_ENMeval.csv"))



# Blue Parrotfish ENMeval ----------------------------------------------------


# read in presence only data with only three columns: species, longitude, latitude (in that order)
bp_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BlueParrotfish_PO_Full.csv"))[,-1]

# run model evaluation on full PO data with random-k-fold partitioning
bp_enm_eval_full <- ENMevaluate(bp_PO_full, pred_select, bg = bg_pts, 
                                tune.args = list(fc = c("L", "LQ", "LQH", "LQHP"), 
                                                 rm = c(0.50, 1.0, 2.0, 5.0, 6.0)),
                                partitions = "randomkfold", partition.settings = ps, 
                                algorithm = "maxent.jar", categoricals = "Habitat", 
                                parallel = TRUE, numCores = n_cores)

write.csv(bp_enm_eval_full@results, paste(enm_wd, "Subadult_BlueParrotfish_ENMeval.csv"))



# Rainbow Parrotfish ENMeval ----------------------------------------------------


# read in presence only data with only three columns: species, longitude, latitude (in that order)
rp_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_RainbowParrotfish_PO_Full.csv"))[,-1]

# run model evaluation on full PO data with random-k-fold partitioning
rp_enm_eval_full <- ENMevaluate(rp_PO_full, pred_select, bg = bg_pts, 
                                tune.args = list(fc = c("L", "LQ", "LQH", "LQHP"), 
                                                 rm = c(0.50, 1.0, 2.0, 5.0, 6.0)),
                                partitions = "randomkfold", partition.settings = ps, 
                                algorithm = "maxent.jar", categoricals = "Habitat", 
                                parallel = TRUE, numCores = n_cores)

write.csv(rp_enm_eval_full@results, paste(enm_wd, "Subadult_RainbowParrotfish_ENMeval.csv"))



# Gray Snapper ENMeval ----------------------------------------------------


# read in presence only data with only three columns: species, longitude, latitude (in that order)
lg_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_GraySnapper_PO_Full.csv"))[,-1]

# run model evaluation on full PO data with random-k-fold partitioning
lg_enm_eval_full <- ENMevaluate(lg_PO_full, pred_select, bg = bg_pts, 
                                tune.args = list(fc = c("L", "LQ", "LQH", "LQHP"), 
                                                 rm = c(0.50, 1.0, 2.0, 5.0, 6.0)),
                                partitions = "randomkfold", partition.settings = ps, 
                                algorithm = "maxent.jar", categoricals = "Habitat", 
                                parallel = TRUE, numCores = n_cores)

write.csv(lg_enm_eval_full@results, paste(enm_wd, "Subadult_GraySnapper_ENMeval.csv"))



# Bluestriped Grunt ENMeval -----------------------------------------------


# read in presence only data with only two columns: species, longitude, latitude (in that order)
hs_PO_full <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BluestripedGrunt_PO_Full.csv"))[,-1]

# run model evaluation on full PO data with random-k-fold partitioning
hs_enm_eval_full <- ENMevaluate(hs_PO_full, pred_select, bg = bg_pts, 
                                tune.args = list(fc = c("L", "LQ", "LQH", "LQHP"), 
                                                 rm = c(0.50, 1.0, 2.0, 5.0, 6.0)),
                                partitions = "randomkfold", partition.settings = ps, 
                                algorithm = "maxent.jar", categoricals = "Habitat", 
                                parallel = TRUE, numCores = n_cores)

write.csv(hs_enm_eval_full@results, paste(enm_wd, "Subadult_BluestripedGrunt_ENMeval.csv"))



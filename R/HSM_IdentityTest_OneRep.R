### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script is used to:
# 1. Pairwise analyses of species' HSMs - specifically, the Identity test (Dan Warren et al.)
# Note: this script perfomrs only ONE rep of the identity test
#     - occurrence data for 2 species must be pre-permuted (see Occurrence_Permutations.R)
#     - this script is used in a job array on the Cedar cluster (DRAC), 
#       where each index in the array performs a separate rep

# This scrip is part of Isla Turcke's first PhD chapter in the lab of 
# Dr. S.J. Green at the University of Alberta (2022-2026). 
# Data are specific to southern Florida and habitat suitability was modeled 
# in MaxEnt for five focal species.

### TO USE THIS FILE ###
# Before running this R script:
# - This file should be in the same directory as:
#    1. A folder called "Occurrence_Data" containing speices presence points and background points
#       A sub-folder called "Permutations" containing .csv files of permuted species presence points
#    2. A folder called "Suitability" containing the suitability ascii files from MaxEnt
#    3. A folder called "Predictors" containing the environmental predictors for the HSMs
#    4. The "My_ENMTools_Functions.R" file

print("SUSHI")

# Set Up ------------------------------------------------------------------


# install ENMTools, sf, and terra, if necessary
#install.packages(c("devtools", "sf", "terra")); library(devtools); devtools::install_github("danlwarren/ENMTools", force = TRUE)

# set memory available to the java virtual machine (PERHAPS NOT NEEDED ON CLUSTER)
options(java.parameters = "-Xmx128000m")

# load packages
library(easypackages)
libraries("ENMTools","sf","terra","here","tidyverse","dplyr","rJava")
#ENMTools::install.extras()

# set the Isla_MSc_Ch1 folder as the root directory for relative paths (CLUSTER EDIT)
here::i_am("HSM_IdentityTest.R")

# call on my script of functions based on the ENMTools (CLUSTER EDIT) 
source(here::here("My_ENMTools_Functions.R"))

# change where large temporary rasters are saved (CLUSTER EDIT)
terraOptions(tempdir = "C:/Users/islat/Desktop/IdentityTest_BP_MP/temp")

# set crs
my_crs <- "epsg:6346"



# Import Data --------------------------------------------------------------


## Predictor Rasters -------------------------------------------------------

# Import predictor layers (CLUSTER EDIT)
env.files <- list.files(path = "./Predictors/", pattern = "setMinMax.tif", full.names = TRUE) #pattern = ".asc" for ASCII files
env <- terra::rast(env.files)

#env <- check.env(env = env, verbose = TRUE)

rm(env.files)

# set crs
terra::crs(env) <- my_crs
predictor_names <- names(env)


# make sure min and max are set for each layer (CLUSTER EDIT: do I need to write these layers out? if so, where?)
for (i in 1:nlyr(env)){
  if (hasMinMax(env[[i]]) == FALSE) {
    setMinMax(env[[i]])
    print(paste("MinMax set for layer ", i))
  }
  #writeRaster(env[[i]], paste0("Predictors/",predictor_names[i],"_setMinMax.tif"))
}

### SET MIN MAX CAN BE DONE AHEAD OF TIME IF RASTERS ARE SAVED AS .TIF
### I DON'T KNOW IF MAXENT WILL WORK WITH .TIF FILES BUT I WILL CHECK


## Occurrence Data -----------------------------------------------------

# Import permuted species occurrence points 

# get input file names from the command line
args <- commandArgs(trailingOnly = TRUE)

# make sure three arguments were provided: two .csv files and one replicate index
if (length(args) != 3) {
  stop("Three inputs must be provided.")
}

# get file names and input files
input_file1 <- args[1]
input_file2 <- args[2]
rep <- args[3]
SP1_occ <- read.csv(here("Occurrence_Data","Permutations",input_file1))
SP2_occ <- read.csv(here("Occurrence_Data","Permutations",input_file2))

# Import dataframe of 10,000 background points (chosen according to bias file)
# convert to spatVect
back_pts <- read.csv(here("Occurrence_Data","Final_Background_Points.csv"))
back_pts <- back_pts[sample(nrow(back_pts), size = 100),]
back_vect <- terra::vect(back_pts, geom = c("longitude","latitude"))

### Create enmtools.species objects

# blue parrotfish
SP1_enm <- enmtools.species(species.name = "Blue Parrotfish", 
                            presence.points = vect(SP1_occ, geom = c("longitude","latitude")),
                            background.points = back_vect)
crs(SP1_enm$presence.points) <- my_crs
crs(SP1_enm$background.points) <- my_crs
SP1_enm <- check.species(SP1_enm)

# midnight parrotfish
SP2_enm <- enmtools.species(species.name = "Midnight Parrotfish", 
                            presence.points = vect(SP2_occ, geom = c("longitude","latitude")),
                            background.points = back_vect)
crs(SP2_enm$presence.points) <- my_crs
crs(SP2_enm$background.points) <- my_crs
SP2_enm <- check.species(SP2_enm)

# clean up
rm(args, input_file1, input_file2, SP1_occ, SP2_occ, back_pts, back_vect)



# Identity Test -----------------------------------------------------------


# Perform my version of the ENMTools identity test
idtest_output <- my.identity.test.onerep(species.1.perm = SP1_enm, species.2.perm = SP2_enm, 
                                  env, clamp = FALSE, verbose = TRUE)

# Write the raw results to a file (CLUSTER EDIT)
write.csv(idtest_output, paste0("IdentityTest_Results/BP_MP/perm", rep, "_overlap.csv"))


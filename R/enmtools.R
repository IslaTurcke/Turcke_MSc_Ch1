# # WELCOME # #

# This is script __ of __ in Isla's data prep pipeline.

# This script is used to look for statistically significant differences between
# habitat suitability models (HSMs) for five focal species for Isla Turcke's first 
# MSc chapter in the lab of Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida and habitat suitability was modeled in MaxEnt.

### TO USE THIS FILE ###
# Before running this R script:
# - Make sure the maxent results are in a folder called HSMs, with a subfolder
#   for each species.
# - If they are not, run a maxent model for each species using data from the "Final_Data" folder.



# Set Up ------------------------------------------------------------------


# install ENMTools, sf, and terra, if necessary
# install.packages(c("devtools", "sf", "terra")); library(devtools); devtools::install_github("danlwarren/ENMTools", force = TRUE)

# load packages
library(ENMTools); library(sf); library(terra); library(here); library(tidyverse)

# set working directory
setwd("Z:/Isla_MSc_Ch1/")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/Collinearity_ENMevaluate.R")

# change where large temporary rasters are saved
terraOptions(tempdir = "Z:/Isla_MSc_Ch1/Temp/")

# Create a directory for the ENMTools results
dir.create("Permutation_Analysis/ENM_Results")

# Import raster layers
habitat <- rast(here("Final_Data","Predictors_ASCII","Habitat_Type.asc"))
mg_dist <- rast(here("Final_Data","Predictors_ASCII","Mangrove_Distance.asc"))
depth <- rast(here("Final_Data","Predictors_ASCII","Depth.asc"))
slope <- rast(here("Final_Data","Predictors_ASCII","Slope.asc"))
curvature <- rast(here("Final_Data","Predictors_ASCII","Curvature.asc"))
rug_acr <- rast(here("Final_Data","Predictors_ASCII","Rugosity_ACR.asc"))
bpi_fine <- rast(here("Final_Data","Predictors_ASCII","BPI_Fine.asc"))
bpi_broad <- rast(here("Final_Data","Predictors_ASCII","BPI_Broad.asc"))
sum_temp <- rast(here("Final_Data","Predictors_ASCII","Summer_Temperature.asc"))
sum_do <- rast(here("Final_Data","Predictors_ASCII","Summer_Dissolved_Oxygen.asc"))
win_temp <- rast(here("Final_Data","Predictors_ASCII","Winter_Temperature.asc"))
win_sal <- rast(here("Final_Data","Predictors_ASCII","Winter_Salinity.asc"))
win_do <- rast(here("Final_Data","Predictors_ASCII","Winter_Dissolved_Oxygen.asc"))

# Combine raster layers
env <- c(habitat, mg_dist)
#, depth, slope, curvature, rug_acr, bpi_broad, bpi_fine, 
#         sum_temp, sum_do, win_temp, win_sal, win_do)
#rm(habitat, mg_dist, 
rm(depth, slope, curvature, rug_acr, bpi_broad, bpi_fine, 
   sum_temp, sum_do, win_temp, win_sal, win_do)

# Import species occurrence points for two species (see formatting!)
bp <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BlueParrotfish_PO_Full.csv"))
mp <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_MidnightParrotfish_PO_Full.csv"))
rp <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_RainbowParrotfish_PO_Full.csv"))
gs <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_GraySnapper_PO_Full.csv"))
bg <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BluestripedGrunt_PO_Full.csv"))

# Format for ENMTools
colnames(bp)[2:3] <- c("Longitude", "Latitude")
colnames(mp)[2:3] <- c("Longitude", "Latitude")
colnames(rp)[2:3] <- c("Longitude", "Latitude")
colnames(gs)[2:3] <- c("Longitude", "Latitude")
colnames(bg)[2:3] <- c("Longitude", "Latitude")

# Write each species to a separate file, with only "Longitude" and "Latitude columns
write.csv(bp[,2:3], here("Permutation_Analysis","BlueParrotfish_Subadult.csv"), row.names = FALSE)
write.csv(mp[,2:3], here("Permutation_Analysis","MidnightParrotfish_Subadult.csv"), row.names = FALSE)
write.csv(rp[,2:3], here("Permutation_Analysis","RainbowParrotfish_Subadult.csv"), row.names = FALSE)
write.csv(gs[,2:3], here("Permutation_Analysis","GraySnapper_Subadult.csv"), row.names = FALSE)
write.csv(bg[,2:3], here("Permutation_Analysis","BluestripedGrunt_Subadult.csv"), row.names = FALSE)

# Save the paths to the two separate files
bp_path <- "Permutation_Analysis/BlueParrotfish_Subadult.csv"
mp_path <- "Permutation_Analysis/MidnightParrotfish_Subadult.csv"
rp_path <- "Permutation_Analysis/RainbowParrotfish_Subadult.csv"
gs_path <- "Permutation_Analysis/GraySnapper_Subadult.csv"
bg_path <- "Permutation_Analysis/BluestripedGrunt_Subadult.csv"

# Import background points
back_pts <- read.csv(here("Final_Data","Final_Background_Points.csv")) 
back_pts <- back_pts[1:100,] %>% st_as_sf(., coords = c(1,2)) %>% terra::vect(.)

# clean up
rm(bp, mp, rp, gs, bg)



# Blue vs Midnight Parrotfish ---------------------------------------------


# Import the two species occurrences and convert to enmtools.species objects
bp_csv <- read.csv(bp_path); bp_sf <- st_as_sf(bp_csv, coords = c(1,2)); bp_spatvector <- terra::vect(bp_sf)
bp_enm <- enmtools.species(species.name = "bp", presence.points = bp_spatvector)
bp_enm$background.points = back_pts
mp_csv <- read.csv(mp_path); mp_sf <- st_as_sf(mp_csv, coords = c(1,2)); mp_spatvector <- terra::vect(mp_sf)
mp_enm <- enmtools.species(species.name = "mp", presence.points = mp_spatvector)
mp_enm$background.points = back_pts

# Set the number of replicate runs of the identity.test() function
reps <- 10 # should normally be 100

# Set the number of background points to be used
back <- 100 # should normally be 10000

# Perform the identity test
idtest_bp_mp <- identity.test(bp_enm, mp_enm, env, type = 'mx', f = NULL,
  nreps = reps, nback = back, bg.source = "points",
  low.memory = TRUE, rep.dir = "Permutation_Analysis/temp",
  verbose = TRUE, clamp = TRUE
)

# Write the raw results to a file
write.csv(idtest$reps.overlap, "Permutation_Analysis/ENM_Results/BP_MP/reps_overlap.csv")

# Split the permuted results from the empirical results
permute.reps <- idtest$reps.overlap[2:nrow(idtest$reps.overlap),]

# Set the percentile you want to use from your permuted results to obtain the critical values
critical.percent <- 95
critical.number <- round((critical.percent/100)*(reps))
d.critical <- as.vector(permute.reps[order(permute.reps[,"D"], decreasing = TRUE),"D"])[critical.number]
i.critical <- as.vector(permute.reps[order(permute.reps[,"I"], decreasing = TRUE),"I"])[critical.number]
rank.cor.critical <- as.vector(permute.reps[order(permute.reps[,"rank.cor"], decreasing = TRUE),"rank.cor"])[critical.number]

# Create a summary results object
results.summary <- rbind(idtest$reps.overlap[1,1:3], c(d.critical, i.critical, rank.cor.critical))

# Create the proper row names for the summary results object

rownames(results.summary) <- c("empirical.value", "permuted.critical.value")

# Write the summary results object to a file 

write.csv(results.summary, "enmres/results_summary.csv")
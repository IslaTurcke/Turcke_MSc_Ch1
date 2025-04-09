### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script is used to:
# 1. Pairwise analyses of species' HSMs - specifically, the Identity test (Dan Warren et al.)

# This scrip is part of Isla Turcke's first PhD chapter in the lab of 
# Dr. S.J. Green at the University of Alberta (2022-2026). 
# Data are specific to southern Florida and habitat suitability was modeled 
# in MaxEnt for five focal species.

### TO USE THIS FILE ###
# Before running this R script:
# - This file should be in the same directory as:
#    1. A folder called "Species_Occurrence" containing speices presence points and background points
#    2. A folder called "Suitability_ASCIIs" containing the suitability ascii files from MaxEnt
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
ENMTools::install.extras()

# set working directory (CLUSTER EDIT)
setwd("Z:/Isla_MSc_Ch1/")

# set the Isla_MSc_Ch1 folder as the root directory for relative paths (CLUSTER EDIT)
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/HSM_Analysis.R")

# call on my script of functions based on the ENMTools (CLUSTER EDIT) 
source(here::here("GitHub_Repositories","Turcke_MSc_Ch1","R","My_ENMTools_Functions.R"))

# change where large temporary rasters are saved (CLUSTER EDIT)
terraOptions(tempdir = "Z:/Isla_MSc_Ch1/Temp/")

# set crs
my_crs <- "epsg:6346"



# Import Data --------------------------------------------------


## Suitability Rasters -----------------------------------------------------

# Import MaxEnt suitability rasters (CLUSTER EDIT)
bp_suit <- rast(here("HSM_Results","Subadult_BlueParrotfish","SCA_COER_avg.asc"))
mp_suit <- rast(here("HSM_Results","Subadult_MidnightParrotfish","SCA_COEL_avg.asc"))
#rp_suit <- rast(here("HSM_Results","Subadult_RainbowParrotfish","SCA_GUAC_avg.asc"))
#gs_suit <- rast(here("HSM_Results","Subadult_GraySnapper","LUT_GRIS_avg.asc"))
#bg_suit <- rast(here("HSM_Results","Subadult_BluestripedGrunt","HAE_SCIU_avg.asc"))

# set crs
terra::crs(bp_suit) <- my_crs
terra::crs(mp_suit) <- my_crs
#terra::crs(rp_suit) <- my_crs
#terra::crs(gs_suit) <- my_crs
#terra::crs(bg_suit) <- my_crs

# make sure min and max are available
if (hasMinMax(bp_suit) == FALSE) {
  setMinMax(bp_suit)
}
if (hasMinMax(mp_suit) == FALSE) {
  setMinMax(mp_suit)
}
# if (hasMinMax(rp_suit) == FALSE) {
#   setMinMax(rp_suit)
# }
# if (hasMinMax(gs_suit) == FALSE) {
#   setMinMax(gs_suit)
# }
# if (hasMinMax(bg_suit) == FALSE) {
#   setMinMax(bg_suit)
# }


## Predictor Rasters -------------------------------------------------------

# Import predictor layers (CLUSTER EDIT)
habitat <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Habitat_Type.asc"))
mg_dist <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Mangrove_Distance.asc"))
depth <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Depth.asc"))
slope <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Slope.asc"))
curvature <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Curvature.asc"))
rug_acr <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Rugosity_ACR.asc"))
bpi_fine <- rast(here("Final_Data","Predictors_ENMTools_IDTest","BPI_Fine.asc"))
bpi_broad <- rast(here("Final_Data","Predictors_ENMTools_IDTest","BPI_Broad.asc"))
sum_temp <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Summer_Temperature.asc"))
sum_do <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Summer_Dissolved_Oxygen.asc"))
win_temp <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Winter_Temperature.asc"))
win_sal <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Winter_Salinity.asc"))
win_do <- rast(here("Final_Data","Predictors_ENMTools_IDTest","Winter_Dissolved_Oxygen.asc"))

# Combine raster layers
env <- c(habitat, mg_dist, depth, slope, curvature, rug_acr, bpi_broad, bpi_fine, 
         sum_temp, sum_do, win_temp, win_sal, win_do)
#env <- check.env(env = env, verbose = TRUE)

rm(habitat, mg_dist, depth, slope, curvature, rug_acr, bpi_broad, bpi_fine, 
   sum_temp, sum_do, win_temp, win_sal, win_do)

# set crs
terra::crs(env) <- my_crs
predictor_names <- names(env)


# make sure min and max are set for each layer (CLUSTER EDIT: do I need to write these layers out? if so, where?)
for (i in 1:nlyr(env)){
  if (hasMinMax(env[[i]]) == FALSE) {
    setMinMax(env[[i]])
    print(paste("MinMax set for layer ", i))
  }
  #writeRaster(env[[i]], paste0("Final_Data/Predictors_setMinMax/",predictor_names[i],"_setMinMax.asc"))
}


## ENM Species Objects -----------------------------------------------------

# Import species occurrence points for species, selecting only lon and lat columns
bp <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BlueParrotfish_PO_Full.csv")) %>% 
  select("longitude","latitude")
mp <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_MidnightParrotfish_PO_Full.csv")) %>% 
  select("longitude","latitude")
# rp <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_RainbowParrotfish_PO_Full.csv")) %>% 
#   select("longitude","latitude")
# gs <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_GraySnapper_PO_Full.csv")) %>% 
#   select("longitude","latitude")
# bg <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BluestripedGrunt_PO_Full.csv")) %>% 
#   select("longitude","latitude")

# Import dataframe of 10,000 background points (chosen according to bias file)
# convert to spatVect
back_pts <- read.csv(here("Final_Data","Final_Background_Points.csv"))
back_pts <- back_pts[sample(nrow(back_pts), size = 100),]
back_vect <- terra::vect(back_pts, geom = c("longitude","latitude"))

### Create enmtools.species objects

# blue parrotfish
bp_enm <- enmtools.species(species.name = "Blue Parrotfish", 
                           presence.points = vect(bp, geom = c("longitude","latitude")),
                           background.points = back_vect)
crs(bp_enm$presence.points) <- my_crs
crs(bp_enm$background.points) <- my_crs
bp_enm <- check.species(bp_enm)

# midnight parrotfish
mp_enm <- enmtools.species(species.name = "Midnight Parrotfish", 
                           presence.points = vect(mp, geom = c("longitude","latitude")),
                           background.points = back_vect)
crs(mp_enm$presence.points) <- my_crs
crs(mp_enm$background.points) <- my_crs
mp_enm <- check.species(mp_enm)

# rainbow parrotfish
# rp_enm <- enmtools.species(species.name = "Rainbow Parrotfish", 
#                            presence.points = vect(rp, geom = c("longitude","latitude")),
#                            background.points = back_vect)
# crs(rp_enm$presence.points) <- my_crs
# crs(rp_enm$background.points) <- my_crs
# rp_enm <- check.species(rp_enm)
# 
# # gray snapper
# gs_enm <- enmtools.species(species.name = "Gray Snapper", 
#                            presence.points = vect(gs, geom = c("longitude","latitude")),
#                            background.points = back_vect)
# crs(gs_enm$presence.points) <- my_crs
# crs(gs_enm$background.points) <- my_crs
# gs_enm <- check.species(gs_enm)
# 
# # bluestriped grunt
# bg_enm <- enmtools.species(species.name = "Bluestriped Grunt",
#                            presence.points = vect(bg, geom = c("longitude","latitude")),
#                            background.points = back_vect)
# crs(bg_enm$presence.points) <- my_crs
# crs(bg_enm$background.points) <- my_crs
# bg_enm <- check.species(bg_enm)

# clean up
rm(bp, mp, back_pts, back_vect)
rm(i)
#rm(rp, gs, bg)



# Identity Test -----------------------------------------------------------


# Perform my version of the ENMTools identity test
idtest_bp_mp <- my.identity.test(species.1 = bp_enm, species.2 = mp_enm, 
                                 suitability.1 = bp_suit, suitability.2 = mp_suit,
                                 env, nreps = 2, clamp = FALSE, verbose = TRUE)

# Write the raw results to a file (CLUSTER EDIT)
write.csv(idtest_bp_mp$reps.overlap, "Permutation_Analysis/ENM_Results/BP_MP/reps_overlap.csv")



# Critical Values ---------------------------------------------------------


# Split the permuted results from the empirical results
permute.reps <- idtest_bp_mp$reps.overlap[2:nrow(idtest_bp_mp$reps.overlap),]

# Set the percentile you want to use from your permuted results to obtain the critical values
critical.percent <- 95
critical.number <- round((critical.percent/100)*(reps))
d.critical <- as.vector(permute.reps[order(permute.reps[,"D"], decreasing = TRUE),"D"])[critical.number]
i.critical <- as.vector(permute.reps[order(permute.reps[,"I"], decreasing = TRUE),"I"])[critical.number]
rank.cor.critical <- as.vector(permute.reps[order(permute.reps[,"rank.cor"], decreasing = TRUE),"rank.cor"])[critical.number]

# Create a summary results object
results.summary <- rbind(idtest_bp_mp$reps.overlap[1,1:3], c(d.critical, i.critical, rank.cor.critical))

# Create the proper row names for the summary results object
rownames(results.summary) <- c("empirical.value", "permuted.critical.value")

# Write the summary results object to a file (CLUSTER EDIT)
write.csv(results.summary, "enmres/results_summary.csv")





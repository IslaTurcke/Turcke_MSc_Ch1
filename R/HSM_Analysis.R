### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script is used to:
# 1. Summarize the results of the MaxEnt HSMs 
# 2. Pairwise analyses of species' HSMs 

# This scrip is part of Isla Turcke's first MSc chapter in the lab of 
# Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida and habitat suitability was modeled 
# in MaxEnt for five focal species.

### TO USE THIS FILE ###
# Before running this R script:
# - Make sure the maxent results are in a folder called HSMs, with a subfolder
#   for each species.
# - If they are not, run a maxent model for each species using data from the "Final_Data" folder.



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

# create an empty dataframe for pairwise analyses of species' HSMs
pairwise_results <- data.frame(Pair = c("BP-MP","BP-RP","BP-GS","BP-BG",
                                        "MP-RP","MP-GS","MP-BG",
                                        "RP-GS","RP-BG","GS-BG"),
                               Comparison = c("Within","Within","Between","Between",
                                              "Within","Between","Between",
                                              "Between","Between","Within"),
                               PresencePoint_Overlap = numeric(10),
                               D = numeric(10),
                               I = numeric(10),
                               rho = numeric(10))



# Single HSM Analyses -----------------------------------------------------


## Summarize Results -------------------------------------------------------

# import results csv files
bp_hsm <- read.csv(here("HSM_Results","Subadult_BlueParrotfish","maxentResults.csv"))
mp_hsm <- read.csv(here("HSM_Results","Subadult_MidnightParrotfish","maxentResults.csv"))
rp_hsm <- read.csv(here("HSM_Results","Subadult_RainbowParrotfish","maxentResults.csv"))
gs_hsm <- read.csv(here("HSM_Results","Subadult_GraySnapper","maxentResults.csv"))
bg_hsm <- read.csv(here("HSM_Results","Subadult_BluestripedGrunt","maxentResults.csv"))

# combine average values for each species into one data table
# select desired columns
hsm_results <- rbind(bp_hsm[1:10,],mp_hsm[1:10,],rp_hsm[1:10,],gs_hsm[1:10,],bg_hsm[1:10,]) %>% 
  mutate(Samples_Train = X.Training.samples, AUC_Train = Training.AUC, AUC_Test = Test.AUC, 
         MaxSSS_cloglog_Train = Maximum.training.sensitivity.plus.specificity.Cloglog.threshold,
         MaxSSS_cloglog_Test = Maximum.test.sensitivity.plus.specificity.Cloglog.threshold) %>% 
  select("Species","Samples_Train","AUC_Train","AUC_Test","Entropy","MaxSSS_cloglog_Train","MaxSSS_cloglog_Test",25:37) %>% 
  separate(Species, into = c("Species","Run"), sep = "_(?=[^_]+$)")

# calculate mean and standard error for each value
# keep only predictors that were in the top 6 (based on importance)
hsm_summary <- hsm_results %>% group_by(Species) %>% 
  summarize(across(c(3:20), list(mean = ~ mean(.), 
                                 lower = ~ mean(.) - (qt(0.975, n()-1) * sd(.) / sqrt(n())),
                                 upper = ~ mean(.) + (qt(0.975, n()-1) * sd(.) / sqrt(n()))),
                   .names = "{col}_{fn}"))

# clean up
rm(bp_hsm, mp_hsm, rp_hsm, gs_hsm, bg_hsm)


## Niche Breadth -----------------------------------------------------------

# read in suitability rasters
bp_suit <- rast(here("HSM_Results","Subadult_BlueParrotfish","SCA_COER_avg.asc"))
mp_suit <- rast(here("HSM_Results","Subadult_MidnightParrotfish","SCA_COEL_avg.asc"))
rp_suit <- rast(here("HSM_Results","Subadult_RainbowParrotfish","SCA_GUAC_avg.asc"))
gs_suit <- rast(here("HSM_Results","Subadult_GraySnapper","LUT_GRIS_avg.asc"))
bg_suit <- rast(here("HSM_Results","Subadult_BluestripedGrunt","HAE_SCIU_avg.asc"))

# set crs
terra::crs(bp_suit) <- my_crs
terra::crs(mp_suit) <- my_crs
terra::crs(rp_suit) <- my_crs
terra::crs(gs_suit) <- my_crs
terra::crs(bg_suit) <- my_crs

# make sure min and max are available
if (hasMinMax(bp_suit) == FALSE) {
  setMinMax(bp_suit)
}
if (hasMinMax(mp_suit) == FALSE) {
  setMinMax(mp_suit)
}
if (hasMinMax(rp_suit) == FALSE) {
  setMinMax(rp_suit)
}
if (hasMinMax(gs_suit) == FALSE) {
  setMinMax(gs_suit)
}
if (hasMinMax(bg_suit) == FALSE) {
  setMinMax(bg_suit)
}

# Levins' (1968) two metrics of niche breadth
bp_breadth <- raster.breadth(bp_suit)
mp_breadth <- raster.breadth(mp_suit)
rp_breadth <- raster.breadth(rp_suit)
gs_breadth <- raster.breadth(gs_suit)
bg_breadth <- raster.breadth(bg_suit)

# add to results summary dataframe
hsm_summary <- hsm_summary %>% mutate(Levins_B1 = c(bp_breadth$B1,mp_breadth$B1,rp_breadth$B1,gs_breadth$B1,bg_breadth$B1),
                                      Levins_B2 = c(bp_breadth$B2,mp_breadth$B2,rp_breadth$B2,gs_breadth$B2,bg_breadth$B2))

# write out dataset to GitHub repository
write.csv(hsm_summary, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","MaxEnt_Summary_Subadult.csv"))

# clean up
rm(bp_breadth, mp_breadth, rp_breadth, gs_breadth, bg_breadth)



# ENM Species Objects -----------------------------------------------------


# Import species occurrence points for species, selecting only lon and lat columns
bp <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BlueParrotfish_PO_Full.csv")) %>% 
  select("longitude","latitude")
mp <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_MidnightParrotfish_PO_Full.csv")) %>% 
  select("longitude","latitude")
rp <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_RainbowParrotfish_PO_Full.csv")) %>% 
  select("longitude","latitude")
gs <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_GraySnapper_PO_Full.csv")) %>% 
  select("longitude","latitude")
bg <- read.csv(here("Final_Data","Species_Occurrence","Subadult","Subadult_BluestripedGrunt_PO_Full.csv")) %>% 
  select("longitude","latitude")

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
rp_enm <- enmtools.species(species.name = "Rainbow Parrotfish", 
                           presence.points = vect(rp, geom = c("longitude","latitude")),
                           background.points = back_vect)
crs(rp_enm$presence.points) <- my_crs
crs(rp_enm$background.points) <- my_crs
rp_enm <- check.species(rp_enm)

# gray snapper
gs_enm <- enmtools.species(species.name = "Gray Snapper", 
                           presence.points = vect(gs, geom = c("longitude","latitude")),
                           background.points = back_vect)
crs(gs_enm$presence.points) <- my_crs
crs(gs_enm$background.points) <- my_crs
gs_enm <- check.species(gs_enm)

# bluestriped grunt
bg_enm <- enmtools.species(species.name = "Bluestriped Grunt",
                           presence.points = vect(bg, geom = c("longitude","latitude")),
                           background.points = back_vect)
crs(bg_enm$presence.points) <- my_crs
crs(bg_enm$background.points) <- my_crs
bg_enm <- check.species(bg_enm)

# clean up
rm(bp, mp, rp, gs, bg, back_pts, back_vect)



# Pairwise Analyses -------------------------------------------------------


## Presence Point Overlap -----------------------------------------------------------

# calculate overlap of presence points
bp_mp_pp_overlap <- point.overlap(bp_enm, mp_enm)
bp_rp_pp_overlap <- point.overlap(bp_enm, rp_enm)
bp_gs_pp_overlap <- point.overlap(bp_enm, gs_enm)
bp_bg_pp_overlap <- point.overlap(bp_enm, bg_enm)

mp_rp_pp_overlap <- point.overlap(mp_enm, rp_enm)
mp_gs_pp_overlap <- point.overlap(mp_enm, gs_enm)
mp_bg_pp_overlap <- point.overlap(mp_enm, gs_enm)

rp_gs_pp_overlap <- point.overlap(rp_enm, gs_enm)
rp_bg_pp_overlap <- point.overlap(rp_enm, gs_enm)

gs_bg_pp_overlap <- point.overlap(gs_enm, bg_enm)

# add to pairwise results dataframe
pairwise_results$PresencePoint_Overlap <- c(bp_mp_pp_overlap, bp_rp_pp_overlap, bp_gs_pp_overlap, bp_bg_pp_overlap,
                                            mp_rp_pp_overlap, mp_gs_pp_overlap, mp_bg_pp_overlap,
                                            rp_gs_pp_overlap, rp_bg_pp_overlap, gs_bg_pp_overlap)


## Suitability Overlap -----------------------------------------------------

bp.mp <- my.raster.overlap(bp_suit, mp_suit)

### Suitability calculated using separate script for each species pair - I don't remember why...



# Identity Test Significance ----------------------------------------------

# calculating the p-value for each pairwise overlap comparison

# read in files
# import and combine data
ID_paths <- list.files(here("HSM_Analysis", "Identity_Test", "Results"), full.names = TRUE, pattern = "^OverlapValues_.*\\.csv$")

ID_data <- map_dfr(ID_paths, function(path) {
  df <- read_csv(path, col_names = TRUE)
  
  # Extract species pair name from the filename
  file_name <- tools::file_path_sans_ext(basename(path))
  species_pair <- str_remove(file_name, "OverlapValues_")
  df$species_pair <- species_pair
  
  return(df)
})

# set column names
colnames(ID_data) <- c("row","D","I","R","species_pair")

# create list of species pairs
pairs <- unique(ID_data$species_pair)

# create empty data frame to store p-values
p_vals <- data.frame()

# loop through species pair and calculate p-value

for (i in pairs) {
  
  # filter data for species pair i
  temp_data <- ID_data %>% filter(species_pair == i)
  
  # split permuted values from empirical
  temp_permuted <- temp_data %>% filter(row == 1)
  temp_empirical <- temp_data %>% filter(row == "empirical")
  
  # calculate p-values
  D <- temp_empirical$D
  count_greater_D <- sum(temp_permuted$D <= D, na.rm = TRUE)
  p_D <- (count_greater_D + 1e-6) / (nrow(temp_permuted) + 1e-6)
  
  I <- temp_empirical$I
  count_greater_I <- sum(temp_permuted$I <= I, na.rm = TRUE)
  p_I <- (count_greater_I + 1e-6) / (nrow(temp_permuted) + 1e-6)
  
  R <- temp_empirical$R
  count_greater_R <- sum(temp_permuted$R <= R, na.rm = TRUE)
  p_R <- (count_greater_R + 1e-6) / (nrow(temp_permuted) + 1e-6)
  
  # save p-values to dataframe
  temp_pvals <- c(i, p_D, p_I, p_R)
  p_vals <- rbind(p_vals, temp_pvals)

}

# set column names
colnames(p_vals) <- c("Species_Pair","p_val_D","p_val_I","p_val_R")

# output p-vals
write.csv(p_vals, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","IdentityTest_pValues.csv"))



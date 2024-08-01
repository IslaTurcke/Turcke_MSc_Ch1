### WELCOME ###

# This is script __ of __ in Isla's data plotting pipeline.

# This script is used to plot results from the Maximum Entropy
# habitat suitability models (HSMs) of five focal species for Isla Turcke's first 
# MSc chapter in the lab of Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida.



# Set Up ------------------------------------------------------------------


# install ENMTools, sf, and terra, if necessary
# install.packages(c("devtools", "sf", "terra")); library(devtools); devtools::install_github("danlwarren/ENMTools", force = TRUE)

# load packages
library(easypackages)
libraries("sf","terra","here","tidyverse","dplyr","ggplot2","ggmap","gridExtra")

# set working directory
setwd("Z:/Isla_MSc_Ch1/")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/HSM_Plotting.R")

# change where large temporary rasters are saved
terraOptions(tempdir = "Z:/Isla_MSc_Ch1/Temp/")

# read in summary stats for HSM results
hsm_summary <- read.csv(here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","MaxEnt_Summary_Subadult.csv"))



# Variable Importance -----------------------------------------------------


# remove predictors that are not in the top 6 (based on importance) of any species
# remove model statistics unrelated to variable importance
hsm_summary <- hsm_summary %>% select(-c(3:17)) %>% 
  select(-c(BPI_Fine.permutation.importance_mean, BPI_Fine.permutation.importance_lower, BPI_Fine.permutation.importance_upper,
                                         Curvature.permutation.importance_mean, Curvature.permutation.importance_lower, Curvature.permutation.importance_upper,
                                         Summer_Temperature.permutation.importance_mean, Summer_Temperature.permutation.importance_lower, Summer_Temperature.permutation.importance_upper,
                                         Winter_Dissolved_Oxygen.permutation.importance_mean, Winter_Dissolved_Oxygen.permutation.importance_lower, Winter_Dissolved_Oxygen.permutation.importance_upper,
                                         Winter_Salinity.permutation.importance_mean, Winter_Salinity.permutation.importance_lower, Winter_Salinity.permutation.importance_upper,
                                         Winter_Temperature.permutation.importance_mean, Winter_Temperature.permutation.importance_lower, Winter_Temperature.permutation.importance_upper,
                                         X))

# Reshape the data to long format for ggplot2 
hsm_long <- hsm_summary %>% pivot_longer(cols = -Species, names_to =c("Predictor", ".value"), 
                                         names_pattern = "(.*)_(.*)" ) 

# Create the plot 
ggplot(hsm_long, aes(x = Predictor, y = mean, fill = Species)) + 
  geom_bar(position = position_dodge(), stat = "identity") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), width = 0.25) + 
  labs(x = "Environmental Predictor", y = "Average Permutation Importance") + 
  theme_minimal() + 
  theme(axis.text.x =element_text(angle = 45, hjust = 1))

ggplot(hsm_long, aes(x = Species, y = mean, fill = Predictor)) + 
  geom_bar(position = position_dodge(), stat = "identity") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), width = 0.25) + 
  labs(x = "Species", y = "Average Permutation Importance") + 
  theme_minimal() + 
  theme(axis.text.x =element_text(angle = 45, hjust = 1))



# Response Curves ---------------------------------------------------------


# function to read in response curve data
read_dat_file <- function(file) {
  df <- read.table(file, header = TRUE, sep = ",")
  
  # add column for species code
  species <- substr(basename(file), 1, 8)
  df <- cbind(Species = species, df)
  
  return(df)
}

# read in data
bp_files <- list.files(here("HSMs","Subadult_BlueParrotfish","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
bp_resp <- lapply(bp_files, read_dat_file) 

mp_files <- list.files(here("HSMs","Subadult_MidnightParrotfish","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
mp_resp <- lapply(mp_files, read_dat_file) 

rp_files <- list.files(here("HSMs","Subadult_RainbowParrotfish","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
rp_resp <- lapply(rp_files, read_dat_file) 

gs_files <- list.files(here("HSMs","Subadult_GraySnapper","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
gs_resp <- lapply(gs_files, read_dat_file) 

bg_files <- list.files(here("HSMs","Subadult_BluestripedGrunt","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
bg_resp <- lapply(bg_files, read_dat_file) 

rm(bp_files, mp_files, rp_files, gs_files, bg_files)

# combine all into one data frame
resp_curvs <- rbind(bind_rows(bp_resp), bind_rows(mp_resp), bind_rows(rp_resp), bind_rows(gs_resp), bind_rows(bg_resp))

# create the plot
ggplot(resp_curvs, aes(x = x, y = y, colour = Species, group = Species)) +
  geom_line() +
  facet_wrap(~ resp_curvs$variable, scales = "free") +
  labs(y = "Habitat Suitability") +
  theme_minimal()


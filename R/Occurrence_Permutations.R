### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script is used to:
# 1. Permute the species occurrence data (Presence-only) of two species
#    in preparation to perform an identity test (Warren et al.) on the two species.

# This scrip is part of Isla Turcke's first PhD chapter in the lab of 
# Dr. S.J. Green at the University of Alberta (2022-2026). 
# Data are specific to southern Florida and habitat suitability was modeled 
# in MaxEnt for five focal species.

### TO USE THIS FILE ###
# Before running this R script:
# - This file should be in the same directory as:
#    1. A folder called "Occurrence_Data" containing speices presence points 
#    2. A sub-folder called "Permutations" to store the outputs from this script



# Set Up ------------------------------------------------------------------


# load packages
library(easypackages)
libraries("here","tidyverse")

# set the Isla_MSc_Ch1 folder as the root directory for relative paths (CLUSTER EDIT)
here::i_am("Occurrence_Permutations.R")



# Import Data --------------------------------------------------


## Occurrence Data -----------------------------------------------------

# Import species occurrence points for species, selecting only lon and lat columns
SP1_occ <- read.csv(here("Occurrence_Data","Subadult_BlueParrotfish_PO_Full.csv")) %>% 
  select("longitude","latitude")
SP2_occ <- read.csv(here("Occurrence_Data","Subadult_MidnightParrotfish_PO_Full.csv")) %>% 
  select("longitude","latitude")



# Generate Random Permutations --------------------------------------------


# combine presence points
combined.occ <- rbind(SP1_occ, SP2_occ)

# randomize presence points
set.seed(123)
n_permutations <- 100
permutations_list <- lapply(1:n_permutations, function(x) combined.occ[sample(nrow(combined.occ)), ])

# split randomized combined presence points back into two "species"
for (i in 1:n_permutations) {
  # get i'th permutation
  perm_i <- permutations_list[[i]]
  
  # split combined presence points into "species 1" and "species 2"
  SP1_perm <- perm_i[1:nrow(SP1_occ), ]
  SP2_perm <- perm_i[(nrow(SP1_occ)+1):nrow(perm_i), ]
  
  # write out permuted data to folder
  write.csv(SP1_perm, file = paste0("Occurrence_Data/Permutations/Perm", i, "_Sp1.csv"))
  write.csv(SP2_perm, file = paste0("Occurrence_Data/Permutations/Perm", i, "_Sp2.csv"))
}

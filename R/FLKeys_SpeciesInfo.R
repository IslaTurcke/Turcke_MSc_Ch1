### WELCOME ###

# This script was used to prepare data for Isla Turcke's first MSc chapter 
# in the lab of Dr. Stephanie Green at the University of Alberta (2022-2024). 
# Data are specific to southern Florida and include: Reef Visual Census (RVC) 
# and Mangrove Visual Survey (MVS) fish survey data, and fish characteristic
# data accessed from FishBase through rfishbase. These data were used for habitat
# suitability modeling using maximum entropy.

### TO USE THIS FILE ###
# This is script __ of __ in Isla's data analysis pipeline

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)


### CREATING A SPECIES INFO DATA TABLE for Florida Keys RVC and MVS  ###



# Set Up ------------------------------------------------------------------


# load packages
library(easypackages)
libraries("here", "devtools", "tidyverse", "conflicted", "readxl", "rfishbase")

# prevent conflicts between packages
conflicted::conflict_prefer("filter", "dplyr")

# rvc -> install and load the South Florida Reef Visual Census package from
# https://github.com/jeremiaheb/rvc
# install.packages('devtools')
devtools::install_github('jeremiaheb/rvc')
library(rvc)




# read in my pre-formatted data sets at tibbles
rvc <- read_csv(paste0(species_wd, "RVC_Start.csv"))
mvs <- read_csv(paste0(species_wd, "MVS_Start.csv"))



# Match Species Codes to Dictionaries -------------------------------------


# load species code dictionaries to obtain scientific and common names
rvc_tax <- getTaxonomicData()
mvs_tax <- read.csv(paste0(
  source_wd, "Mangrove_Visual_Survey/SPECIES_CODES_Serafy_BB_MANGROVE_FISH_DATA_1998W_2019W.csv"),
  stringsAsFactors = F)

# make species codes match between rvc, mvs, and both code dictionaries
rvc_tax$SPECIES_CD <- sub(" ", "_", rvc_tax$SPECIES_CD)

# I edited the mvs species codes in excel so that they match the rvc format
# import my dictionary for old -> new mvs codes
mvs_code_dict <- read.csv(paste0(species_wd, "MVS_Code_Dictionary.csv"), row.names = "X")
mvs_tax <- mvs_tax %>%
  left_join(., mvs_code_dict, by = c("Code" = "OLD_CODE")) %>%
  select(SPECIES_CODE, Latin.name, Common.name)

# create vectors of unique species codes for rvc and mvs
species_rvc <- data.frame( sort(unique(rvc$SPECIES_CODE)) )
species_mvs <- data.frame( sort(unique(mvs$SPECIES_CODE)) )
colnames(species_rvc) <- "SPECIES_CODE"
colnames(species_mvs) <- "SPECIES_CODE"
write.csv(species_rvc, paste0(species_wd, "RVC_SpeciesList.csv"))
write.csv(species_mvs, paste0(species_wd, "MVS_SpeciesList.csv"))


# Merge Taxonomic Data into each Species List -----------------------------


# merge in species info by matching code in dictionaries to code in unique species lists
# scientific name, common name, length-weight conversion coefficients
species_rvc <- left_join(species_rvc, rvc_tax, by = c("SPECIES_CODE" = "SPECIES_CD"))
species_rvc <- species_rvc %>% select(SPECIES_CODE, SCINAME, COMNAME, WLEN_A, WLEN_B)
species_mvs <- left_join(species_mvs, mvs_tax, by = c("SPECIES_CODE" = "SPECIES_CODE"))
species_mvs <- species_mvs %>% select(SPECIES_CODE, Latin.name, Common.name)

# make some typo fixes to make mvs match rvc
species_mvs <- species_mvs %>% 
  mutate(Latin.name = str_replace_all(Latin.name, "spp.", "sp.")) %>% 
  mutate(Latin.name = str_replace_all(Latin.name, "Haemulon plumieri", "Haemulon plumierii")) %>% 
  mutate(Latin.name = str_replace_all(Latin.name, "Scarus iserti", "Scarus iseri")) %>% 
  mutate(Latin.name = str_replace_all(Latin.name, "Chilomycterus schoepfi", "Chilomycterus schoepfii")) %>%
  mutate(Latin.name = str_replace_all(Latin.name, "SEE NEXT", "Lagodon/Archosargus sp."))
species_rvc <- species_rvc %>% 
  mutate(SCINAME = str_replace_all(SCINAME, "species", "sp."))

# add info for GOB_DILE
species_rvc[species_rvc$SPECIES_CODE == "GOB_DILE", c("SCINAME","COMNAME")] <- 
  c("Gobiosoma dilepsis", "Orangesided Goby")

# save files in My_Datasets folder
write.csv(species_rvc, paste0(species_wd, "RVC_Species_Info.csv"))
write.csv(species_mvs, paste0(species_wd, "MVS_Species_Info.csv"))

# merge into one list of all unique species
species_info <- merge(species_rvc, species_mvs, by.x = "SPECIES_CODE", by.y = "SPECIES_CODE", 
                      all.x = TRUE, all.y = TRUE)

# fill in missing rvc codes and common names with available mvs info
species_info$SCI_NAME <- if_else(is.na(species_info$SCINAME) == TRUE, 
                                       species_info$Latin.name, species_info$SCINAME)
species_info$COM_NAME <- if_else(is.na(species_info$COMNAME) == TRUE, 
                                     species_info$Common.name, species_info$COMNAME)

# only keep final columns for species code, sci name, common name, and weight from length coefficients
species_info <- species_info %>% select(SPECIES_CODE, SCI_NAME, COM_NAME, WLEN_A, WLEN_B)


# Adding Columns for Species Type -----------------------------------------


# FOCAL SPECIES
species_info$FOCAL_SPE <- ifelse(species_info$SPECIES_CODE %in% c("SCA_COEL","SCA_COER","SCA_COES","SCA_GUAC"), 1, 0)

# NICHE MEMBERS 

# import list of species functionally similar to my focal species
scar_equiv <- read.csv(paste0(species_wd, "Scarus_Equivs.csv"))
species_info$NICHE <- ifelse(species_info$SCI_NAME %in% scar_equiv$SCI_NAME, 1, 0)

# PREDATORS

# to view all records of fish identified only to the genus
# include appropriate ones in my predator dataset in google sheets
##spe_info <- species_info[endsWith(species_info$SPECIES_CODE, '.'), ]

# import predator dataset that I made in google sheets
predator_list <- read.csv(paste0(species_wd, "Scarus_Predators.csv"))
species_info$PREDATOR <- ifelse(species_info$SCI_NAME %in% predator_list$SCI_NAME, 1, 0)


# check to see if I have a and b coefficients for all predators
predator_info <- species_info %>% 
  filter(., species_info$PREDATOR == 1) %>%
  select(., -c("FOCAL_SPE","NICHE"))

# I don't. So next section:


# Filling in length-weight coefficients for predators from rfishbase -------

missing_names <- predator_info[!complete.cases(predator_info), "SCI_NAME"]

missing_coeffs <- length_weight(missing_names, c("Species", "Sex", "Type", "Number", "a", "b")) %>% 
  filter(., Type == "TL" & Sex != "male" & Sex != "female")

missing_coeffs <- missing_coeffs %>% replace_na(list(Number = 1)) %>% 
  group_by(Species) %>% 
  mutate(WL_A = weighted.mean(a, Number), WL_B = weighted.mean(b, Number)) %>% 
  ungroup()

# Adding W-L coefficients to Species Info Table 

# merge in available coefficients
species_info <- merge(species_info, missing_coeffs[!duplicated(missing_coeffs$Species), c("Species","WL_A","WL_B")], 
                      by.x = "SCI_NAME", by.y = "Species", all.x = TRUE, all.y = FALSE)

# fill in missing coefficients with the ones I just merged in
species_info$WLEN_A <- if_else(is.na(species_info$WLEN_A) == TRUE, 
                             species_info$WL_A, species_info$WLEN_A)

species_info$WLEN_B <- if_else(is.na(species_info$WLEN_B) == TRUE, 
                               species_info$WL_B, species_info$WLEN_B)


# Getting length-length coefficients for predators from rfishbase ---------

# coefficients available for TL to FL
len_len_TtoF <- length_length(predator_list$SCI_NAME, c("Species","Length1","Length2","a","b","Number")) %>% 
  filter(., Length1 == "FL" & Length2 == "TL")

# species that did not have that available
missing_len_names <- predator_list %>% filter(!SCI_NAME %in% len_len_TtoF$Species)
missing_len_names <- missing_len_names$SCI_NAME

# these species only have coefficients to go from FL to TL
len_len_FtoT <- length_length(missing_len_names, c("Species","Length1","Length2","a","b","Number")) %>% 
  filter(., Length1 == "TL" & Length2 == "FL")

# for NUMBER, replace NAs with 1 and take weighted average of a and b values

len_len_FtoT <- len_len_FtoT %>% replace_na(list(Number = 1)) %>% 
  group_by(Species) %>% 
  mutate(LL_A = weighted.mean(a, Number), LL_B = weighted.mean(b, Number)) %>% 
  ungroup()

len_len_TtoF <- len_len_TtoF %>% replace_na(list(Number = 1)) %>% 
  group_by(Species) %>% 
  mutate(LL_A_TtoF = weighted.mean(a, Number), LL_B_TtoF = weighted.mean(b, Number)) %>% 
  ungroup()


# Adding L-L coefficients to Species Info Table ---------------------------

# merge in available coefficients for FL to TL 
species_info <- merge(species_info, len_len_FtoT[!duplicated(len_len_FtoT$Species), c("Species","LL_A","LL_B")], 
                        by.x = "SCI_NAME", by.y = "Species", all.x = TRUE, all.y = FALSE)

species_info <- merge(species_info, len_len_TtoF[!duplicated(len_len_TtoF$Species), c("Species","LL_A_TtoF","LL_B_TtoF")], 
                        by.x = "SCI_NAME", by.y = "Species", all.x = TRUE, all.y = FALSE)

# fill in missing coefficients with those available for TL to FL
species_info$LL_A <- if_else(is.na(species_info$LL_A) == TRUE, 
                               species_info$LL_A_TtoF, species_info$LL_A)

species_info$LL_B <- if_else(is.na(species_info$LL_B) == TRUE, 
                               species_info$LL_B_TtoF, species_info$LL_B)

# remove extra LL_A_TtoF and LL_B_TtoF columns
species_info <- species_info %>% select(c(SPECIES_CODE,SCI_NAME,COM_NAME,
                                          FOCAL_SPE,NICHE,PREDATOR,LL_A,LL_B,WLEN_A,WLEN_B))

# make subset of just predator info
predator_info <- species_info %>% filter(PREDATOR == 1)



# Write out Final Data Set  -----------------------------------------------

# write out data set to My_Dataset folder
write_csv(species_info, paste0(species_wd, "Species_Info.csv"), append = FALSE)


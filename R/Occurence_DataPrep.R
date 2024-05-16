### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script is used to prepare the species occurrence datasets for Isla Turcke's first 
# MSc chapter in the lab of Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida and were subsequently used for habitat
# suitability modeling using maximum entropy modelling.

### TO USE THIS FILE ###
# Before running this R script:
# - Make sure the files RVC_Start.csv and MVS_Start.csv are in the "Data_SmallFiles"
#   folder in the MSc_Ch1_DataPrep repository on my GitHub.
# - If they are not, run the script "RVC-MVS_InitialPrep.R".

### PREPARING SCARUS DATASETS FOR HABITAT SUITABILITY MODELLING ###

# combining rvc and mvs datasets

# splitting into 7 datasets:
# 1. all herbivore sub-adults
# 2. Scarus coelestinus sub-adults
# 3. Scarus coeruleus sub-adults
# 4. Scarus gaucamaia sub-adults
# 5. all invertivore sub-adults
# 6. Lut_gris sub-adults
# 7. Hae_sciu sub-adults

# split each of the above into:
# 1. training data (70%) 
# 2. test data (30%)

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)



# Set Up and Organization -----------------------------------------------------


# INSTALL PACKAGES
#install.packages("truncnorm")
#install.packages("reshape2")
#install.packages("spatialEco")

# LOAD PACKAGES
library(easypackages)
libraries("here", "devtools", "tidyverse", "conflicted", "truncnorm", "reshape2", 
          "sp", "sf", "terra", "spatialEco")

# prevent conflicts between packages
conflicted::conflict_prefer("filter", "dplyr")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/MSc_Ch1_DataPrep/R/Occurence_DataPrep.R")

# save PROJ.4 string for NEW and OLD standard projection 
# EPSG:6346 NAD 1983 2011 UTM Zone 17N
new_crs <- crs("+init=epsg:6346")

# READ IN SPECIES OCCURRENCE DATASETS
# pre-cleaned rvc and mvs data for FLA_KEYS 2014, 2016, 2018, 2022
rvc <- as_tibble(read.csv(here("GitHub_Repositories","MSc_Ch1_DataPrep","Data_SmallFiles","Fish","RVC_Start.csv")))
mvs <- as_tibble(read.csv(here("GitHub_Repositories","MSc_Ch1_DataPrep","Data_SmallFiles","Fish","MVS_Start.csv")))

# NOTE: mvs dataset is already presence ONLY

# find number of unique sites for generating sampling bias grid later
rvc_sites <- rvc %>% distinct(ID_SITE, x, y)
mvs_sites <- mvs %>% distinct(ID_SITE, x, y)



# Filter to include only our Focal Species --------------------------------


# blue, midnight, rainbow parrotfish; blue-striped grunt; gray snapper
rvc_focal <- rvc %>% filter(SPECIES_CODE %in% c("SCA_COER","SCA_COEL","SCA_COES",
                                                "SCA_GUAC","LUT_GRIS","HAE_SCIU")) %>% 
  filter(.$NUM > 0.0) # make rvc_focal presence-only
mvs_focal <- mvs %>% filter(SPECIES_CODE %in% c("SCA_COER","SCA_COEL","SCA_COES",
                                                  "SCA_GUAC","LUT_GRIS","HAE_SCIU"))



# Expand MVS Presence - Simulate Individual Fish Lengths ------------------


# set percentile of minimum length value
p <- 0.10

# initialize expanded data frame and fill with rows where NO == 1
mvs_expanded <- mvs_focal %>% 
  filter(NO == 1) %>% 
  mutate(LEN = AVE_LEN) %>%
  select(-MIN_LEN, -AVE_LEN, -MAX_LEN)

# append to expanded data set as appropriate for each line in mvs_focal

for (i in 1:nrow(mvs_focal)) {
  
  if (mvs_focal[i, "NO"] == 2){
    temp <- mvs_focal[rep(i, 2), ] %>% 
      mutate(LEN = c(mvs_focal$MIN_LEN[i], mvs_focal$MAX_LEN[i]), 
             NO = 1, DENSITY = 1/60*100) %>%
      select(-MIN_LEN, -AVE_LEN, -MAX_LEN)
    mvs_expanded <- bind_rows(mvs_expanded, temp)
  }
  if (mvs_focal[i, "NO"] == 3){
    temp <- mvs_focal[rep(i, 3), ] %>% 
      mutate(LEN = c(mvs_focal$MIN_LEN[i], mvs_focal$AVE_LEN[i], mvs_focal$MAX_LEN[i]), 
             NO = 1, DENSITY = 1/60*100) %>%
      select(-MIN_LEN, -AVE_LEN, -MAX_LEN)
    mvs_expanded <- bind_rows(mvs_expanded, temp)
  }
  if (mvs_focal[i, "NO"] > 3){
    stdev <- (mvs_focal$MIN_LEN[i] - mvs_focal$AVE_LEN[i]) / qnorm(p)
    samples <- rtruncnorm(mvs_focal$NO[i]-2 , a = mvs_focal$MIN_LEN[i], b = mvs_focal$MAX_LEN[i],
                          mean = mvs_focal$AVE_LEN[i], sd = stdev)
    samples <- c(samples, mvs_focal$MIN_LEN[i], mvs_focal$MAX_LEN[i])
    
    temp <- mvs_focal[rep(i, mvs_focal$NO[i]), ] %>% 
      mutate(LEN = samples, NO = 1, DENSITY = 1/60*100) %>%
      select(-MIN_LEN, -AVE_LEN, -MAX_LEN)
    mvs_expanded <- bind_rows(mvs_expanded, temp)
  }
}

### Calculation for stdev based on the equations:
#   [1]   Z = (value - mean) / stdev
#   [2]   Z = qnorm(p) , where p is the percentile of the value in [1]

# check out the latest length samples
hist(samples)

# clean up
rm(i, p, samples, stdev, temp)



# Combine RVC and MVS -----------------------------------------------------


# convert fork length (rvc) to total length using values from FishBase
# TL = a + b*FL

rvc_focal["TOT_LEN"] <- NA

for (i in 1:nrow(rvc_focal)) {
  
  if (rvc_focal[i, "SPECIES_CODE"] == "SCA_COER"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.05 
  }
  if (rvc_focal[i, "SPECIES_CODE"] == "SCA_COEL"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.05 # no value in FishBase
  }
  if (rvc_focal[i, "SPECIES_CODE"] == "SCA_COES"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.05 # average of coer and coel
  }
  if (rvc_focal[i, "SPECIES_CODE"] == "SCA_GUAC"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.05
  }
  if (rvc_focal[i, "SPECIES_CODE"] == "LUT_GRIS"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.049
  }
  if (rvc_focal[i, "SPECIES_CODE"] == "HAE_SCIU"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.034
  }
}

# add column to mvs called "NUM" and set equal to current column "NO".
mvs_expanded <- mvs_expanded %>% mutate(NUM = NO, TOT_LEN = LEN)

# combine rvc presence rows (NUM > 0) with mvs rows
all_focal <- rbind(rvc_focal[rvc_focal$NUM > 0.0 ,c("SOURCE","ID_SURV","ID_SITE",
                                                    "x","y","SPECIES_CODE","NUM","TOT_LEN")],
                   mvs_expanded[,c("SOURCE","ID_SURV","ID_SITE","x","y",
                                   "SPECIES_CODE","NUM","TOT_LEN")])



# Filter by Length --------------------------------------------------------


# Filter each species for sub-adults
# (between size at 1 YR and size at maturation) 

# Gray snapper
all_subadults <- all_focal %>% filter(SPECIES_CODE == "LUT_GRIS") %>% 
  filter(.$TOT_LEN >= 9.51 & .$TOT_LEN <= 32.1)

# Blue-striped grunt
temp <- all_focal %>% filter(SPECIES_CODE == "HAE_SCIU") %>% 
  filter(.$TOT_LEN >= 11.9 & .$TOT_LEN <= 25.33)
all_subadults <- rbind(all_subadults, temp)

# Blue parrotfish
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COER") %>% 
  filter(.$TOT_LEN >= 23.59 & .$TOT_LEN <= 35.3)
all_subadults <- rbind(all_subadults, temp)

# Midnight parrotfish
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COEL") %>% 
  filter(.$TOT_LEN >= 24.43 & .$TOT_LEN <= 42.5)
all_subadults <- rbind(all_subadults, temp)

# Blue or Midnight parrotfish (identification not positive)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COES") %>% 
  filter(.$TOT_LEN >= 23.59 & .$TOT_LEN <= 42.5)
all_subadults <- rbind(all_subadults, temp)

# Rainbow parrotfish
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_GUAC") %>% 
  filter(.$TOT_LEN >= 27.31 & .$TOT_LEN <= 42.7)
all_subadults <- rbind(all_subadults, temp)



# Perform Long to Wide Conversion -----------------------------------------


# long to wide conversion, keep species separate when we sum by ID_SURV

# -> long to wide
# -> split COES density between COEL and COER
# -> calculate total numbers of our focal species
all_sub_w <- all_subadults %>% dcast(., SOURCE+ID_SURV+ID_SITE+x+y ~ SPECIES_CODE, 
                               value.var = "NUM", fun.aggregate = sum)

# if there are any SCA_COES present, split them between SCA_COER and SCA_COEL  
if ("SCA_COES" %in% colnames(all_sub_w)) {
    all_sub_w <- all_sub_w %>% mutate(SCA_COEL = SCA_COEL + (SCA_COES / 2), 
                                      SCA_COER = SCA_COER + (SCA_COES / 2)) %>% 
    select(-SCA_COES)
  }
  
# add columns for our two functional groups: herbivores and invertivores
all_sub_w <- all_sub_w %>% mutate(INVERT = HAE_SCIU + LUT_GRIS,
                                  HERB = SCA_COEL + SCA_COER + SCA_GUAC)



# Convert Number of Fish to Binary  ---------------------------------------


# if >= 1 fish was seen at the survey site it's presence [1], else absence [0]

all_sub_PA <- all_sub_w %>% 
  mutate(PRES_COEL = ifelse(SCA_COEL > 0.0, 1, 0), PRES_COER = ifelse(SCA_COER > 0.0, 1, 0),
         PRES_GUAC = ifelse(SCA_GUAC > 0.0, 1, 0), PRES_HERB = ifelse(HERB > 0.0, 1, 0),
         PRES_GRIS = ifelse(LUT_GRIS > 0.0, 1, 0), PRES_SCIU = ifelse(HAE_SCIU > 0.0, 1, 0),
         PRES_INVERT = ifelse(INVERT > 0.0, 1, 0)) %>% 
  select(-c(SCA_COEL, SCA_COER, SCA_GUAC, HERB, LUT_GRIS, HAE_SCIU, INVERT))



# FINAL PRESENCE ONLY DATASETS --------------------------------------------


# separate by species/group and filter out absences
coer <- all_sub_PA %>% select(SOURCE, ID_SURV, x, y, PRES_COER) %>% 
  filter(PRES_COER == 1)
coel <- all_sub_PA %>% select(SOURCE, ID_SURV, x, y, PRES_COEL) %>% 
  filter(PRES_COEL == 1)
guac <- all_sub_PA %>% select(SOURCE, ID_SURV, x, y, PRES_GUAC) %>% 
  filter(PRES_GUAC == 1)
herb <- all_sub_PA %>% select(SOURCE, ID_SURV, x, y, PRES_HERB) %>% 
  filter(PRES_HERB == 1)
gris <- all_sub_PA %>% select(SOURCE, ID_SURV, x, y, PRES_GRIS) %>% 
  filter(PRES_GRIS == 1)
sciu <- all_sub_PA %>% select(SOURCE, ID_SURV, x, y, PRES_SCIU) %>% 
  filter(PRES_SCIU == 1)
invert <- all_sub_PA %>% select(SOURCE, ID_SURV, x, y, PRES_INVERT) %>% 
  filter(PRES_INVERT == 1)



# Clean Up ----------------------------------------------------------------

rm(rvc, mvs, rvc_focal, mvs_focal, mvs_expanded, all_focal, all_subadults, temp,
    all_sub_w, all_sub_PA)



# Split into Training and Testing Data ------------------------------------


# randomly split data for model training and evaluation (70-30%, respectively)
library(ISLR)
# set seed to ensure replicability
set.seed(123)  
# blue parrotfish
coer_train_index <- sample(seq_len(nrow(coer)), size = floor(0.70*nrow(coer)))  
coer_train <- coer[coer_train_index,] 
coer_test <- coer[-coer_train_index,]  
# midnight parrotfish
coel_train_index = sample(seq_len(nrow(coel)), size = floor(0.70*nrow(coel)))  
coel_train = coel[coel_train_index,] 
coel_test = coel[-coel_train_index,]
# rainbow parrotfish
guac_train_index = sample(seq_len(nrow(guac)), size = floor(0.70*nrow(guac)))  
guac_train = guac[guac_train_index,] 
guac_test = guac[-guac_train_index,]
# herbivore functional group
herb_train_index = sample(seq_len(nrow(herb)), size = floor(0.70*nrow(herb)))  
herb_train = herb[herb_train_index,] 
herb_test = herb[-herb_train_index,]
# gray snapper
gris_train_index = sample(seq_len(nrow(gris)), size = floor(0.70*nrow(gris)))  
gris_train = gris[gris_train_index,] 
gris_test = gris[-gris_train_index,]
# bluestriped grunt
sciu_train_index = sample(seq_len(nrow(sciu)), size = floor(0.70*nrow(sciu)))  
sciu_train = sciu[sciu_train_index,] 
sciu_test = sciu[-sciu_train_index,]
# invertivore functional group
invert_train_index = sample(seq_len(nrow(invert)), size = floor(0.70*nrow(invert)))  
invert_train = invert[invert_train_index,] 
invert_test = invert[-invert_train_index,]



# Write out Datasets ------------------------------------------------------


# Midnight parrotfish
write_csv(coel_train, here("Final_Data","Species_Training","Midnight_Parrotfish_PO_Train.csv")
          , append = F)
write_csv(coel_test, here("Final_Data","Species_Testing","Midnight_Parrotfish_PO_Test.csv")
          , append = F)
# Blue parrotfish
write_csv(coer_train, here("Final_Data","Species_Training","Blue_Parrotfish_PO_Train.csv")
          , append = F)
write_csv(coer_test, here("Final_Data","Species_Testing","Blue_Parrotfish_PO_Test.csv")
          , append = F)
# Rainbow parrotfish
write_csv(guac_train, here("Final_Data","Species_Training","Rainbow_Parrotfish_PO_Train.csv")
          , append = F)
write_csv(guac_test, here("Final_Data","Species_Testing","Rainbow_Parrotfish_PO_Test.csv")
          , append = F)
# Gray Snapper
write_csv(gris_train, here("Final_Data","Species_Training","Gray_Snapper_PO_Train.csv")
          , append = F)
write_csv(gris_test, here("Final_Data","Species_Testing","Gray_Snapper_PO_Test.csv")
          , append = F)
# Bluestriped Grunt
write_csv(sciu_train, here("Final_Data","Species_Training","Bluestriped_Grunt_PO_Train.csv")
          , append = F)
write_csv(sciu_test, here("Final_Data","Species_Testing","Bluestriped_Grunt_PO_Test.csv")
          , append = F)
# Herbivores
write_csv(herb_train, here("Final_Data","Species_Training","Herbivores_PO_Train.csv")
          , append = F)
write_csv(herb_test, here("Final_Data","Species_Testing","Herbivores_PO_Test.csv")
          , append = F)
# Invertivores
write_csv(invert_train, here("Final_Data","Species_Training","Invertivores_PO_Train.csv")
          , append = F)
write_csv(invert_test, here("Final_Data","Species_Testing","Invertivores_PO_Test.csv")
          , append = F)



# Sampling Bias Grid ------------------------------------------------------


# create sampling effort raster to parse out sampling bias: rule of thumb for
# selecting bandwidth according to Scott (1992) and Bowman and Azzalini (1997)
choose_bw = function(spdf) {
  X = coordinates(spdf)
  sigma = c(sd(X[,1]), sd(X[,2])) * (2 / (3 * nrow(X))) ^ (1/6)
}

# creating spatial pixels data frame of sampling effort
sampling_effort <- full_join(rvc_sites, mvs_sites) %>%
  select(x, y) %>%
  st_as_sf(., coords = c(1, 2), crs = new_crs) %>%
  add_column("count" = 1)
sampling_effort_spdf <- sampling_effort %>% as(., "Spatial")

# run choose bandwidth function on sampling effort spdf
domain_bw <- choose_bw(sampling_effort_spdf)

# create a template grid using the study region raster as a guide
domain_grid <- rast(here("Final_Data","Study_Region.tif"))

# clean up
rm(mvs, mvs_sites, rvc, rvc_sites, sampling_effort_spdf)
gc()

# calculate kernel density surface
kde <- sp.kde(x = sampling_effort, bw = domain_bw, ref = domain_grid, res = 5,
                     standardize = T)

# save initial bias grid because that step takes a long time
writeRaster(domain_kde, here("Intermediate_Data","initial_bias_grid.tif"), overwrite = T)
kde <- terra::rast(here("Intermediate_Data","initial_bias_grid.tif"))

# resample because even though I asked for 5 x 5 m it does not give that :(
domain_kde <- terra::resample(kde, domain_grid, "bilinear", threads = T)

# add small constant value because bias grid can't have 0 in MaxEnt
domain_kde <- domain_kde + 0.0001

# save bias grid to Final Data Folder
terra::writeRaster(domain_kde, here("Final_Data","Sampling_Bias.tif"), overwrite = T)

# read in using raster package to write it out as an ASCII file
kde_raster <- raster::raster(here("Final_Data","Sampling_Bias.tif"))
raster::writeRaster(kde_raster, here("Final_Data","Final_ascii","Sampling_Bias.asc"),
                    format = "ascii", overwrite = T)

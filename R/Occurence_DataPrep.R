### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script is used to prepare the species occurrence datasets for Isla Turcke's first 
# MSc chapter in the lab of Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida and were subsequently used for habitat
# suitability modeling using maximum entropy modelling.

### TO USE THIS FILE ###
# Before running this R script:
# - Make sure the files RVC_Start.csv and MVS_Start.csv are in the "Data_SmallFiles"
#   folder in the Turcke_MSc_Ch1 repository on my GitHub.
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
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/Occurence_DataPrep.R")

# save PROJ.4 string for NEW and OLD standard projection 
# EPSG:6346 NAD 1983 2011 UTM Zone 17N
new_crs <- crs("+init=epsg:6346")

# READ IN SPECIES OCCURRENCE DATASETS
# pre-cleaned rvc and mvs data for FLA_KEYS 2014, 2016, 2018, 2022
rvc <- as_tibble(read.csv(here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","Fish","RVC_Start.csv")))
mvs <- as_tibble(read.csv(here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","Fish","MVS_Start.csv")))

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


# Filter each species for juveniles, sub-adults, and adults
# (between size at 1 YR and size at maturation) 

# Gray snapper
all_juvs <- all_focal %>% filter(SPECIES_CODE == "LUT_GRIS") %>% filter(.$TOT_LEN < 9.51)
all_subadults <- all_focal %>% filter(SPECIES_CODE == "LUT_GRIS") %>% filter(.$TOT_LEN >= 9.51 & .$TOT_LEN <= 32.1)
all_adults <- all_focal %>% filter(SPECIES_CODE == "LUT_GRIS") %>% filter(.$TOT_LEN > 32.1)

# Blue-striped grunt
temp <- all_focal %>% filter(SPECIES_CODE == "HAE_SCIU") %>% filter(.$TOT_LEN < 11.9)
all_juvs <- rbind(all_juvs, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "HAE_SCIU") %>% filter(.$TOT_LEN >= 11.9 & .$TOT_LEN <= 25.33)
all_subadults <- rbind(all_subadults, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "HAE_SCIU") %>% filter(.$TOT_LEN > 25.33)
all_adults <- rbind(all_adults, temp)

# Blue parrotfish
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COER") %>% filter(.$TOT_LEN < 23.59)
all_juvs <- rbind(all_juvs, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COER") %>% filter(.$TOT_LEN >= 23.59 & .$TOT_LEN <= 35.3)
all_subadults <- rbind(all_subadults, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COER") %>% filter(.$TOT_LEN > 35.3)
all_adults <- rbind(all_adults, temp)

# Midnight parrotfish
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COEL") %>% filter(.$TOT_LEN < 24.43)
all_juvs <- rbind(all_juvs, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COEL") %>% filter(.$TOT_LEN >= 24.43 & .$TOT_LEN <= 42.5)
all_subadults <- rbind(all_subadults, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COEL") %>% filter(.$TOT_LEN > 42.5)
all_adults <- rbind(all_adults, temp)

# Blue or Midnight parrotfish (identification not positive)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COES") %>% filter(.$TOT_LEN < 23.59)
all_juvs <- rbind(all_juvs, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COES") %>% filter(.$TOT_LEN >= 23.59 & .$TOT_LEN <= 42.5)
all_subadults <- rbind(all_subadults, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COES") %>% filter(.$TOT_LEN > 42.5)
all_adults <- rbind(all_adults, temp)

# Rainbow parrotfish
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_GUAC") %>% filter(.$TOT_LEN < 27.3)
all_juvs <- rbind(all_juvs, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_GUAC") %>% filter(.$TOT_LEN >= 27.31 & .$TOT_LEN <= 42.7)
all_subadults <- rbind(all_subadults, temp)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_GUAC") %>% filter(.$TOT_LEN > 42.7)
all_adults <- rbind(all_adults, temp)



# Perform Long to Wide Conversion -----------------------------------------


# long to wide conversion, keep species separate when we sum by ID_SURV

# -> long to wide
# -> split COES density between COEL and COER
# -> calculate total numbers of our focal species
all_juv_w <- all_juvs %>% dcast(., SOURCE+ID_SURV+ID_SITE+x+y ~ SPECIES_CODE, 
                               value.var = "NUM", fun.aggregate = sum)
all_sub_w <- all_subadults %>% dcast(., SOURCE+ID_SURV+ID_SITE+x+y ~ SPECIES_CODE, 
                                     value.var = "NUM", fun.aggregate = sum)
all_adt_w <- all_adults %>% dcast(., SOURCE+ID_SURV+ID_SITE+x+y ~ SPECIES_CODE, 
                                     value.var = "NUM", fun.aggregate = sum)

# if there are any SCA_COES present, split them between SCA_COER and SCA_COEL  
if ("SCA_COES" %in% colnames(all_juv_w)) {
    all_juv_w <- all_juv_w %>% mutate(SCA_COEL = SCA_COEL + (SCA_COES / 2), 
                                      SCA_COER = SCA_COER + (SCA_COES / 2)) %>% 
    select(-SCA_COES)
}
if ("SCA_COES" %in% colnames(all_sub_w)) {
  all_sub_w <- all_sub_w %>% mutate(SCA_COEL = SCA_COEL + (SCA_COES / 2), 
                                    SCA_COER = SCA_COER + (SCA_COES / 2)) %>% 
    select(-SCA_COES)
}
if ("SCA_COES" %in% colnames(all_adt_w)) {
  all_adt_w <- all_adt_w %>% mutate(SCA_COEL = SCA_COEL + (SCA_COES / 2), 
                                    SCA_COER = SCA_COER + (SCA_COES / 2)) %>% 
    select(-SCA_COES)
}
  
# add columns for our two functional groups: herbivores and invertivores
all_juv_w <- all_juv_w %>% mutate(INVERT = HAE_SCIU + LUT_GRIS,
                                  HERB = SCA_COEL + SCA_COER + SCA_GUAC)
all_sub_w <- all_sub_w %>% mutate(INVERT = HAE_SCIU + LUT_GRIS,
                                  HERB = SCA_COEL + SCA_COER + SCA_GUAC)
all_adt_w <- all_adt_w %>% mutate(INVERT = HAE_SCIU + LUT_GRIS,
                                  HERB = SCA_COEL + SCA_COER + SCA_GUAC)



# Convert Number of Fish to Binary  ---------------------------------------


# if >= 1 fish was seen at the survey site it's presence [1], else absence [0]

all_juv_PA <- all_juv_w %>% 
  mutate(PRES_COEL = ifelse(SCA_COEL > 0.0, 1, 0), PRES_COER = ifelse(SCA_COER > 0.0, 1, 0),
         PRES_GUAC = ifelse(SCA_GUAC > 0.0, 1, 0), PRES_HERB = ifelse(HERB > 0.0, 1, 0),
         PRES_GRIS = ifelse(LUT_GRIS > 0.0, 1, 0), PRES_SCIU = ifelse(HAE_SCIU > 0.0, 1, 0),
         PRES_INVERT = ifelse(INVERT > 0.0, 1, 0)) %>% 
  select(-c(SCA_COEL, SCA_COER, SCA_GUAC, HERB, LUT_GRIS, HAE_SCIU, INVERT))
all_sub_PA <- all_sub_w %>% 
  mutate(PRES_COEL = ifelse(SCA_COEL > 0.0, 1, 0), PRES_COER = ifelse(SCA_COER > 0.0, 1, 0),
         PRES_GUAC = ifelse(SCA_GUAC > 0.0, 1, 0), PRES_HERB = ifelse(HERB > 0.0, 1, 0),
         PRES_GRIS = ifelse(LUT_GRIS > 0.0, 1, 0), PRES_SCIU = ifelse(HAE_SCIU > 0.0, 1, 0),
         PRES_INVERT = ifelse(INVERT > 0.0, 1, 0)) %>% 
  select(-c(SCA_COEL, SCA_COER, SCA_GUAC, HERB, LUT_GRIS, HAE_SCIU, INVERT))
all_adt_PA <- all_adt_w %>% 
  mutate(PRES_COEL = ifelse(SCA_COEL > 0.0, 1, 0), PRES_COER = ifelse(SCA_COER > 0.0, 1, 0),
         PRES_GUAC = ifelse(SCA_GUAC > 0.0, 1, 0), PRES_HERB = ifelse(HERB > 0.0, 1, 0),
         PRES_GRIS = ifelse(LUT_GRIS > 0.0, 1, 0), PRES_SCIU = ifelse(HAE_SCIU > 0.0, 1, 0),
         PRES_INVERT = ifelse(INVERT > 0.0, 1, 0)) %>% 
  select(-c(SCA_COEL, SCA_COER, SCA_GUAC, HERB, LUT_GRIS, HAE_SCIU, INVERT))

# write out to species occurrence folders
write_csv(all_juv_PA, here("Final_Data","Species_Occurrence","Juvenile",
                           "Juvenile_PA_Full.csv"), append = F)
write_csv(all_sub_PA, here("Final_Data","Species_Occurrence","Subadult",
                           "Subadult_PA_Full.csv"), append = F)
write_csv(all_adt_PA, here("Final_Data","Species_Occurrence","Adult",
                           "Adult_PA_Full.csv"), append = F)
# write out subadult data to GitHub repository
write_csv(all_sub_PA, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles",
                           "Occurrence_Subadult_PA_Full.csv"), append = F)



# Final FULL PA Datasets --------------------------------------------


# separate by species/group
# make binary presence (0/1) a factor and add PRES2 as a factor
# add a column for species code

# juveniles
coer_juv_PA <- all_juv_PA %>% mutate(PRES = as.factor(PRES_COER), PRES2 = as.factor(ifelse(all_juv_PA$PRES_COER == 1, "PRESENCE","ABSENCE"))) %>% 
  mutate(SPECIES_CODE = "SCA_COER") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
coel_juv_PA <- all_juv_PA %>% mutate(PRES = as.factor(PRES_COEL), PRES2 = as.factor(ifelse(all_juv_PA$PRES_COEL == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "SCA_COEL") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
guac_juv_PA <- all_juv_PA %>% mutate(PRES = as.factor(PRES_GUAC), PRES2 = as.factor(ifelse(all_juv_PA$PRES_GUAC == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "SCA_GUAC") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
herb_juv_PA <- all_juv_PA %>% mutate(PRES = as.factor(PRES_HERB), PRES2 = as.factor(ifelse(all_juv_PA$PRES_HERB == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "HERB") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
gris_juv_PA <- all_juv_PA %>% mutate(PRES = as.factor(PRES_GRIS), PRES2 = as.factor(ifelse(all_juv_PA$PRES_GRIS == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "LUT_GRIS") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
sciu_juv_PA <- all_juv_PA %>% mutate(PRES = as.factor(PRES_SCIU), PRES2 = as.factor(ifelse(all_juv_PA$PRES_SCIU == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "HAE_SCIU") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
invert_juv_PA <- all_juv_PA %>% mutate(PRES = as.factor(PRES_INVERT), PRES2 = as.factor(ifelse(all_juv_PA$PRES_INVERT == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "INVERT") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)

# subadults
coer_sub_PA <- all_sub_PA %>% mutate(PRES = as.factor(PRES_COER), PRES2 = as.factor(ifelse(all_sub_PA$PRES_COER == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "SCA_COER") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
coel_sub_PA <- all_sub_PA %>% mutate(PRES = as.factor(PRES_COEL), PRES2 = as.factor(ifelse(all_sub_PA$PRES_COEL == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "SCA_COEL") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
guac_sub_PA <- all_sub_PA %>% mutate(PRES = as.factor(PRES_GUAC), PRES2 = as.factor(ifelse(all_sub_PA$PRES_GUAC == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "SCA_GUAC") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
herb_sub_PA <- all_sub_PA %>% mutate(PRES = as.factor(PRES_HERB), PRES2 = as.factor(ifelse(all_sub_PA$PRES_HERB == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "HERB") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
gris_sub_PA <- all_sub_PA %>% mutate(PRES = as.factor(PRES_GRIS), PRES2 = as.factor(ifelse(all_sub_PA$PRES_GRIS == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "LUT_GRIS") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
sciu_sub_PA <- all_sub_PA %>% mutate(PRES = as.factor(PRES_SCIU), PRES2 = as.factor(ifelse(all_sub_PA$PRES_SCIU == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "HAE_SCIU") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
invert_sub_PA <- all_sub_PA %>% mutate(PRES = as.factor(PRES_INVERT), PRES2 = as.factor(ifelse(all_sub_PA$PRES_INVERT == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "INVERT") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)

# adults
coer_adt_PA <- all_adt_PA %>% mutate(PRES = as.factor(PRES_COER), PRES2 = as.factor(ifelse(all_adt_PA$PRES_COER == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "SCA_COER") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
coel_adt_PA <- all_adt_PA %>% mutate(PRES = as.factor(PRES_COEL), PRES2 = as.factor(ifelse(all_adt_PA$PRES_COEL == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "SCA_COEL") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
guac_adt_PA <- all_adt_PA %>% mutate(PRES = as.factor(PRES_GUAC), PRES2 = as.factor(ifelse(all_adt_PA$PRES_GUAC == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "SCA_GUAC") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
herb_adt_PA <- all_adt_PA %>% mutate(PRES = as.factor(PRES_HERB), PRES2 = as.factor(ifelse(all_adt_PA$PRES_HERB == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "HERB") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
gris_adt_PA <- all_adt_PA %>% mutate(PRES = as.factor(PRES_GRIS), PRES2 = as.factor(ifelse(all_adt_PA$PRES_GRIS == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "LUT_GRIS") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
sciu_adt_PA <- all_adt_PA %>% mutate(PRES = as.factor(PRES_SCIU), PRES2 = as.factor(ifelse(all_adt_PA$PRES_SCIU == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "HAE_SCIU") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)
invert_adt_PA <- all_adt_PA %>% mutate(PRES = as.factor(PRES_INVERT), PRES2 = as.factor(ifelse(all_adt_PA$PRES_INVERT == 1, "PRESENCE","ABSENCE"))) %>%
  mutate(SPECIES_CODE = "INVERT") %>% select(SPECIES_CODE, SOURCE, ID_SURV, x, y, PRES, PRES2)

# clean up
rm(rvc, mvs, rvc_focal, mvs_focal, mvs_expanded, all_focal, all_juvs, all_subadults, all_adults,
   all_juv_w, all_sub_w, all_adt_w, all_juv_PA, all_sub_PA, all_adt_PA, temp, i)

# save FULL PA datasets
# blue parrotfish
write_csv(coer_juv_PA, here("Final_Data","Species_Occurrence","Juvenile","Juvenile_BlueParrotfish_PA_Full.csv"),
          append = F)
write_csv(coer_sub_PA, here("Final_Data","Species_Occurrence","Subadult","Subadult_BlueParrotfish_PA_Full.csv"),
          append = F)
write_csv(coer_adt_PA, here("Final_Data","Species_Occurrence","Adult","Adult_BlueParrotfish_PA_Full.csv"),
          append = F)
# midnight parrotfish
write_csv(coel_juv_PA, here("Final_Data","Species_Occurrence","Juvenile","Juvenile_MidnightParrotfish_PA_Full.csv"),
          append = F)
write_csv(coel_sub_PA, here("Final_Data","Species_Occurrence","Subadult","Subadult_MidnightParrotfish_PA_Full.csv"),
          append = F)
write_csv(coel_adt_PA, here("Final_Data","Species_Occurrence","Adult","Adult_MidnightParrotfish_PA_Full.csv"),
          append = F)
# rainbow parrotfish
write_csv(guac_juv_PA, here("Final_Data","Species_Occurrence","Juvenile","Juvenile_RainbowParrotfish_PA_Full.csv"),
          append = F)
write_csv(guac_sub_PA, here("Final_Data","Species_Occurrence","Subadult","Subadult_RainbowParrotfish_PA_Full.csv"),
          append = F)
write_csv(guac_adt_PA, here("Final_Data","Species_Occurrence","Adult","Adult_RainbowParrotfish_PA_Full.csv"),
          append = F)
# bluestriped grunt
write_csv(sciu_juv_PA, here("Final_Data","Species_Occurrence","Juvenile","Juvenile_BluestripedGrunt_PA_Full.csv"),
          append = F)
write_csv(sciu_sub_PA, here("Final_Data","Species_Occurrence","Subadult","Subadult_BluestripedGrunt_PA_Full.csv"),
          append = F)
write_csv(sciu_adt_PA, here("Final_Data","Species_Occurrence","Adult","Adult_BluestripedGrunt_PA_Full.csv"),
          append = F)
# gray snapper
write_csv(gris_juv_PA, here("Final_Data","Species_Occurrence","Juvenile","Juvenile_GraySnapper_PA_Full.csv"),
          append = F)
write_csv(gris_sub_PA, here("Final_Data","Species_Occurrence","Subadult","Subadult_GraySnapper_PA_Full.csv"),
          append = F)
write_csv(gris_adt_PA, here("Final_Data","Species_Occurrence","Adult","Adult_GraySnapper_PA_Full.csv"),
          append = F)
# herbivores
write_csv(herb_juv_PA, here("Final_Data","Species_Occurrence","Juvenile","Juvenile_Herbivore_PA_Full.csv"),
          append = F)
write_csv(herb_sub_PA, here("Final_Data","Species_Occurrence","Subadult","Subadult_Herbivore_PA_Full.csv"),
          append = F)
write_csv(herb_adt_PA, here("Final_Data","Species_Occurrence","Adult","Adult_Herbivore_PA_Full.csv"),
          append = F)
# invertivores
write_csv(invert_juv_PA, here("Final_Data","Species_Occurrence","Juvenile","Juvenile_Invertivore_PA_Full.csv"),
          append = F)
write_csv(invert_sub_PA, here("Final_Data","Species_Occurrence","Subadult","Subadult_Invertivore_PA_Full.csv"),
          append = F)
write_csv(invert_adt_PA, here("Final_Data","Species_Occurrence","Adult","Adult_Invertivore_PA_Full.csv"),
          append = F)



# Final FULL PO Datasets --------------------------------------------


# blue parrotfish
write_csv(coer_juv_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Juvenile","Juvenile_BlueParrotfish_PO_Full.csv"), append = F)
write_csv(coer_sub_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Subadult","Subadult_BlueParrotfish_PO_Full.csv"), append = F)
write_csv(coer_adt_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Adult","Adult_BlueParrotfish_PO_Full.csv"), append = F)
# midnight parrotfish
write_csv(coel_juv_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Juvenile","Juvenile_MidnightParrotfish_PO_Full.csv"), append = F)
write_csv(coel_sub_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Subadult","Subadult_MidnightParrotfish_PO_Full.csv"), append = F)
write_csv(coel_adt_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Adult","Adult_MidnightParrotfish_PO_Full.csv"), append = F)
# rainbow parrotfish
write_csv(guac_juv_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Juvenile","Juvenile_RainbowParrotfish_PO_Full.csv"), append = F)
write_csv(guac_sub_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Subadult","Subadult_RainbowParrotfish_PO_Full.csv"), append = F)
write_csv(guac_adt_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Adult","Adult_RainbowParrotfish_PO_Full.csv"), append = F)
# bluestriped grunt
write_csv(sciu_juv_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Juvenile","Juvenile_BluestripedGrunt_PO_Full.csv"), append = F)
write_csv(sciu_sub_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Subadult","Subadult_BluestripedGrunt_PO_Full.csv"), append = F)
write_csv(sciu_adt_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Adult","Adult_BluestripedGrunt_PO_Full.csv"), append = F)
# gray snapper
write_csv(gris_juv_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Juvenile","Juvenile_GraySnapper_PO_Full.csv"), append = F)
write_csv(gris_sub_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Subadult","Subadult_GraySnapper_PO_Full.csv"), append = F)
write_csv(gris_adt_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Adult","Adult_GraySnapper_PO_Full.csv"), append = F)
# herbivores
write_csv(herb_juv_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Juvenile","Juvenile_Herbivore_PO_Full.csv"), append = F)
write_csv(herb_sub_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Subadult","Subadult_Herbivore_PO_Full.csv"), append = F)
write_csv(herb_adt_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Adult","Adult_Herbivore_PO_Full.csv"), append = F)
# invertivores
write_csv(invert_juv_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Juvenile","Juvenile_Invertivore_PO_Full.csv"), append = F)
write_csv(invert_sub_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Subadult","Subadult_Invertivore_PO_Full.csv"), append = F)
write_csv(invert_adt_PA %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y), 
          here("Final_Data","Species_Occurrence","Adult","Adult_Invertivore_PO_Full.csv"), append = F)



# Split into Training and Testing Data ------------------------------------


# randomly split data for model training and evaluation (70-30%, respectively)

library(ISLR)
# set seed to ensure replicability
set.seed(123)  

# blue parrotfish
train_index <- sample(seq_len(nrow(coer_juv_PA)), size = ceiling(0.70*nrow(coer_juv_PA))) 
coer_juv_PO_train <- coer_juv_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y) 
coer_juv_PO_test <- coer_juv_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(coer_sub_PA)), size = ceiling(0.70*nrow(coer_sub_PA)))  
coer_sub_PO_train <- coer_sub_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
coer_sub_PO_test <- coer_sub_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(coer_adt_PA)), size = ceiling(0.70*nrow(coer_adt_PA)))  
coer_adt_PO_train <- coer_adt_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
coer_adt_PO_test <- coer_adt_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
# midnight parrotfish
train_index <- sample(seq_len(nrow(coel_juv_PA)), size = ceiling(0.70*nrow(coel_juv_PA)))  
coel_juv_PO_train <- coel_juv_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y) 
coel_juv_PO_test <- coel_juv_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(coel_sub_PA)), size = ceiling(0.70*nrow(coel_sub_PA)))  
coel_sub_PO_train <- coel_sub_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
coel_sub_PO_test <- coel_sub_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(coel_adt_PA)), size = ceiling(0.70*nrow(coel_adt_PA)))  
coel_adt_PO_train <- coel_adt_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
coel_adt_PO_test <- coel_adt_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
# rainbow parrotfish
train_index <- sample(seq_len(nrow(guac_juv_PA)), size = ceiling(0.70*nrow(guac_juv_PA)))  
guac_juv_PO_train <- guac_juv_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y) 
guac_juv_PO_test <- guac_juv_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(guac_sub_PA)), size = ceiling(0.70*nrow(guac_sub_PA)))  
guac_sub_PO_train <- guac_sub_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
guac_sub_PO_test <- guac_sub_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(guac_adt_PA)), size = ceiling(0.70*nrow(guac_adt_PA)))  
guac_adt_PO_train <- guac_adt_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
guac_adt_PO_test <- guac_adt_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
# herbivore functional group
train_index <- sample(seq_len(nrow(herb_juv_PA)), size = ceiling(0.70*nrow(herb_juv_PA)))  
herb_juv_PO_train <- herb_juv_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y) 
herb_juv_PO_test <- herb_juv_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(herb_sub_PA)), size = ceiling(0.70*nrow(herb_sub_PA)))  
herb_sub_PO_train <- herb_sub_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
herb_sub_PO_test <- herb_sub_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(herb_adt_PA)), size = ceiling(0.70*nrow(herb_adt_PA)))  
herb_adt_PO_train <- herb_adt_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
herb_adt_PO_test <- herb_adt_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
# gray snapper
train_index <- sample(seq_len(nrow(gris_juv_PA)), size = ceiling(0.70*nrow(gris_juv_PA)))  
gris_juv_PO_train <- gris_juv_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y) 
gris_juv_PO_test <- gris_juv_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(gris_sub_PA)), size = ceiling(0.70*nrow(gris_sub_PA)))  
gris_sub_PO_train <- gris_sub_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
gris_sub_PO_test <- gris_sub_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(gris_adt_PA)), size = ceiling(0.70*nrow(gris_adt_PA)))  
gris_adt_PO_train <- gris_adt_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
gris_adt_PO_test <- gris_adt_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
# bluestriped grunt
train_index <- sample(seq_len(nrow(sciu_juv_PA)), size = ceiling(0.70*nrow(sciu_juv_PA)))  
sciu_juv_PO_train <- sciu_juv_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y) 
sciu_juv_PO_test <- sciu_juv_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(sciu_sub_PA)), size = ceiling(0.70*nrow(sciu_sub_PA)))  
sciu_sub_PO_train <- sciu_sub_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
sciu_sub_PO_test <- sciu_sub_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(sciu_adt_PA)), size = ceiling(0.70*nrow(sciu_adt_PA)))  
sciu_adt_PO_train <- sciu_adt_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
sciu_adt_PO_test <- sciu_adt_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
# invertivore functional group
train_index <- sample(seq_len(nrow(invert_juv_PA)), size = ceiling(0.70*nrow(invert_juv_PA)))  
invert_juv_PO_train <- invert_juv_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y) 
invert_juv_PO_test <- invert_juv_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(invert_sub_PA)), size = ceiling(0.70*nrow(invert_sub_PA)))  
invert_sub_PO_train <- invert_sub_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
invert_sub_PO_test <- invert_sub_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
train_index <- sample(seq_len(nrow(invert_adt_PA)), size = ceiling(0.70*nrow(invert_adt_PA)))  
invert_adt_PO_train <- invert_adt_PA[train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)
invert_adt_PO_test <- invert_adt_PA[-train_index,] %>% filter(PRES == 1) %>% select(SPECIES_CODE, x, y)

# clean up
rm(train_index)



# Write out PO Train/Test Datasets ------------------------------------------------------


# Midnight parrotfish
write_csv(coel_juv_PO_train, here("Final_Data","Species_Occurrence","Juvenile","Training",
                               "MidnightParrotfish_Juvenile_PO_Train.csv"), append = F)
write_csv(coel_sub_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training",
                               "MidnightParrotfish_Subadult_PO_Train.csv"), append = F)
write_csv(coel_adt_PO_train, here("Final_Data","Species_Occurrence","Adult","Training",
                               "MidnightParrotfish_Adult_PO_Train.csv"), append = F)
write_csv(coel_juv_PO_test, here("Final_Data","Species_Occurrence","Juvenile","Testing",
                               "MidnightParrotfish_Juvenile_PO_Test.csv"), append = F)
write_csv(coel_sub_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing",
                               "MidnightParrotfish_Subadult_PO_Test.csv"), append = F)
write_csv(coel_adt_PO_test, here("Final_Data","Species_Occurrence","Adult","Testing",
                               "MidnightParrotfish_Adult_PO_Test.csv"), append = F)
# Blue parrotfish
write_csv(coer_juv_PO_train, here("Final_Data","Species_Occurrence","Juvenile","Training",
                               "BlueParrotfish_Juvenile_PO_Train.csv"), append = F)
write_csv(coer_sub_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training",
                               "BlueParrotfish_Subadult_PO_Train.csv"), append = F)
write_csv(coer_adt_PO_train, here("Final_Data","Species_Occurrence","Adult","Training",
                               "BlueParrotfish_Adult_PO_Train.csv"), append = F)
write_csv(coer_juv_PO_test, here("Final_Data","Species_Occurrence","Juvenile","Testing",
                              "BlueParrotfish_Juvenile_PO_Test.csv"), append = F)
write_csv(coer_sub_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing",
                              "BlueParrotfish_Subadult_PO_Test.csv"), append = F)
write_csv(coer_adt_PO_test, here("Final_Data","Species_Occurrence","Adult","Testing",
                              "BlueParrotfish_Adult_PO_Test.csv"), append = F)
# Rainbow parrotfish
write_csv(guac_juv_PO_train, here("Final_Data","Species_Occurrence","Juvenile","Training",
                               "RainbowParrotfish_Juvenile_PO_Train.csv"), append = F)
write_csv(guac_sub_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training",
                               "RainbowParrotfish_Subadult_PO_Train.csv"), append = F)
write_csv(guac_adt_PO_train, here("Final_Data","Species_Occurrence","Adult","Training",
                               "RainbowParrotfish_Adult_PO_Train.csv"), append = F)
write_csv(guac_juv_PO_test, here("Final_Data","Species_Occurrence","Juvenile","Testing",
                              "RainbowParrotfish_Juvenile_PO_Test.csv"), append = F)
write_csv(guac_sub_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing",
                              "RainbowParrotfish_Subadult_PO_Test.csv"), append = F)
write_csv(guac_adt_PO_test, here("Final_Data","Species_Occurrence","Adult","Testing",
                              "RainbowParrotfish_Adult_PO_Test.csv"), append = F)
# Gray Snapper
write_csv(gris_juv_PO_train, here("Final_Data","Species_Occurrence","Juvenile","Training",
                               "GraySnapper_Juvenile_PO_Train.csv"), append = F)
write_csv(gris_sub_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training",
                               "GraySnapper_Subadult_PO_Train.csv"), append = F)
write_csv(gris_adt_PO_train, here("Final_Data","Species_Occurrence","Adult","Training",
                               "GraySnapper_Adult_PO_Train.csv"), append = F)
write_csv(gris_juv_PO_test, here("Final_Data","Species_Occurrence","Juvenile","Testing",
                              "GraySnapper_Juvenile_PO_Test.csv"), append = F)
write_csv(gris_sub_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing",
                              "GraySnapper_Subadult_PO_Test.csv"), append = F)
write_csv(gris_adt_PO_test, here("Final_Data","Species_Occurrence","Adult","Testing",
                              "GraySnapper_Adult_PO_Test.csv"), append = F)
# Bluestriped Grunt
write_csv(sciu_juv_PO_train, here("Final_Data","Species_Occurrence","Juvenile","Training",
                               "BluestripedGrunt_Juvenile_PO_Train.csv"), append = F)
write_csv(sciu_sub_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training",
                               "BluestripedGrunt_Subadult_PO_Train.csv"), append = F)
write_csv(sciu_adt_PO_train, here("Final_Data","Species_Occurrence","Adult","Training",
                               "BluestripedGrunt_Adult_PO_Train.csv"), append = F)
write_csv(sciu_juv_PO_test, here("Final_Data","Species_Occurrence","Juvenile","Testing",
                              "BluestripedGrunt_Juvenile_PO_Test.csv"), append = F)
write_csv(sciu_sub_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing",
                              "BluestripedGrunt_Subadult_PO_Test.csv"), append = F)
write_csv(sciu_adt_PO_test, here("Final_Data","Species_Occurrence","Adult","Testing",
                              "BluestripedGrunt_Adult_PO_Test.csv"), append = F)
# Herbivores
write_csv(herb_juv_PO_train, here("Final_Data","Species_Occurrence","Juvenile","Training",
                               "Herbivore_Juvenile_PO_Train.csv"), append = F)
write_csv(herb_sub_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training",
                               "Herbivore_Subadult_PO_Train.csv"), append = F)
write_csv(herb_adt_PO_train, here("Final_Data","Species_Occurrence","Adult","Training",
                               "Herbivore_Adult_PO_Train.csv"), append = F)
write_csv(herb_juv_PO_test, here("Final_Data","Species_Occurrence","Juvenile","Testing",
                              "Herbivore_Juvenile_PO_Test.csv"), append = F)
write_csv(herb_sub_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing",
                              "Herbivore_Subadult_PO_Test.csv"), append = F)
write_csv(herb_adt_PO_test, here("Final_Data","Species_Occurrence","Adult","Testing",
                              "Herbivore_Adult_PO_Test.csv"), append = F)
# Invertivores
write_csv(invert_juv_PO_train, here("Final_Data","Species_Occurrence","Juvenile","Training",
                               "Invertivore_Juvenile_PO_Train.csv"), append = F)
write_csv(invert_sub_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training",
                               "Invertivore_Subadult_PO_Train.csv"), append = F)
write_csv(invert_adt_PO_train, here("Final_Data","Species_Occurrence","Adult","Training",
                               "Invertivore_Adult_PO_Train.csv"), append = F)
write_csv(invert_juv_PO_test, here("Final_Data","Species_Occurrence","Juvenile","Testing",
                              "Invertivore_Juvenile_PO_Test.csv"), append = F)
write_csv(invert_sub_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing",
                              "Invertivore_Subadult_PO_Test.csv"), append = F)
write_csv(invert_adt_PO_test, here("Final_Data","Species_Occurrence","Adult","Testing",
                              "Invertivore_Adult_PO_Test.csv"), append = F)

# clean up
rm(coel_adt_PA, coel_adt_PO_test, coel_adt_PO_train, coel_juv_PA, coel_juv_PO_test, 
   coel_juv_PO_train, coel_sub_PA, coel_sub_PO_test, coel_sub_PO_train,
   coer_adt_PA, coer_adt_PO_test, coer_adt_PO_train, coer_juv_PA, coer_juv_PO_test, 
   coer_juv_PO_train, coer_sub_PA, coer_sub_PO_test, coer_sub_PO_train,
   guac_adt_PA, guac_adt_PO_test, guac_adt_PO_train, guac_juv_PA, guac_juv_PO_test, 
   guac_juv_PO_train, guac_sub_PA, guac_sub_PO_test, guac_sub_PO_train,
   herb_adt_PA, herb_adt_PO_test, herb_adt_PO_train, herb_juv_PA, herb_juv_PO_test, 
   herb_juv_PO_train, herb_sub_PA, herb_sub_PO_test, herb_sub_PO_train,
   gris_adt_PA, gris_adt_PO_test, gris_adt_PO_train, gris_juv_PA, gris_juv_PO_test, 
   gris_juv_PO_train, gris_sub_PA, gris_sub_PO_test, gris_sub_PO_train,
   sciu_adt_PA, sciu_adt_PO_test, sciu_adt_PO_train, sciu_juv_PA, sciu_juv_PO_test, 
   sciu_juv_PO_train, sciu_sub_PA, sciu_sub_PO_test, sciu_sub_PO_train,
   invert_adt_PA, invert_adt_PO_test, invert_adt_PO_train, invert_juv_PA, invert_juv_PO_test, 
   invert_juv_PO_train, invert_sub_PA, invert_sub_PO_test, invert_sub_PO_train)



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
domain_grid <- rast(here("Final_Data","Final_Study_Region.tif"))

# clean up
rm(mvs_sites, rvc, rvc_sites, sampling_effort_spdf)
gc()

# calculate kernel density surface
kde <- sp.kde(x = sampling_effort, bw = domain_bw, ref = domain_grid, res = 5,
                     standardize = T)

# save initial bias grid because that step takes a long time
writeRaster(kde, here("Intermediate_Data","initial_bias_grid.tif"), overwrite = T)
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

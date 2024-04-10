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
install.packages("truncnorm")
install.packages("reshape2")

# LOAD PACKAGES
library(easypackages)
libraries("here", "devtools", "tidyverse", "conflicted", "truncnorm", "reshape2")

# prevent conflicts between packages
conflicted::conflict_prefer("filter", "dplyr")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/MSc_Ch1_DataPrep/R/Occurence_DataPrep.R")

# READ IN SPECIES OCCURRENCE DATASETS
# pre-cleaned rvc and mvs data for FLA_KEYS 2014, 2016, 2018, 2022
rvc <- as_tibble(read.csv(here("GitHub_Repositories","MSc_Ch1_DataPrep","Data_SmallFiles","Fish","RVC_Start.csv")))
mvs <- as_tibble(read.csv(here("GitHub_Repositories","MSc_Ch1_DataPrep","Data_SmallFiles","Fish","MVS_Start.csv")))

# NOTE: mvs dataset is already presence ONLY



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



# Combine RVC and MVS -----------------------------------------------------


# convert fork length (rvc) to total length using values from FishBase
# TL = a + b*FL

rvc_focal["TOT_LEN"] <- NA

for (i in 1:nrow(rvc_focal)) {
  
  if (rvc_focal[i, "SPECIES_CODE"] == "SCA_COER"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.049 ### FIND PROPER VALUES
  }
  if (rvc_focal[i, "SPECIES_CODE"] == "SCA_COEL"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.049
  }
  if (rvc_focal[i, "SPECIES_CODE"] == "SCA_GUAC"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.049
  }
  if (rvc_focal[i, "SPECIES_CODE"] == "LUT_GRIS"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.049
  }
  if (rvc_focal[i, "SPECIES_CODE"] == "HAE_SCIU"){
    rvc_focal$TOT_LEN[i] = rvc_focal$LEN[i] * 1.049
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
# (between size at 1 YR and size at maturation) # L_min ? L_50 ? L_100 ? 

# Gray snapper
all_subadults <- all_focal %>% filter(SPECIES_CODE == "LUT_GRIS") %>% 
  filter(.$TOT_LEN >= 9.0 & .$TOT_LEN <= 25.0)

# Blue-striped grunt
temp <- all_focal %>% filter(SPECIES_CODE == "HAE_SCIU") %>% 
  filter(.$TOT_LEN >= 11.0 & .$TOT_LEN <= 26.0)
all_subadults <- rbind(all_subadults, temp)

# Blue parrotfish
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COER") %>% 
  filter(.$TOT_LEN >= 11.0 & .$TOT_LEN <= 39.0)
all_subadults <- rbind(all_subadults, temp)

# Midnight parrotfish
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COEL") %>% 
  filter(.$TOT_LEN >= 11.0 & .$TOT_LEN <= 39.0)
all_subadults <- rbind(all_subadults, temp)

# Blue or Midnight parrotfish (identification not positive)
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_COES") %>% 
  filter(.$TOT_LEN >= 11.0 & .$TOT_LEN <= 39.0)
all_subadults <- rbind(all_subadults, temp)

# Rainbow parrotfish
temp <- all_focal %>% filter(SPECIES_CODE == "SCA_GUAC") %>% 
  filter(.$TOT_LEN >= 11.0 & .$TOT_LEN <= 39.0)
all_subadults <- rbind(all_subadults, temp)



# Perform Long to Wide Conversion -----------------------------------------


# long to wide conversion, keep species separate when we sum by ID_SURV

# -> long to wide
# -> split COES density between COEL and COER
# -> calculate total numbers of our focal species
all_sub_w <- all_subadults %>% dcast(., SOURCE+ID_SURV+ID_SITE+x+y ~ SPECIES_CODE, 
                               value.var = "NUM", fun.aggregate = sum) %>%
  mutate(SCA_COEL = SCA_COEL + (SCA_COES / 2), SCA_COER = SCA_COER + (SCA_COES / 2)) %>% 
  select(-SCA_COES) %>% 
  mutate(INVERT = HAE_SCIU + LUT_GRIS) %>% 
  mutate(HERB = SCA_COEL + SCA_COER + SCA_GUAC)



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

##################


# Write out Datasets ------------------------------------------------------

write.csv(density_scarus, paste0(species_wd, "Density_Scarus.csv"))
write.csv(density_juvenile, paste0(species_wd, "Density_Juvenile.csv"))
write.csv(density_subadult, paste0(species_wd, "Density_Sub-adult.csv"))
write.csv(density_adult, paste0(species_wd, "Density_Adult.csv"))

write.csv(pa_scarus, paste0(species_wd, "PA_Scarus.csv"))
write.csv(pa_juvenile, paste0(species_wd, "PA_Juvenile.csv"))
write.csv(pa_subadult, paste0(species_wd, "PA_Sub-adult.csv"))
write.csv(pa_adult, paste0(species_wd, "PA_Adult.csv"))

write.csv(pres_scarus, paste0(species_wd, "Presence_Scarus.csv"))
write.csv(pres_juvenile, paste0(species_wd, "Presence_Juvenile.csv"))
write.csv(pres_subadult, paste0(species_wd, "Presence_Sub-adult.csv"))
write.csv(pres_adult, paste0(species_wd, "Presence_Adult.csv"))

write.csv(pres_scarus_coer, paste0(species_wd, "Presence_Scarus_COER.csv"))
write.csv(pres_juvenile_coer, paste0(species_wd, "Presence_Juvenile_COER.csv"))
write.csv(pres_subadult_coer, paste0(species_wd, "Presence_Sub-adult_COER.csv"))
write.csv(pres_adult_coer, paste0(species_wd, "Presence_Adult_COER.csv"))

write.csv(pres_scarus_coel, paste0(species_wd, "Presence_Scarus_COEL.csv"))
write.csv(pres_juvenile_coel, paste0(species_wd, "Presence_Juvenile_COEL.csv"))
write.csv(pres_subadult_coel, paste0(species_wd, "Presence_Sub-adult_COEL.csv"))
write.csv(pres_adult_coel, paste0(species_wd, "Presence_Adult_COEL.csv"))

write.csv(pres_scarus_guac, paste0(species_wd, "Presence_Scarus_GUAC.csv"))
write.csv(pres_juvenile_guac, paste0(species_wd, "Presence_Juvenile_GUAC.csv"))
write.csv(pres_subadult_guac, paste0(species_wd, "Presence_Sub-adult_GUAC.csv"))
write.csv(pres_adult_guac, paste0(species_wd, "Presence_Adult_GUAC.csv")) 

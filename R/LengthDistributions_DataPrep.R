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

### PREPARING SPECIES DATASETS FOR HABITAT SUITABILITY MODELLING ###

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
libraries("here", "devtools", "tidyverse", "conflicted", "truncnorm", 
          "sp", "sf", "terra", "spatialEco", "data.table")

# prevent conflicts between packages
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("dcast", "data.table")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/Subadult_Occurrence_DataPrep.R")

# save PROJ.4 string for NEW and OLD standard projection 
# EPSG:6346 NAD 1983 2011 UTM Zone 17N
new_crs <- crs("+init=epsg:6346")

# READ IN SPECIES OCCURRENCE DATASETS
# pre-cleaned rvc and mvs data for FLA_KEYS 2014, 2016, 2018, 2022
rvc <- as.data.table(read.csv(here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","Fish","RVC_Start.csv")))
mvs <- as.data.table(read.csv(here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","Fish","MVS_Start.csv")))

# NOTE: mvs dataset is already presence ONLY

# find number of unique sites for generating sampling bias grid later
rvc_sites <- rvc %>% distinct(ID_SURV, x, y)
mvs_sites <- mvs %>% distinct(ID_SURV, x, y)



# Expand MVS Presence - Simulate Individual Fish Lengths ---------------------


# filter for our focal species
mvs_focal <- mvs %>% filter(SPECIES_CODE %in% c("SCA_COER","SCA_COEL","SCA_COES",
                                                "SCA_GUAC","LUT_GRIS","HAE_SCIU"))

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

# check out the latest length samples and clean up
hist(samples)
rm(i, p, samples, stdev, temp, mvs_focal)



# Blue Parrotfish ---------------------------------------------------------


### RVC PREP FIRST

# convert fork length to total length
bp_rvc <- rvc %>% filter(SPECIES_CODE == "SCA_COER") %>% mutate(N = NUM, TOT_LEN = LEN * 1.05) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN)

### MVS PREP NEXT

# presence sites for blue parrotfish
bp_mvs_p <- mvs_expanded %>% filter(SPECIES_CODE == "SCA_COER") %>% mutate(N = NO, TOT_LEN = LEN) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN) %>% 
  filter(N != 0)

# absence sites for blue parrotfish
bp_mvs_a <- mvs %>% filter(!ID_SURV %in% bp_mvs_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "ALL", SPECIES_CODE = "SCA_COER", N = 0.0, TOT_LEN = 0.0)

# combine presence and absence mvs sites for blue parrotfish
bp_mvs <- rbind(bp_mvs_p, bp_mvs_a)

# combine rvc and mvs
bp_PA <- rbind(bp_rvc, bp_mvs)


### WRITE OUT DATASET

# PA long form - with length values for each fish
write_csv(bp_PA, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","Fish","BlueParrotfish_PA_Full_Long.csv"))



# Midnight Parrotfish ---------------------------------------------------------


### RVC PREP FIRST

# convert fork length to total length
mp_rvc <- rvc %>% filter(SPECIES_CODE == "SCA_COEL") %>% mutate(N = NUM, TOT_LEN = LEN * 1.05) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN)

### MVS PREP NEXT

# presence sites for midnight parrotfish
mp_mvs_p <- mvs_expanded %>% filter(SPECIES_CODE == "SCA_COEL") %>% mutate(N = NO, TOT_LEN = LEN) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN) %>% 
  filter(N != 0)

# absence sites for midnight parrotfish
mp_mvs_a <- mvs %>% filter(!ID_SURV %in% mp_mvs_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "ALL", SPECIES_CODE = "SCA_COEL", N = 0.0, TOT_LEN = 0.0)

# combine presence and absence mvs sites for midnight parrotfish
mp_mvs <- rbind(mp_mvs_p, mp_mvs_a)

# combine rvc and mvs
mp_PA <- rbind(mp_rvc, mp_mvs)


### WRITE OUT DATASET

# PA long form - with length values for each fish
write_csv(mp_PA, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","Fish","MidnightParrotfish_PA_Full_Long.csv"))



# Rainbow Parrotfish ---------------------------------------------------------


### RVC PREP FIRST

# convert fork length to total length
rp_rvc <- rvc %>% filter(SPECIES_CODE == "SCA_GUAC") %>% mutate(N = NUM, TOT_LEN = LEN * 1.05) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN)

### MVS PREP NEXT

# presence sites for rainbow parrotfish
rp_mvs_p <- mvs_expanded %>% filter(SPECIES_CODE == "SCA_GUAC") %>% mutate(N = NO, TOT_LEN = LEN) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN) %>% 
  filter(N != 0)

# absence sites for rainbow parrotfish
rp_mvs_a <- mvs %>% filter(!ID_SURV %in% rp_mvs_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "ALL", SPECIES_CODE = "SCA_GUAC", N = 0.0, TOT_LEN = 0.0)

# combine presence and absence mvs sites for rainbow parrotfish
rp_mvs <- rbind(rp_mvs_p, rp_mvs_a)

# combine rvc and mvs
rp_PA <- rbind(rp_rvc, rp_mvs)


### WRITE OUT DATASET

# PA long form - with length values for each fish
write_csv(rp_PA, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","Fish","RainbowParrotfish_PA_Full_Long.csv"))



# Gray Snapper ---------------------------------------------------------


### RVC PREP FIRST

# convert fork length to total length
gs_rvc <- rvc %>% filter(SPECIES_CODE == "LUT_GRIS") %>% mutate(N = NUM, TOT_LEN = LEN * 1.05) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN)

### MVS PREP NEXT

# presence sites for gray snapper
gs_mvs_p <- mvs_expanded %>% filter(SPECIES_CODE == "LUT_GRIS") %>% mutate(N = NO, TOT_LEN = LEN) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN) %>% 
  filter(N != 0)

# absence sites for gray snapper
gs_mvs_a <- mvs %>% filter(!ID_SURV %in% gs_mvs_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "ALL", SPECIES_CODE = "LUT_GRIS", N = 0.0, TOT_LEN = 0.0)

# combine presence and absence mvs sites for gray snapper
gs_mvs <- rbind(gs_mvs_p, gs_mvs_a)

# combine rvc and mvs
gs_PA <- rbind(gs_rvc, gs_mvs)


### WRITE OUT DATASET

# PA long form - with length values for each fish
write_csv(gs_PA, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","Fish","GraySnapper_PA_Full_Long.csv"))



# Blue striped Grunt ---------------------------------------------------------


### RVC PREP FIRST

# convert fork length to total length
bg_rvc <- rvc %>% filter(SPECIES_CODE == "HAE_SCIU") %>% mutate(N = NUM, TOT_LEN = LEN * 1.05) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN)

### MVS PREP NEXT

# presence sites for bluestriped grunt
bg_mvs_p <- mvs_expanded %>% filter(SPECIES_CODE == "HAE_SCIU") %>% mutate(N = NO, TOT_LEN = LEN) %>% 
  mutate(LIFE_STAGE = "ALL") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N, TOT_LEN) %>% 
  filter(N != 0)

# absence sites for bluestriped grunt
bg_mvs_a <- mvs %>% filter(!ID_SURV %in% bg_mvs_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "ALL", SPECIES_CODE = "HAE_SCIU", N = 0.0, TOT_LEN = 0.0)

# combine presence and absence mvs sites for bluestriped grunt
bg_mvs <- rbind(bg_mvs_p, bg_mvs_a)

# combine rvc and mvs
bg_PA <- rbind(bg_rvc, bg_mvs)


### WRITE OUT DATASET

# PA long form - with length values for each fish
write_csv(bg_PA, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","Fish","BluestripedGrunt_PA_Full_Long.csv"))


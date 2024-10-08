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
                                                "SCA_GUAC","LUT_GRIS","HAE_SCIU")) %>% 
  mutate(AVE_LEN = ifelse(is.na(AVE_LEN)==TRUE, MIN_LEN+1, AVE_LEN)) %>% 
  mutate(MAX_LEN = ifelse(is.na(MAX_LEN)==TRUE, AVE_LEN+1, MAX_LEN)) %>% 
  mutate(AVE_LEN = ifelse(NO > 3 & MIN_LEN == AVE_LEN, MIN_LEN+1, AVE_LEN)) %>% 
  mutate(MAX_LEN = ifelse(NO > 3 & AVE_LEN == MAX_LEN, AVE_LEN+1, MAX_LEN))

# Note: above steps are because the following code can't handle when MIN == AVE == MAX or when there are NAs

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
# filter for subadults (between size at 1 YR and size at maturation)
bp_rvc <- rvc %>% filter(SPECIES_CODE == "SCA_COER") %>% mutate(TOT_LEN = LEN * 1.05) 
bp_rvc_sub <- bp_rvc %>% mutate(N = ifelse(TOT_LEN < 23.59 | TOT_LEN > 35.3, 0, NUM)) %>% 
  mutate(LIFE_STAGE = "SUBADULT") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N)

### MVS PREP NEXT

# presence sites for subadult blue parrotfish
bp_mvs_sub_p <- mvs_expanded %>% filter(SPECIES_CODE == "SCA_COER") %>%  
  mutate(N = ifelse(LEN < 23.59 | LEN > 35.3, 0, NO)) %>% mutate(LIFE_STAGE = "SUBADULT") %>% 
  select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N) %>% filter(N != 0)

# absence sites for subadult blue parrotfish
bp_mvs_sub_a <- mvs %>% filter(!ID_SURV %in% bp_mvs_sub_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "SUBADULT", SPECIES_CODE = "SCA_COER", N = 0.0)

# combine presence and absence mvs sites for blue parrotfish
bp_mvs_sub <- rbind(bp_mvs_sub_p, bp_mvs_sub_a)

### COMBINE RVC and MVS

# rbind and perform long to wide conversion
bp_PA <- rbind(bp_rvc_sub, bp_mvs_sub)
bp_PA_w <- bp_PA %>% dcast(., SOURCE+x+y+LIFE_STAGE ~ SPECIES_CODE, 
                                 value.var = "N", fun.aggregate = sum)

# clean up
rm(bp_rvc, bp_rvc_sub, bp_mvs_sub_p, bp_mvs_sub_a, bp_mvs_sub, bp_PA)

### FINAL FULL, TRAINING, AND TESTING DATASETS

# Presence/Absence and Presence-Only full
bp_PA_full <- bp_PA_w %>% mutate(SPECIES_CODE = "SCA_COER", PRES = ifelse(SCA_COER > 0.0,1,0)) %>% 
  mutate(PRES2 = ifelse(PRES == 1,"PRESENCE","ABSENCE")) %>% 
  mutate(species = SPECIES_CODE, longitude = x, latitude = y) %>% 
  select(LIFE_STAGE, species, SOURCE, longitude, latitude, PRES, PRES2)

bp_PO_full <- bp_PA_full %>% filter(PRES == 1) %>% select(species, longitude, latitude)

# PA and PO training sets

library("ISLR")
# set seed to ensure replicability
set.seed(123)  

bp_train_index <- sample(seq_len(nrow(bp_PA_full)), size = ceiling(0.70*nrow(bp_PA_full))) 
bp_PA_train <- bp_PA_full[bp_train_index,]%>% select(species, longitude, latitude, PRES, PRES2)
bp_PO_train <- bp_PA_train %>% filter(PRES == 1) %>% select(species, longitude, latitude) 
bp_PA_test <- bp_PA_full[-bp_train_index,] %>% select(species, longitude, latitude, PRES, PRES2)
bp_PO_test <- bp_PA_test %>% filter(PRES == 1) %>% select(species, longitude, latitude)

### WRITE OUT DATASETS

# PA and PO full
write_csv(bp_PA_full, here("Final_Data","Species_Occurrence","Subadult","Subadult_BlueParrotfish_PA_Full.csv"), 
          append = F)
write_csv(bp_PO_full, here("Final_Data","Species_Occurrence","Subadult","Subadult_BlueParrotfish_PO_Full.csv"), 
          append = F)
# PA and PO training
write_csv(bp_PA_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_BlueParrotfish_PA_Train.csv"), 
          append = F)
write_csv(bp_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_BlueParrotfish_PO_Train.csv"), 
          append = F)
# PA and PO testing
write_csv(bp_PA_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_BlueParrotfish_PA_Test.csv"), 
          append = F)
write_csv(bp_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_BlueParrotfish_PO_Test.csv"), 
          append = F)

# clean up
rm(bp_PA_full, bp_PA_test, bp_PA_train, bp_PO_full, bp_PO_test, bp_PO_train)


# Midnight Parrotfish ---------------------------------------------------------


### RVC PREP FIRST

# convert fork length to total length
# filter for subadults (between size at 1 YR and size at maturation)
mp_rvc <- rvc %>% filter(SPECIES_CODE == "SCA_COEL") %>% mutate(TOT_LEN = LEN * 1.05) 
mp_rvc_sub <- mp_rvc %>% mutate(N = ifelse(TOT_LEN < 24.43 | TOT_LEN > 42.5, 0, NUM)) %>% 
  mutate(LIFE_STAGE = "SUBADULT") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N)

### MVS PREP NEXT

# presence sites for subadult midnight parrotfish
mp_mvs_sub_p <- mvs_expanded %>% filter(SPECIES_CODE == "SCA_COEL") %>% 
  mutate(N = ifelse(LEN < 24.43 | LEN > 42.5, 0, NO)) %>% mutate(LIFE_STAGE = "SUBADULT") %>% 
  select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N) %>% filter(N != 0)

# absence sites for subadult midnight parrotfish
mp_mvs_sub_a <- mvs %>% filter(!ID_SURV %in% mp_mvs_sub_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "SUBADULT", SPECIES_CODE = "SCA_COEL", N = 0.0)

# combine presence and absence mvs sites for midnight parrotfish
mp_mvs_sub <- rbind(mp_mvs_sub_p, mp_mvs_sub_a)

### COMBINE RVC and MVS

# rbind and perform long to wide conversion
mp_PA <- rbind(mp_rvc_sub, mp_mvs_sub)
mp_PA_w <- mp_PA %>% dcast(., SOURCE+x+y+LIFE_STAGE ~ SPECIES_CODE, 
                           value.var = "N", fun.aggregate = sum)

# clean up
rm(mp_rvc, mp_rvc_sub, mp_mvs_sub_p, mp_mvs_sub_a, mp_mvs_sub, mp_PA)

### FINAL FULL, TRAINING, AND TESTING DATASETS

# Presence/Absence and Presence-Only full
mp_PA_full <- mp_PA_w %>% mutate(SPECIES_CODE = "SCA_COEL", PRES = ifelse(SCA_COEL > 0.0,1,0)) %>% 
  mutate(PRES2 = ifelse(PRES == 1,"PRESENCE","ABSENCE")) %>% 
  mutate(species = SPECIES_CODE, longitude = x, latitude = y) %>% 
  select(LIFE_STAGE, species, SOURCE, longitude, latitude, PRES, PRES2)

mp_PO_full <- mp_PA_full %>% filter(PRES == 1) %>% select(species, longitude, latitude)

# PA and PO training sets

# set seed to ensure replicability
set.seed(123)  

mp_train_index <- sample(seq_len(nrow(mp_PA_full)), size = ceiling(0.70*nrow(mp_PA_full))) 
mp_PA_train <- mp_PA_full[mp_train_index,]%>% select(species, longitude, latitude, PRES, PRES2)
mp_PO_train <- mp_PA_train %>% filter(PRES == 1) %>% select(species, longitude, latitude) 
mp_PA_test <- mp_PA_full[-mp_train_index,] %>% select(species, longitude, latitude, PRES, PRES2)
mp_PO_test <- mp_PA_test %>% filter(PRES == 1) %>% select(species, longitude, latitude)

### WRITE OUT DATASETS

# PA and PO full
write_csv(mp_PA_full, here("Final_Data","Species_Occurrence","Subadult","Subadult_MidnightParrotfish_PA_Full.csv"), 
          append = F)
write_csv(mp_PO_full[,1:3], here("Final_Data","Species_Occurrence","Subadult","Subadult_MidnightParrotfish_PO_Full.csv"), 
          append = F)
# PA and PO training
write_csv(mp_PA_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_MidnightParrotfish_PA_Train.csv"), 
          append = F)
write_csv(mp_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_MidnightParrotfish_PO_Train.csv"), 
          append = F)
# PA and PO testing
write_csv(mp_PA_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_MidnightParrotfish_PA_Test.csv"), 
          append = F)
write_csv(mp_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_MidnightParrotfish_PO_Test.csv"), 
          append = F)

# clean up
rm(mp_PA_full, mp_PA_test, mp_PA_train, mp_PO_full, mp_PO_test, mp_PO_train)


# Rainbow Parrotfish ---------------------------------------------------------


### RVC PREP FIRST

# convert fork length to total length
# filter for subadults (between size at 1 YR and size at maturation)
rp_rvc <- rvc %>% filter(SPECIES_CODE == "SCA_GUAC") %>% mutate(TOT_LEN = LEN * 1.05) 
rp_rvc_sub <- rp_rvc %>% mutate(N = ifelse(TOT_LEN < 27.31 | TOT_LEN > 42.7, 0, NUM)) %>% 
  mutate(LIFE_STAGE = "SUBADULT") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N)

### MVS PREP NEXT

# presence sites for subadult rainbow parrotfish
rp_mvs_sub_p <- mvs_expanded %>% filter(SPECIES_CODE == "SCA_GUAC") %>% 
  mutate(N = ifelse(LEN < 27.31 | LEN > 42.7, 0, NO)) %>% mutate(LIFE_STAGE = "SUBADULT") %>% 
  select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N) %>% filter(N != 0)

# absence sites for subadult rainbow parrotfish
rp_mvs_sub_a <- mvs %>% filter(!ID_SURV %in% rp_mvs_sub_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "SUBADULT", SPECIES_CODE = "SCA_GUAC", N = 0.0)

# combine presence and absence mvs sites for rainbow parrotfish
rp_mvs_sub <- rbind(rp_mvs_sub_p, rp_mvs_sub_a)

### COMBINE RVC and MVS

# rbind and perform long to wide conversion
rp_PA <- rbind(rp_rvc_sub, rp_mvs_sub)
rp_PA_w <- rp_PA %>% dcast(., SOURCE+x+y+LIFE_STAGE ~ SPECIES_CODE, 
                           value.var = "N", fun.aggregate = sum)

# clean up
rm(rp_rvc, rp_rvc_sub, rp_mvs_sub_p, rp_mvs_sub_a, rp_mvs_sub, rp_PA)

### FINAL FULL, TRAINING, AND TESTING DATASETS

# Presence/Absence and Presence-Only full
rp_PA_full <- rp_PA_w %>% mutate(SPECIES_CODE = "SCA_GUAC", PRES = ifelse(SCA_GUAC > 0.0,1,0)) %>% 
  mutate(PRES2 = ifelse(PRES == 1,"PRESENCE","ABSENCE")) %>% 
  mutate(species = SPECIES_CODE, longitude = x, latitude = y) %>% 
  select(LIFE_STAGE, species, SOURCE, longitude, latitude, PRES, PRES2)

rp_PO_full <- rp_PA_full %>% filter(PRES == 1) %>% select(species, longitude, latitude)

# PA and PO training sets

# set seed to ensure replicability
set.seed(123)  

rp_train_index <- sample(seq_len(nrow(rp_PA_full)), size = ceiling(0.70*nrow(rp_PA_full))) 
rp_PA_train <- rp_PA_full[rp_train_index,]%>% select(species, longitude, latitude, PRES, PRES2)
rp_PO_train <- rp_PA_train %>% filter(PRES == 1) %>% select(species, longitude, latitude) 
rp_PA_test <- rp_PA_full[-rp_train_index,] %>% select(species, longitude, latitude, PRES, PRES2)
rp_PO_test <- rp_PA_test %>% filter(PRES == 1) %>% select(species, longitude, latitude)

### WRITE OUT DATASETS

# PA and PO full
write_csv(rp_PA_full, here("Final_Data","Species_Occurrence","Subadult","Subadult_RainbowParrotfish_PA_Full.csv"), 
          append = F)
write_csv(rp_PO_full, here("Final_Data","Species_Occurrence","Subadult","Subadult_RainbowParrotfish_PO_Full.csv"), 
          append = F)
# PA and PO training
write_csv(rp_PA_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_RainbowParrotfish_PA_Train.csv"), 
          append = F)
write_csv(rp_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_RainbowParrotfish_PO_Train.csv"), 
          append = F)
# PA and PO testing
write_csv(rp_PA_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_RainbowParrotfish_PA_Test.csv"), 
          append = F)
write_csv(rp_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_RainbowParrotfish_PO_Test.csv"), 
          append = F)

# clean up
rm(rp_PA_full, rp_PA_test, rp_PA_train, rp_PO_full, rp_PO_test, rp_PO_train)


# Gray Snapper ---------------------------------------------------------


### RVC PREP FIRST

# convert fork length to total length
# filter for subadults (between size at 1 YR and size at maturation)
gs_rvc <- rvc %>% filter(SPECIES_CODE == "LUT_GRIS") %>% mutate(TOT_LEN = LEN * 1.049) 
gs_rvc_sub <- gs_rvc %>% mutate(N = ifelse(TOT_LEN < 9.51 | TOT_LEN > 32.1, 0, NUM)) %>% 
  mutate(LIFE_STAGE = "SUBADULT") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N)

### MVS PREP NEXT

# presence sites for subadult gray snapper
gs_mvs_sub_p <- mvs_expanded %>% filter(SPECIES_CODE == "LUT_GRIS") %>% 
  mutate(N = ifelse(LEN < 9.51 | LEN > 32.1, 0, NO)) %>% mutate(LIFE_STAGE = "SUBADULT") %>% 
  select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N) %>% filter(N != 0)

# absence sites for subadult gray snapper
gs_mvs_sub_a <- mvs %>% filter(!ID_SURV %in% gs_mvs_sub_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "SUBADULT", SPECIES_CODE = "LUT_GRIS", N = 0.0)

# combine presence and absence mvs sites for gray snapper
gs_mvs_sub <- rbind(gs_mvs_sub_p, gs_mvs_sub_a)

### COMBINE RVC and MVS

# rbind and perform long to wide conversion
gs_PA <- rbind(gs_rvc_sub, gs_mvs_sub)
gs_PA_w <- gs_PA %>% dcast(., SOURCE+x+y+LIFE_STAGE ~ SPECIES_CODE, 
                           value.var = "N", fun.aggregate = sum)

# clean up
rm(gs_rvc, gs_rvc_sub, gs_mvs_sub_p, gs_mvs_sub_a, gs_mvs_sub, gs_PA)

### FINAL FULL, TRAINING, AND TESTING DATASETS

# Presence/Absence and Presence-Only full
gs_PA_full <- gs_PA_w %>% mutate(SPECIES_CODE = "LUT_GRIS", PRES = ifelse(LUT_GRIS > 0.0,1,0)) %>% 
  mutate(PRES2 = ifelse(PRES == 1,"PRESENCE","ABSENCE")) %>% 
  mutate(species = SPECIES_CODE, longitude = x, latitude = y) %>% 
  select(LIFE_STAGE, species, SOURCE, longitude, latitude, PRES, PRES2)

gs_PO_full <- gs_PA_full %>% filter(PRES == 1) %>% select(species, longitude, latitude)

# PA and PO training sets

# set seed to ensure replicability
set.seed(123)  

gs_train_index <- sample(seq_len(nrow(gs_PA_full)), size = ceiling(0.70*nrow(gs_PA_full))) 
gs_PA_train <- gs_PA_full[gs_train_index,]%>% select(species, longitude, latitude, PRES, PRES2)
gs_PO_train <- gs_PA_train %>% filter(PRES == 1) %>% select(species, longitude, latitude) 
gs_PA_test <- gs_PA_full[-gs_train_index,] %>% select(species, longitude, latitude, PRES, PRES2)
gs_PO_test <- gs_PA_test %>% filter(PRES == 1) %>% select(species, longitude, latitude)

### WRITE OUT DATASETS

# PA and PO full
write_csv(gs_PA_full, here("Final_Data","Species_Occurrence","Subadult","Subadult_GraySnapper_PA_Full.csv"), 
          append = F)
write_csv(gs_PO_full, here("Final_Data","Species_Occurrence","Subadult","Subadult_GraySnapper_PO_Full.csv"), 
          append = F)
# PA and PO training
write_csv(gs_PA_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_GraySnapper_PA_Train.csv"), 
          append = F)
write_csv(gs_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_GraySnapper_PO_Train.csv"), 
          append = F)
# PA and PO testing
write_csv(gs_PA_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_GraySnapper_PA_Test.csv"), 
          append = F)
write_csv(gs_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_GraySnapper_PO_Test.csv"), 
          append = F)

# clean up
rm(gs_PA_full, gs_PA_test, gs_PA_train, gs_PO_full, gs_PO_test, gs_PO_train)


# Blue striped Grunt ---------------------------------------------------------


### RVC PREP FIRST

# convert fork length to total length
# filter for subadults (between size at 1 YR and size at maturation)
bg_rvc <- rvc %>% filter(SPECIES_CODE == "HAE_SCIU") %>% mutate(TOT_LEN = LEN * 1.034) 
bg_rvc_sub <- bg_rvc %>% mutate(N = ifelse(TOT_LEN < 11.9 | TOT_LEN > 25.33, 0, NUM)) %>% 
  mutate(LIFE_STAGE = "SUBADULT") %>% select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N)

### MVS PREP NEXT

# presence sites for subadult blue striped grunt
bg_mvs_sub_p <- mvs_expanded %>% filter(SPECIES_CODE == "HAE_SCIU") %>% 
  mutate(N = ifelse(LEN < 11.9 | LEN > 25.33, 0, NO)) %>% mutate(LIFE_STAGE = "SUBADULT") %>% 
  select(SOURCE, ID_SURV, x, y, LIFE_STAGE, SPECIES_CODE, N) %>% filter(N != 0)

# absence sites for subadult blue striped grunt
bg_mvs_sub_a <- mvs %>% filter(!ID_SURV %in% bg_mvs_sub_p$ID_SURV) %>% select(SOURCE, ID_SURV, x, y) %>% 
  mutate(LIFE_STAGE = "SUBADULT", SPECIES_CODE = "HAE_SCIU", N = 0.0)

# combine presence and absence mvs sites for blue striped grunt
bg_mvs_sub <- rbind(bg_mvs_sub_p, bg_mvs_sub_a)

### COMBINE RVC and MVS

# rbind and perform long to wide conversion
bg_PA <- rbind(bg_rvc_sub, bg_mvs_sub)
bg_PA_w <- bg_PA %>% dcast(., SOURCE+x+y+LIFE_STAGE ~ SPECIES_CODE, 
                           value.var = "N", fun.aggregate = sum)

# clean up
rm(bg_rvc, bg_rvc_sub, bg_mvs_sub_p, bg_mvs_sub_a, bg_mvs_sub, bg_PA)

### FINAL FULL, TRAINING, AND TESTING DATASETS

# Presence/Absence and Presence-Only full
bg_PA_full <- bg_PA_w %>% mutate(SPECIES_CODE = "HAE_SCIU", PRES = ifelse(HAE_SCIU > 0.0,1,0)) %>% 
  mutate(PRES2 = ifelse(PRES == 1,"PRESENCE","ABSENCE")) %>% 
  mutate(species = SPECIES_CODE, longitude = x, latitude = y) %>% 
  select(LIFE_STAGE, species, SOURCE, longitude, latitude, PRES, PRES2)

bg_PO_full <- bg_PA_full %>% filter(PRES == 1) %>% select(species, longitude, latitude)

# PA and PO training sets

# set seed to ensure replicability
set.seed(123)  

bg_train_index <- sample(seq_len(nrow(bg_PA_full)), size = ceiling(0.70*nrow(bg_PA_full))) 
bg_PA_train <- bg_PA_full[bg_train_index,]%>% select(species, longitude, latitude, PRES, PRES2)
bg_PO_train <- bg_PA_train %>% filter(PRES == 1) %>% select(species, longitude, latitude) 
bg_PA_test <- bg_PA_full[-bg_train_index,] %>% select(species, longitude, latitude, PRES, PRES2)
bg_PO_test <- bg_PA_test %>% filter(PRES == 1) %>% select(species, longitude, latitude)

### WRITE OUT DATASETS

# PA and PO full
write_csv(bg_PA_full, here("Final_Data","Species_Occurrence","Subadult","Subadult_BluestripedGrunt_PA_Full.csv"), 
          append = F)
write_csv(bg_PO_full, here("Final_Data","Species_Occurrence","Subadult","Subadult_BluestripedGrunt_PO_Full.csv"), 
          append = F)
# PA and PO training
write_csv(bg_PA_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_BluestripedGrunt_PA_Train.csv"), 
          append = F)
write_csv(bg_PO_train, here("Final_Data","Species_Occurrence","Subadult","Training","Subadult_BluestripedGrunt_PO_Train.csv"), 
          append = F)
# PA and PO testing
write_csv(bg_PA_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_BluestripedGrunt_PA_Test.csv"), 
          append = F)
write_csv(bg_PO_test, here("Final_Data","Species_Occurrence","Subadult","Testing","Subadult_BluestripedGrunt_PO_Test.csv"), 
          append = F)

# clean up
rm(bg_PA_full, bg_PA_test, bg_PA_train, bg_PO_full, bg_PO_test, bg_PO_train)



# Bias Grid ---------------------------------------------------------------


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
terra::writeRaster(domain_kde, here("Final_Data","Final_Sampling_Bias.tif"), overwrite = T)




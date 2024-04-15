### WELCOME ###

# This is script __ of __ in Isla's data analysis pipeline.

### PREPARING RVC AND MVS DATASETS FOR HSM ###

# This script is used to clean and prepare the species occurrence datasets for Isla Turcke's first 
# MSc chapter in the lab of Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida and were subsequently used for habitat
# suitability modeling using maximum entropy models.

### TO USE THIS FILE ###
# Before running this R script:
# - Obtain a copy of the Mangrove Visual Survey dataset 
#   and save it in the "MVS" folder in the "Source_Data" folder.

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)



# Set Up and Organization -----------------------------------------------------


# LOAD PACKAGES
library(easypackages)
libraries("here", "devtools", "tidyverse", "conflicted", "readxl", "terra", "sf")

# prevent conflicts between packages
conflicted::conflict_prefer("filter", "dplyr")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/MSc_Ch1_DataPrep/R/RVC-MVS_InitialPrep.R")

# DEFINE COORDINATE SYSTEMS 
# project CRS - EPSG:6346 NAD 1983 (2011) UTM Zone 17N
new_crs <- crs("+init=epsg:6346")
# source data (EPSG:4326 WGS 84/World Geodetic System 1984 (decdeg))
gcs <- crs("+init=epsg:4326")
# previously used CRS - grey snapper and blue-striped grunt
old_crs <- crs("+init=epsg:26958")



# Reading in Source Data for Years of Interest --------------------------------


# rvc -> install and load the South Florida Reef Visual Census package from
# https://github.com/jeremiaheb/rvc
#install.packages('devtools')
devtools::install_github('jeremiaheb/rvc')
library(rvc)

# load rvc data for Florida Keys in years 2014, 2016, 2018. 
# Other data accessible with: getStratumData, getBenthicData, getTaxonomicData
rvc <- as_tibble(getSampleData(years = c(2014, 2016, 2018, 2022), regions = "FLA KEYS"))

# read in mvs data from excel file
mvs <- read_xlsx(path = paste0(here("GitHub_Repositories","MSc_Ch1_DataPrep",
                                    "Data_SmallFiles","Fish","MangroveFishData_1998W-2023W_withMetadata.xlsx")))

# filter to keep years of interest and omit rows with "NO_SURVEY"
mvs <- mvs %>% filter(YR %in% c(2014, 2016, 2018, 2022)) %>% filter(SP != "NO_SURVEY")



# Creating Unique IDs, DATE and Matching Species Codes --------------------------

# edit rvc species codes
rvc$SPECIES_CODE <- sub(" ", "_", rvc$SPECIES_CD)

# output a list of all the unique species in each dataset
mvs_species <- as_tibble(unique(mvs$SP))
write.csv(mvs_species, here("GitHub_Repositories","MSc_Ch1_DataPrep","Data_SmallFiles","Fish","MVS_SpeciesList.csv"))
rvc_species <- as_tibble(unique(rvc$SPECIES_CODE))
write.csv(rvc_species, here("GitHub_Repositories","MSc_Ch1_DataPrep","Data_SmallFiles","Fish","RVC_SpeciesList.csv"))

# read in mvs to rvc species code dictionary
code_dict <- read_xlsx(here("GitHub_Repositories","MSc_Ch1_DataPrep","Data_SmallFiles","Fish","MVS-RVC_SpeciesCodes_Dictionary.xlsx"), col_names = T)

# merge in new codes
mvs_newcodes <- merge(mvs, code_dict, by.x = "SP", by.y = "OLD_MVS_CODE", 
               all.x = TRUE, all.y = FALSE)

# adding columns for unique survey ID and unique site ID
rvc_IDs <- rvc %>% 
  unite("ID_SURV", c(PRIMARY_SAMPLE_UNIT,STATION_NR,YEAR), sep = "_", remove = FALSE) %>% 
  unite("ID_SITE", c(PRIMARY_SAMPLE_UNIT,STATION_NR), sep = "_", remove = TRUE) %>% 
  unite("DATE", c(YEAR, MONTH), sep = "-", remove = TRUE)
mvs_IDs <- mvs_newcodes %>% 
  unite("ID_SURV", c(`Ref Site`,Site,YR), sep = "_", remove = FALSE) %>%
  unite("ID_SITE", c(`Ref Site`,Site), sep = "_", remove = TRUE) %>% 
  unite("DATE", c(YR, MO), sep = "-", remove = TRUE)

# keep only necessary columns
rvc_2 <- rvc_IDs %>% select(ID_SURV, ID_SITE, DATE, LON_DEGREES, LAT_DEGREES, 
                            UNDERWATER_VISIBILITY, SPECIES_CODE, NUM, LEN)
mvs_2 <- mvs_IDs %>% select(ID_SURV, ID_SITE, DATE, LON, LAT, SPECIES_CODE, NO, 
                          MIN_IN, AVE_IN, MAX_IN)



# Unit Conversions --------------------------------------------------------

# convert NUM (fish per 176.7 m^2) to DENSITY (fish per 100 m^2)
rvc_3 <- rvc_2 %>% 
  mutate(DENSITY = (100 * NUM)/(3.14 * (if_else(UNDERWATER_VISIBILITY < 7.5, 
                                                UNDERWATER_VISIBILITY, 7.5))^2)) %>%
  select(ID_SURV, ID_SITE, DATE, LON_DEGREES, LAT_DEGREES, SPECIES_CODE, NUM, DENSITY, LEN)

# convert mvs length measurements from inches to cm (multiply by 2.54)
# convert number of fish per transect (60 m^2) to fish per 100 m^2
mvs_3 <- mvs_2 %>% 
  mutate(MIN_LEN = MIN_IN*2.54, AVE_LEN = AVE_IN*2.54, MAX_LEN = MAX_IN*2.54,
         DENSITY = NO / 60 * 100) %>% 
  select(ID_SURV, ID_SITE, DATE, LON, LAT, SPECIES_CODE, NO, DENSITY, MIN_LEN, AVE_LEN, MAX_LEN)

# set NO, MIN_IN, AVE_IN, MAX_IN to 0 when species = NO_FISH
mvs_3$NO <- ifelse(mvs_3$SPECIES_CODE == "NO_FISH", 0.0, mvs_3$NO)
mvs_3$DENSITY <- ifelse(mvs_3$SPECIES_CODE == "NO_FISH", 0.0, mvs_3$DENSITY)
mvs_3$MIN_LEN <- ifelse(mvs_3$SPECIES_CODE == "NO_FISH", 0.0, mvs_3$MIN_LEN)
mvs_3$AVE_LEN <- ifelse(mvs_3$SPECIES_CODE == "NO_FISH", 0.0, mvs_3$AVE_LEN)
mvs_3$MAX_LEN <- ifelse(mvs_3$SPECIES_CODE == "NO_FISH", 0.0, mvs_3$MAX_LEN)



# Clip data sets to Study Region -------------------------------------------


# average lat and lon values, grouped by survey
rvc_4 <- rvc_3 %>% 
  group_by(., ID_SURV) %>% 
  mutate(LON_DEG = mean(LON_DEGREES), LAT_DEG = mean(LAT_DEGREES)) %>%
  select(ID_SURV, ID_SITE, DATE, LON_DEG, LAT_DEG, SPECIES_CODE, NUM, DENSITY, LEN) %>%
  ungroup()

mvs_4 <- mvs_3 %>% 
  group_by(., ID_SURV) %>% 
  mutate(LON_DEG = mean(LON), LAT_DEG = mean(LAT)) %>%
  select(ID_SURV, ID_SITE, DATE, LON_DEG, LAT_DEG, 
         SPECIES_CODE, NO, DENSITY, MIN_LEN, AVE_LEN, MAX_LEN) %>%
  ungroup()

# convert data sets to spatVector point datasets (terra)
rvc_vect <- vect(rvc_4, geom=c("LON_DEG","LAT_DEG"), crs=gcs, keepgeom=F)
mvs_vect <- vect(mvs_4, geom=c("LON_DEG","LAT_DEG"), crs=gcs, keepgeom=F)

# clean up
rm(rvc, rvc_IDs, rvc_2, rvc_3, rvc_4, mvs, mvs_newcodes, mvs_IDs, mvs_2, mvs_3, mvs_4)

# read in our study domain
study_poly <- vect(here("Final_Data","Study_Region.shp"))
#study_rast <- rast(here("Final_Data","Study_Region.tif"))

# project occurrence data to match crs with study region
rvc_vect <- terra::project(rvc_vect, new_crs)
mvs_vect <- terra::project(mvs_vect, new_crs)

# crop data to study region
rvc_mask <- terra::mask(rvc_vect, study_poly)
mvs_mask <- terra::mask(mvs_vect, study_poly)

plot(study_poly)
points(rvc_mask, col = "turquoise3")
points(mvs_mask, col = "purple")

# add source column to say if data is from rvc or mvs
rvc_mask$SOURCE <- "RVC"
mvs_mask$SOURCE <- "MVS"



# Outputting final datasets as .csv ---------------------------------------


# convert from spatVector to data.frame 
rvc_df <- terra::as.data.frame(rvc_mask, geom = "XY")
mvs_df <- terra::as.data.frame(mvs_mask, geom = "XY")

# save as csv to Intermediate_Data folder
write_csv(mvs_df, here("GitHub_Repositories","MSc_Ch1_DataPrep","Data_SmallFiles",
                       "Fish","MVS_Start.csv"), append = F)
write_csv(rvc_df, here("GitHub_Repositories","MSc_Ch1_DataPrep","Data_SmallFiles","Fish",
                       "RVC_Start.csv"), append = F)

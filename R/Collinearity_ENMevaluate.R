### WELCOME ###

# This is script __ of __ in Isla's data prep pipeline.

# This script was used to prepare data for Isla Turcke's first MSc chapter 
# in the lab of Dr. Stephanie Green at the University of Alberta (2022-2024). 
# Data are specific to southern Florida and include bottom water conditions 
# such as temperature, salinity, and dissolved oxygen. These data were used for 
# habitat suitability modeling using maximum entropy.

### CONTACT ###
# Isla Turcke (turcke@ualberta.ca)

### CHECKING FOR COLLINEARITY ###
# This script checks the 17 predictor datasets for (multi) collinearity
# using Pairwise Pearson correlation and Variance Inflation Factor.

### TUNING MAXENT MODEL SETTINGS ###
# This script also uses ENMevaluate() to build models iteratively 
# across a range of user-specified tuning settings.
# These evaluation statistics should help the user identify 
# model settings that balance fit and predictive ability.



# Set Up ------------------------------------------------------------------


# install packages
install.packages("usdm")
install.packages("sdmpredictors")
install.packages("corrplot")
install.packages("Cairo")

# load packages
library(easypackages)
libraries("raster", "terra", "sf", "here", "dplyr", "usdm", "sdmpredictors", "PNWColors",
          "corrplot", "Cairo")


# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/Collinearity_ENMevaluate.R")

# save PROJ.4 string for NEW projection 
# EPSG:6346 NAD 1983 2011 UTM Zone 17N
new_crs <- crs("+init=epsg:6346")



# Load Predictor Rasters --------------------------------------------------

habitat <- raster(here("Final_Data","Predictors_ASCII","Habitat_Type.asc"))
mg_dist <- raster(here("Final_Data","Predictors_ASCII","Mangrove_Distance.asc"))
depth <- raster(here("Final_Data","Predictors_ASCII","Depth.asc"))
slope <- raster(here("Final_Data","Predictors_ASCII","Slope.asc"))
curvature <- raster(here("Final_Data","Predictors_ASCII","Curvature.asc"))
plan_curv <- raster(here("Final_Data","Predictors_ASCII","Planar_Curvature.asc"))
prof_curv <- raster(here("Final_Data","Predictors_ASCII","Profile_Curvature.asc"))
rug_acr <- raster(here("Final_Data","Predictors_ASCII","Rugosity_ACR.asc"))
rug_vrm <- raster(here("Final_Data","Predictors_ASCII","Rugosity_VRM.asc"))
bpi_fine <- raster(here("Final_Data","Predictors_ASCII","BPI_Fine.asc"))
bpi_broad <- raster(here("Final_Data","Predictors_ASCII","BPI_Broad.asc"))
sum_temp <- raster(here("Final_Data","Predictors_ASCII","Summer_Temperature.asc"))
sum_sal <- raster(here("Final_Data","Predictors_ASCII","Summer_Salinity.asc"))
sum_do <- raster(here("Final_Data","Predictors_ASCII","Summer_Dissolved_Oxygen.asc"))
win_temp <- raster(here("Final_Data","Predictors_ASCII","Winter_Temperature.asc"))
win_sal <- raster(here("Final_Data","Predictors_ASCII","Winter_Salinity.asc"))
win_do <- raster(here("Final_Data","Predictors_ASCII","Winter_Dissolved_Oxygen.asc"))

# define crs for each raster
crs(habitat) <- new_crs
crs(mg_dist) <- new_crs
crs(depth) <- new_crs
crs(slope) <- new_crs
crs(curvature) <- new_crs
crs(plan_curv) <- new_crs
crs(prof_curv) <- new_crs
crs(rug_acr) <- new_crs
crs(rug_vrm) <- new_crs
crs(bpi_fine) <- new_crs
crs(bpi_broad) <- new_crs
crs(sum_temp) <- new_crs
crs(sum_sal) <- new_crs
crs(sum_do) <- new_crs
crs(win_temp) <- new_crs
crs(win_sal) <- new_crs
crs(win_do) <- new_crs

# create raster stack
pred_full <- raster::stack(x = list(habitat, mg_dist, depth, slope, curvature, 
                                    plan_curv, prof_curv, rug_acr, rug_vrm, bpi_fine,
                                    bpi_broad, sum_temp, sum_sal, sum_do, win_temp,
                                    win_sal, win_do))
names(pred_full) <- c("Habitat","Mangrove_Dist","Depth","Slope","Curvature","Plan_Curv",
                      "Profile_Curv","ACR_Rugosity","Terrain_Ruggedness","BPI_Fine","BPI_Broad",
                      "Sum_Temp","Sum_Sal","Sum_DO","Win_Temp","Win_Sal","Win_DO")

pred_test <- raster::stack(x = list(habitat, mg_dist, depth, slope))
names(pred_test) <- c("Habitat","Mangrove_Dist","Depth","Slope")


# Pearson Pairwise Correlation --------------------------------------------


# full pearson correlation matrix on all spatial predictors
ppcor_full <- pearson_correlation_matrix(pred_full)

# plot full correlation matrix
palette <- pnw_palette("Shuksan2", 200, type = "continuous")
par(mar = c(0,0,0,0))
corrplot(ppcor_full, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.6, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.7) # order = FPC, hclust

# save plot as png
Cairo(file = here("GitHub_Repositories","Turcke_MSc_Ch1","Figures","Correlation_FullPredictorSet.png"),
      bg = "white", type = "png", units = "in", width = 7, height = 7, 
      pointsize = 12, dpi = 600)
par(mar = c(0,0,0,0))
corrplot(ppcor_full, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.6,       # order = FPC, original
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8) 
dev.off()

# save as .csv
write.csv(ppcor_full, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles",
                           "Correlation_FullPredictorSet.csv"))

# clean up
rm(CRS, new_crs, ppcor_full, pred_full)


# Variance Inflation Factors (VIF) ----------------------------------------

# We can also use VIF to assess multicollinearity.
# VIF measures how much the behavior (variance) of a variable is influenced
# by its interaction with other variables. VIF allows a quick measure of how
# much a variable is contributing to the standard error in the regression. 
# We want to keep standard errors as small as possible, so we will use a 
# standard VIF threshold of 5.

# take random sample
#cl <- snow::makeCluster(10)
x <- raster::sampleRandom(pred_test, 10)
# this returned fewer than 10000 because of the NAs that were removed
# find a way to fix this
#snow::stopCluster(cl)

# calculate vif for random sample
vif <- vif(as.data.frame(x))
vif
write.csv(vif, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles",
                    "VIF_FullPredictorSet.csv"), row.names = FALSE)


# Predictor Selection -----------------------------------------------------

# include only predictors that do not exceed thresholds 
# --> ppcor = |0.7|
# --> VIF = 5

#pred_select <- raster::stack(x = c(...))


# Correlation - Selected Predictors ---------------------------------------

# pearson correlation matrix on selected spatial predictors
cl = snow::makeCluster(n_cores)
ppcor_select <- pearson_correlation_matrix(pred_select)
snow::stopCluster(cl)

# plot correlation matrix 
palette <- pnw_palette("Shuksan2", 200, type = "continuous")
par(mar = c(0,0,0,0))
corrplot(ppcor_select, method = "color", col = palette, type = "upper",
         order = "AOE", addCoef.col = "black", number.cex = 0.5, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8) # order = FPC, hclust

# save plot as png
Cairo(file = here("GitHub_Repositories","Turcke_MSc_Ch1","Figures","Correlation_SelectedPredictorSet.png"),
      bg = "white", type = "png", units = "in", width = 6, height = 5, 
      pointsize = 12, dpi = 600)
par(mar = c(0,0,0,0))
corrplot(ppcor_full, method = "color", col = palette, type = "upper",
         order = "AOE", addCoef.col = "black", number.cex = 0.5, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8) # order = FPC, hclust
dev.off()

# save as .csv
write.csv(ppcor_full, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles",
                           "Correlation_SelectedPredictorSet.csv"))


# VIF - Selected Predictors -----------------------------------------------

# take random sample
cl <- snow::makeCluster(n_cores)
y <- sampleRandom(pred_select, 10000, na.rm = TRUE)
snow::stopCluster(cl)

# calculate vif for random sample
vif_select <- vif(as.data.frame(y))
vif_select
write.csv(vif_select, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles",
                           "VIF_SelectedPredictorSet.csv"), row.names = FALSE)


# ENM Evaluate ------------------------------------------------------------

# Evaluate what the best settings are for MaxEnt

### FIND/INSTALL JAVA PACKAGE
install.packages("rJava", dependencies = T)
install.packages("ENMeval", dependencies = T)
Sys.setenv(JAVA_HOME = ...)

libraries("rJava", "ENMeval")

# need rJava first, find the dismo directory and move the maxent.jar file there (manually)
system.file("java", package = "dismo")

# read in the survey bias file 
# this will guide the selection of background points by MaxEnt
bias <- raster(here("Final_Data","Predictors_ASCII","Sampling_Bias.asc"))
crs(bias) <- new_crs

# study domain is very large, so select 10,000 background points 
# (the base settings for MaxEnt)
bg_pts = as.data.frame(xyFromCell(bias, sample(which(!is.na(values(subset(pred_select, 1)))), 10000,
                                               prob = values(bias)[!is.na(values(subset(pred_select, 1)))])))

# save to intermediate data folder just in case
write.csv(bg_pts, here("Intermediate_Data","Background_Points.csv"), row.names = FALSE)

# run evaluation using 10-fold cross-validation & background points selected based
# on bias file. Remember to specify that variable 1 in the raster stack (habitat) 
# is categorical and use only the presence data !
enm_wd <- here("ENMevaluate")


# Gray Snapper ENMeval ----------------------------------------------------

# read in presence only data with only two columns: longitude and latitude (in that order)
lg_full = read.csv(here("Final_Data","Species_Full","GraySnapper_Full.csv"))[,-1]
lg_train = read.csv(here("Final_Data","Species_Training","GraySnapper_Train.csv"))[,-1]
lg_test = read.csv(here("Final_Data","Species_Testing","GraySnapper_Test.csv"))[,-1]

# run model evaluation on full PO data with random-k-fold partitioning
lg_enm_eval_full = ENMevaluate(lg_full, pred_select, bg = bg_pts, 
                               tune.args = list(fc = c("L", "LQ", "LQH", "LQHP"), 
                                                rm = c(0.25, 0.50, 1.0, 2.0, 5.0)),
                               partitions = "randomkfold", kfolds = 10, algorithm = "maxent.jar",
                               categoricals = 1, 
                               parallel = TRUE, numCores = n_cores)

write.csv(lg_enm_eval_full@results, paste(enm_wd, "Subadult_GraySnapper_ENMeval_Partitioning.csv"))

# run model evaluation on training data with fully withheld testing data
lg_enm_eval_tt = ENMevaluate(lg_train, pred_select, bg = bg_pts, 
                             tune.args = list(fc = c("L", "LQ", "LQH", "LQHP"), 
                                              rm = c(0.25, 0.50, 1.0, 2.0, 5.0)),
                             partitions = "testing", occs.testing = lg_test,
                             algorithm = "maxent.jar", categoricals = 1, 
                             parallel = TRUE, numCores = n_cores)

write.csv(lg_enm_eval_tt@results, paste(enm_wd, "Subadult_GraySnapper_ENMeval_TrainTest.csv"))


# Bluestriped Grunt ENMeval -----------------------------------------------

# read in presence only data with only two columns: longitude and latitude (in that order)
hs_full = read.csv(here("Final_Data","Species_Full","Subadult_BluestripedGrunt_Full.csv"))[,-1]
hs_train = read.csv(here("Final_Data","Species_Training","Subadult_BluestripedGrunt_Train.csv"))[,-1]
hs_test = read.csv(here("Final_Data","Species_Testing","Subadult_BluestripedGrunt_Test.csv"))[,-1]

# run model evaluation on full PO data with random-k-fold partitioning
hs_enm_eval_full = ENMevaluate(hs_full, pred_select, bg = bg_pts, 
                               tune.args = list(fc = c("L", "LQ", "LQH", "LQHP"), 
                                                rm = c(0.25, 0.50, 1.0, 2.0, 5.0)),
                               partitions = "randomkfold", kfolds = 10, algorithm = "maxent.jar",
                               categoricals = 1, 
                               parallel = TRUE, numCores = n_cores)

write.csv(hs_enm_eval_full@results, paste(enm_wd, "Subadult_BluestripedGrunt_ENMeval_Partitioning.csv"))

# run model evaluation on training data with fully withheld testing data
hs_enm_eval_tt = ENMevaluate(hs_train, pred_select, bg = bg_pts, 
                             tune.args = list(fc = c("L", "LQ", "LQH", "LQHP"), 
                                              rm = c(0.25, 0.50, 1.0, 2.0, 5.0)),
                             partitions = "testing", occs.testing = hs_test,
                             algorithm = "maxent.jar", categoricals = 1, 
                             parallel = TRUE, numCores = n_cores)

write.csv(hs_enm_eval_tt@results, paste(enm_wd, "Subadult_BluestripedGrunt_ENMeval_TrainTest.csv"))





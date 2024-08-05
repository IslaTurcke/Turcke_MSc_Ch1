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



# Set Up ------------------------------------------------------------------


# install packages
# install.packages("usdm")
# install.packages("sdmpredictors")
# install.packages("corrplot")
# install.packages("Cairo")

# load packages
library(easypackages)
libraries("raster", "terra", "sf", "here", "dplyr", "usdm", "sdmpredictors", "PNWColors",
          "corrplot", "Cairo", "data.table")

# set working directory
setwd("Z:/Isla_MSc_Ch1/")

# change where large temporary rasters are saved
rasterOptions(tmpdir = "Z:/Isla_MSc_Ch1/Temp/")
terraOptions(tempdir = "Z:/Isla_MSc_Ch1/Temp/")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/Collinearity_ENMevaluate.R")

# read in final study region raster to use its CRS and EXT
final_region <- terra::rast(here("Final_Data","Final_Study_Region.tif"))
CRS <- crs(final_region)



# Load Predictor Rasters --------------------------------------------------


# load from ASCII files (didn't work for ENMevaluate)
habitat <- raster(here("Final_Data","Predictors_ASCII","Habitat_Type.asc"))
mg_dist <- raster(here("Final_Data","Predictors_ASCII","Mangrove_Distance.asc"))
depth <- raster(here("Final_Data","Predictors_ASCII","Depth.asc"))
slope <- raster(here("Final_Data","Predictors_ASCII","Slope.asc"))
curvature <- raster(here("Final_Data","Predictors_ASCII","Curvature.asc"))
#plan_curv <- raster(here("Final_Data","Predictors_ASCII","Plan_Curvature.asc"))
#prof_curv <- raster(here("Final_Data","Predictors_ASCII","Profile_Curvature.asc"))
rug_acr <- raster(here("Final_Data","Predictors_ASCII","Rugosity_ACR.asc"))
#rug_vrm <- raster(here("Final_Data","Predictors_ASCII","Rugosity_VRM.asc"))
bpi_fine <- raster(here("Final_Data","Predictors_ASCII","BPI_Fine.asc"))
bpi_broad <- raster(here("Final_Data","Predictors_ASCII","BPI_Broad.asc"))
sum_temp <- raster(here("Final_Data","Predictors_ASCII","Summer_Temperature.asc"))
#sum_sal <- raster(here("Final_Data","Predictors_ASCII","Summer_Salinity.asc"))
sum_do <- raster(here("Final_Data","Predictors_ASCII","Summer_Dissolved_Oxygen.asc"))
win_temp <- raster(here("Final_Data","Predictors_ASCII","Winter_Temperature.asc"))
win_sal <- raster(here("Final_Data","Predictors_ASCII","Winter_Salinity.asc"))
win_do <- raster(here("Final_Data","Predictors_ASCII","Winter_Dissolved_Oxygen.asc"))

# load from aligned and NA matched GeoTIFF files
habitat <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Habitat_Aligned_NAmatch.tif"))
mg_dist <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Mg_Dist_Aligned_NAmatch.tif"))
depth <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Depth_Aligned_NAmatch.tif"))
slope <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Slope_Aligned_NAmatch.tif"))
curvature <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Curvature_Aligned_NAmatch.tif"))
#plan_curv <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Plan_Curv_Aligned_NAmatch.tif"))
#prof_curv <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Profile_Curv_Aligned_NAmatch.tif"))
rug_acr <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","ACR_Rugosity_Aligned_NAmatch.tif"))
#rug_vrm <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Terrain_Ruggedness_Aligned_NAmatch.tif"))
bpi_fine <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","BPI_Fine_Aligned_NAmatch.tif"))
bpi_broad <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","BPI_Broad_Aligned_NAmatch.tif"))
sum_temp <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Summer_Temp_Aligned_NAmatch.tif"))
#sum_sal <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Summer_Sal_Aligned_NAmatch.tif"))
sum_do <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Summer_DO_Aligned_NAmatch.tif"))
win_temp <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Winter_Temp_Aligned_NAmatch.tif"))
win_sal <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Winter_Sal_Aligned_NAmatch.tif"))
win_do <- raster(here("Final_Data","Predictors_GeoTIFFs_Aligned_NAmatch","Winter_DO_Aligned_NAmatch.tif"))

# define crs for each raster
crs(habitat) <- CRS
crs(mg_dist) <- CRS
crs(depth) <- CRS
crs(slope) <- CRS
crs(curvature) <- CRS
#crs(plan_curv) <- CRS
#crs(prof_curv) <- CRS
crs(rug_acr) <- CRS
#crs(rug_vrm) <- CRS
crs(bpi_fine) <- CRS
crs(bpi_broad) <- CRS
crs(sum_temp) <- CRS
#crs(sum_sal) <- CRS
crs(sum_do) <- CRS
crs(win_temp) <- CRS
crs(win_sal) <- CRS
crs(win_do) <- CRS

# create raster stack
pred_full <- raster::stack(x = list(habitat, mg_dist, depth, slope, curvature, 
                                    plan_curv, prof_curv, rug_acr, rug_vrm, bpi_broad,
                                    bpi_fine, sum_temp, sum_sal, sum_do, win_temp,
                                    win_sal, win_do))
names(pred_full) <- c("Habitat","Mangrove_Dist","Depth","Slope","Curvature","Plan_Curv",
                      "Profile_Curv","ACR_Rugosity","Terrain_Ruggedness","BPI_Broad","BPI_Fine",
                      "Sum_Temp","Sum_Sal","Sum_DO","Win_Temp","Win_Sal","Win_DO")



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
rm(CRS, new_crs, ppcor_full)



# Variance Inflation Factors (VIF) ----------------------------------------


# We can also use VIF to assess multicollinearity.
# VIF measures how much the behavior (variance) of a variable is influenced
# by its interaction with other variables. VIF allows a quick measure of how
# much a variable is contributing to the standard error in the regression. 
# We want to keep standard errors as small as possible, so we will use a 
# standard VIF threshold of 5.

# take random sample of 10000 points
x <- as.data.table(raster::sampleRandom(pred_full, 10000, na.rm = T))
i <- nrow(x)
while (i < 10000) {
  temp <- raster::sampleRandom(pred_full, 10000-i, na.rm = T)
  x <- rbind(x, temp)
  i <- nrow(x)
}

# calculate vif for random sample
vif <- vif(x)
vif
write.csv(vif, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles",
                    "VIF_FullPredictorSet.csv"), row.names = FALSE)



# Predictor Selection -----------------------------------------------------

# include only predictors that do not exceed thresholds 
# --> ppcor = |0.7|
# --> VIF = 5

# excluding Plan Curvature, Profile Curvature, Terrain Ruggedness, and Summer Salinity

pred_select <- raster::stack(x = list(habitat, mg_dist, depth, slope, curvature, 
                                      rug_acr, bpi_broad, bpi_fine, sum_temp, 
                                      sum_do, win_temp, win_sal, win_do))
names(pred_select) <- c("Habitat","Mangrove_Dist","Depth","Slope","Curvature",
                        "ACR_Rugosity","BPI_Broad","BPI_Fine","Sum_Temp","Sum_DO",
                        "Win_Temp","Win_Sal","Win_DO")

# clean up
rm(habitat, mg_dist, depth, slope, curvature, plan_curv, prof_curv, rug_acr,
   rug_vrm, bpi_broad, bpi_fine, sum_temp, sum_sal, sum_do, win_temp, win_sal, 
   win_do, pred_full, i, temp)


# Correlation - Selected Predictors ---------------------------------------

# pearson correlation matrix on selected spatial predictors
set.seed(123)
cl = snow::makeCluster(10)
ppcor_select <- pearson_correlation_matrix(pred_select)
snow::stopCluster(cl)

# plot full correlation matrix
palette <- pnw_palette("Shuksan2", 200, type = "continuous")
par(mar = c(0,0,0,0))
corrplot(ppcor_select, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.6, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.7) # order = FPC, hclust

# save plot as png
Cairo(file = here("GitHub_Repositories","Turcke_MSc_Ch1","Figures","Correlation_SelectPredictorSet.png"),
      bg = "white", type = "png", units = "in", width = 7, height = 7, 
      pointsize = 12, dpi = 600)
par(mar = c(0,0,0,0))
corrplot(ppcor_select, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.6,       # order = FPC, original
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8) 
dev.off()

# save as .csv
write.csv(ppcor_select, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles",
                           "Correlation_SelectPredictorSet.csv"))



# VIF - Selected Predictors -----------------------------------------------


# take random sample of 10000 points
y <- raster::sampleRandom(pred_select, 10000, na.rm = T)
i <- nrow(y)
j <- 1
while (i < 10000) {
  temp <- raster::sampleRandom(pred_select, 10000, na.rm = T)
  y <- rbind(y, temp)
  i <- nrow(y)
  print(j)
  j <- j + 1
}
y <- as.data.frame(y[1:10000,])

# calculate vif for random sample
vif <- vif(y)
vif
write.csv(vif, here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles",
                    "VIF_SelectPredictorSet.csv"), row.names = FALSE)

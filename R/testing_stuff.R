library(easypackages)
libraries("terra","here","sf","sp")

here::i_am("GitHub_Repositories/MSc_Ch1_DataPrep/R/testing_stuff.R")

study_region <- rast(here("Final_Data","Study_Region.tif"))

depth_clip <- rast(here("Final_Data","Depth.tif"))
depth_full <- rast(here("Intermediate_Data", "depth_5x5.tif"))

sum_temp <- rast(here("Final_Data","Water_Quality","Summer_Temperature.tif"))
sum_sal <- rast(here("Final_Data","Water_Quality","Summer_Salinity.tif"))
sum_do <- rast(here("Final_Data","Water_Quality","Summer_Dissolved_Oxygen.tif"))
win_temp <- rast(here("Final_Data","Water_Quality","Winter_Temperature.tif"))
win_sal <- rast(here("Final_Data","Water_Quality","Winter_Salinity.tif"))
win_do <- rast(here("Final_Data","Water_Quality","Winter_Dissolved_Oxygen.tif"))

sum_temp_pts <- read.csv(here("Intermediate_Data","Water_Quality","temp_summer.csv"))
coordinates(sum_temp_pts) <- ~ LON_M + LAT_M
sum_temp_pts <- vect(sum_temp_pts)

# plotting unclipped vs clipped depth rasters
plot(depth_full)
plot(depth_clip, col = "blue", add = T)

# plotting interpolated raster vs input grid
terra::plot(study_region, col = "purple")
terra::plot(sum_temp, col = "darkblue", add = T)

# plotting interpolated rasters for initial assessment
terra::plot(sum_temp)
terra::plot(sum_sal)
terra::plot(sum_do)
terra::plot(win_temp)
terra::plot(win_sal)
terra::plot(win_do)
terra::plot(sum_temp)
terra::points(sum_temp_pts, col = sum_temp_pts$SUM_TEMP)



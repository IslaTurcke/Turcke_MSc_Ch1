### WELCOME ###

# This is script __ of __ in Isla's data plotting pipeline.

# This script is used to plot results from the Maximum Entropy
# habitat suitability models (HSMs) of five focal species for Isla Turcke's first 
# MSc chapter in the lab of Dr. S.J. Green at the University of Alberta (2022-2025). 
# Data are specific to southern Florida.



# Set Up ------------------------------------------------------------------


# install ENMTools, sf, and terra, if necessary
# install.packages(c("devtools", "sf", "terra")); library(devtools); devtools::install_github("danlwarren/ENMTools", force = TRUE)

# load packages
library(easypackages)
libraries("sf","terra","here","tidyverse","dplyr","ggplot2","ggmap","gridExtra","viridis","svglite")

# set working directory
setwd("Z:/Isla_MSc_Ch1/")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/HSM_Plotting.R")

# change where large temporary rasters are saved
terraOptions(tempdir = "Z:/Isla_MSc_Ch1/Temp/")

# make colour palette
cols_sp <- c("#EAC211","#770C3E","#00BDAA","#0A7EC2","#0E323A")

# read in summary stats for HSM results
hsm_summary <- read.csv(here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","MaxEnt_Summary_Subadult.csv"))



# Variable Importance -----------------------------------------------------


# keep only columns of permutation importance values
pi_data <- hsm_summary %>% select(c("Species", contains("permutation.importance")))

# change column names
colnames(pi_data) <- gsub("permutation.importance_", "", colnames(pi_data))

# Reshape the data
pi_long <- pi_data %>%
  pivot_longer(
    cols = -Species,
    names_to = c("Predictor", ".value"),
    names_pattern = "(.*).(mean|upper|lower)"
  ) %>% select(-c("upper","lower"))

# removing _ from predictor names
pi_long$Predictor <- gsub("_", " ", pi_long$Predictor)

# make rows for bottom 5 predictors that are not being included ("others")
sca_coel_others <- pi_long %>% filter(Species == "SCA_COEL", Predictor %in% c("Winter Dissolved Oxygen","Summer Temperature","BPI Fine","Winter Temperature","Curvature"))
sca_coel_others <- sum(sca_coel_others[,"mean"])
sca_coer_others <- pi_long %>% filter(Species == "SCA_COER", Predictor %in% c("Winter Dissolved Oxygen","Summer Temperature","BPI Fine","Winter Temperature","Curvature"))
sca_coer_others <- sum(sca_coer_others[,"mean"])
sca_guac_others <- pi_long %>% filter(Species == "SCA_GUAC", Predictor %in% c("Winter Dissolved Oxygen","Summer Temperature","BPI Fine","Winter Temperature","Curvature"))
sca_guac_others <- sum(sca_guac_others[,"mean"])
lut_gris_others <- pi_long %>% filter(Species == "LUT_GRIS", Predictor %in% c("Winter Dissolved Oxygen","Summer Temperature","BPI Fine","Winter Temperature","Curvature"))
lut_gris_others <- sum(lut_gris_others[,"mean"])
hae_sciu_others <- pi_long %>% filter(Species == "HAE_SCIU", Predictor %in% c("Winter Dissolved Oxygen","Summer Temperature","BPI Fine","Winter Temperature","Curvature"))
hae_sciu_others <- sum(hae_sciu_others[,"mean"])

others <- data.frame(Species = c("SCA_COEL","SCA_COER","SCA_GUAC","LUT_GRIS","HAE_SCIU"),
                     Predictor = c("Others", "Others", "Others", "Others", "Others"),
                     mean = c(sca_coel_others, sca_coer_others, sca_guac_others, lut_gris_others, hae_sciu_others))

pi_long <- rbind(pi_long, others)

# change capitalization of predictor names
pi_long$Predictor <- gsub("Habitat Type", "Habitat type", pi_long$Predictor)
pi_long$Predictor <- gsub("Mangrove Distance", "Distance to mangrove", pi_long$Predictor)
pi_long$Predictor <- gsub("Summer Dissolved Oxygen", "Summer dissolved oxygen", pi_long$Predictor)
pi_long$Predictor <- gsub("BPI Broad", "Broad scale BPI", pi_long$Predictor)
pi_long$Predictor <- gsub("Rugosity ACR", "ACR rugosity", pi_long$Predictor)
pi_long$Predictor <- gsub("Winter Salinity", "Winter salinity", pi_long$Predictor)

# keep only rows for top 8 predictors and "others"
pi_long <- pi_long %>% filter(Predictor %in% c("Habitat type","Slope","Distance to mangrove","Summer dissolved oxygen",
                                               "ACR rugosity","Depth","Broad scale BPI","Winter salinity", "Others"))

# change species names to look nice
pi_long$Species <- gsub("SCA_COEL", "S. coelestinus", pi_long$Species)
pi_long$Species <- gsub("SCA_COER", "S. coeruleus", pi_long$Species)
pi_long$Species <- gsub("SCA_GUAC", "S. guacamaia", pi_long$Species)
pi_long$Species <- gsub("LUT_GRIS", "L. griseus", pi_long$Species)
pi_long$Species <- gsub("HAE_SCIU", "H. sciurus", pi_long$Species)

# Order of stacking
pi_long$Species <- factor(pi_long$Species, levels = c("S. coelestinus","S. coeruleus","S. guacamaia","L. griseus","H. sciurus"))

pi_long$Predictor <- factor(pi_long$Predictor, levels = c("Others","Winter salinity","Broad scale BPI","Depth","ACR rugosity","Summer dissolved oxygen",
                                                          "Distance to mangrove","Slope","Habitat type"))

# Define custom colors
species_colors <- c("HAE_SCIU" = "#EAC211", "LUT_GRIS" = "#470C2F", "SCA_GUAC" = "#00BDAA", "SCA_COER" = "#0A7EC2", "SCA_COEL" = "#0E323A")
#p_col <- RColorBrewer::brewer.pal(7, "Dark2")#viridis(7, direction = -1)
#predictor_colors <- c(p_col[7],p_col[6],p_col[5],p_col[4],p_col[3],p_col[2],p_col[1])
predictor_colors <- c("grey", viridis(8, direction = -1))

# Create the plot
p1 <- ggplot(pi_long, aes(x = Species, y = mean, fill = Predictor)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = predictor_colors, guide = guide_legend(reverse = TRUE)) +  # Apply custom colors
  coord_flip() +
  theme_classic() +
  labs(x = "Species", y = "Permutation importance") +
  theme(axis.text.y = element_text(face = "italic"), legend.position = "right")
p1


## Predictor Importance Variation ------------------------------------------

# edit dataset
data_pi <- data %>% select(contains("permutation.importance_mean"))
colnames(data_pi) <- gsub(".permutation.importance_mean", "", colnames(data_pi))
colnames(data_pi) <- gsub("_", " ", colnames(data_pi))

# Function to calculate mean and 95% CI
calculate_mean_ci <- function(column) {
  mean_val <- mean(column)
  ci <- qt(0.975, df = length(column) - 1) * sd(column) / sqrt(length(column))
  return(c(mean = mean_val, ci = ci))
}

# Apply the function to each column and create a new data frame
perm_import <- data_pi %>%
  summarise(across(everything(), calculate_mean_ci)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(statistic = rep(c("mean", "ci"), times = 13)) %>%
  pivot_wider(names_from = statistic, values_from = value)

# set order for plotting
perm_import <- perm_import %>% arrange(desc(mean)) %>%
  mutate(variable = factor(variable, levels = variable))

# add colours
perm_import$color <- c(rev(predictor_colors), "grey","grey","grey","grey")

# plot
p2 <- ggplot(perm_import, aes(x = variable, y = mean)) +
  geom_point(aes(fill = color), shape = 21, size = 4, color = "black", stroke = 0.5) +  # Customize dots
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2) +  # Add error bars
  scale_fill_identity() +  # Use the fill colors from the 'color' column directly
  theme_bw() +
  coord_flip() +
  labs(x = "Predictor", y = "Mean permutation importance ± 95% CI") +
  scale_x_discrete(limits = rev(levels(perm_import$variable)))  # Reverse x-axis order
p2

legend <- get_legend(p1)
p1_nl <- p1 + theme(legend.position = "none")

horiz <- plot_grid(p2, legend, rel_widths = c(2.5, 1), axis = "t")

var_import_top <- plot_grid(p1_nl, horiz, ncol = 1)
var_import_top

library(svglite)
ggsave("VariableImportance.svg", var_import_top, width = 8, height = 5, units = "in", dpi = 500)



# Response Curves ---------------------------------------------------------


# function to read in response curve data
read_dat_file <- function(file) {
  df <- read.table(file, header = TRUE, sep = ",")
  
  # add column for species code
  species <- substr(basename(file), 1, 8)
  df <- cbind(Species = species, df)
  
  return(df)
}

# read in data
bp_files <- list.files(here("HSM_Results","Subadult_BlueParrotfish","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
bp_resp <- lapply(bp_files, read_dat_file) 

mp_files <- list.files(here("HSM_Results","Subadult_MidnightParrotfish","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
mp_resp <- lapply(mp_files, read_dat_file) 

rp_files <- list.files(here("HSM_Results","Subadult_RainbowParrotfish","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
rp_resp <- lapply(rp_files, read_dat_file) 

gs_files <- list.files(here("HSM_Results","Subadult_GraySnapper","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
gs_resp <- lapply(gs_files, read_dat_file) 

bg_files <- list.files(here("HSM_Results","Subadult_BluestripedGrunt","Plots"), pattern = "^[^0-9]*_only.dat", full.names = TRUE)
bg_resp <- lapply(bg_files, read_dat_file) 

rm(bp_files, mp_files, rp_files, gs_files, bg_files)


## Habitat Type ------------------------------------------------------------

# get only habitat type response data
habitat_resp <- rbind(bg_resp[[5]], gs_resp[[5]], rp_resp[[5]], bp_resp[[5]], mp_resp[[5]])
habitat_resp <- habitat_resp %>% filter(habitat_resp$x != 13)

# change species names to look nice
habitat_resp$Species <- gsub("SCA_COEL", "S. coelestinus", habitat_resp$Species)
habitat_resp$Species <- gsub("SCA_COER", "S. coeruleus", habitat_resp$Species)
habitat_resp$Species <- gsub("SCA_GUAC", "S. guacamaia", habitat_resp$Species)
habitat_resp$Species <- gsub("LUT_GRIS", "L. griseus", habitat_resp$Species)
habitat_resp$Species <- gsub("HAE_SCIU", "H. sciurus", habitat_resp$Species)

# set order for plotting 
habitat_resp$Species <- factor(habitat_resp$Species, levels = c("H. sciurus","L. griseus","S. guacamaia","S. coeruleus","S. coelestinus"))

# change habitat type ID back to the name
habitat_resp$x <- gsub("10", "Reef rubble", habitat_resp$x)
habitat_resp$x <- gsub("11", "Mangrove", habitat_resp$x)
habitat_resp$x <- gsub("12", "Artificial", habitat_resp$x)
habitat_resp$x <- gsub("14", "Ridge", habitat_resp$x)
habitat_resp$x <- gsub("1", "Patch reef", habitat_resp$x)
habitat_resp$x <- gsub("2", "Scattered coral/rock", habitat_resp$x)
habitat_resp$x <- gsub("3", "Seagrass (continuous)", habitat_resp$x)
habitat_resp$x <- gsub("4", "Seagrass (discontinuous)", habitat_resp$x)
habitat_resp$x <- gsub("5", "Unconsolidated sediment", habitat_resp$x)
habitat_resp$x <- gsub("6", "Aggregate reef", habitat_resp$x)
habitat_resp$x <- gsub("8", "Pavement", habitat_resp$x)

# set order for plotting based on suitability values
habitat_resp$x <- factor(habitat_resp$x, levels = c("Patch reef","Aggregate reef","Scattered coral/rock","Pavement",
                                                    "Reef rubble","Ridge","Artificial","Mangrove","Seagrass (continuous)",
                                                    "Unconsolidated sediment","Seagrass (discontinuous)"))

# create the plot
habitat_response <- ggplot(habitat_resp, aes(x = x, y = y, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  labs(x = "Habitat type", y = "Relative suitability")
habitat_response


## Slope ------------------------------------------------------------

# get only slope response data
slope_resp <- rbind(bg_resp[[8]], gs_resp[[8]], rp_resp[[8]], bp_resp[[8]], mp_resp[[8]])

# change species names to look nice
slope_resp$Species <- gsub("SCA_COEL", "S. coelestinus", slope_resp$Species)
slope_resp$Species <- gsub("SCA_COER", "S. coeruleus", slope_resp$Species)
slope_resp$Species <- gsub("SCA_GUAC", "S. guacamaia", slope_resp$Species)
slope_resp$Species <- gsub("LUT_GRIS", "L. griseus", slope_resp$Species)
slope_resp$Species <- gsub("HAE_SCIU", "H. sciurus", slope_resp$Species)

# set order for plotting 
slope_resp$Species <- factor(slope_resp$Species, levels = c("H. sciurus","L. griseus","S. guacamaia","S. coeruleus","S. coelestinus"))

# create the plot
slope_response <- ggplot(slope_resp, aes(x = x, y = y, colour = Species)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  labs(x = "Slope (degrees)", y = "Relative suitability")
slope_response


## Mangrove Dist ------------------------------------------------------------

# get only mangrove distance response data
mgdist_resp <- rbind(bg_resp[[6]], gs_resp[[6]], rp_resp[[6]], bp_resp[[6]], mp_resp[[6]])

# change species names to look nice
mgdist_resp$Species <- gsub("SCA_COEL", "S. coelestinus", mgdist_resp$Species)
mgdist_resp$Species <- gsub("SCA_COER", "S. coeruleus", mgdist_resp$Species)
mgdist_resp$Species <- gsub("SCA_GUAC", "S. guacamaia", mgdist_resp$Species)
mgdist_resp$Species <- gsub("LUT_GRIS", "L. griseus", mgdist_resp$Species)
mgdist_resp$Species <- gsub("HAE_SCIU", "H. sciurus", mgdist_resp$Species)

# set order for plotting 
mgdist_resp$Species <- factor(mgdist_resp$Species, levels = c("H. sciurus","L. griseus","S. guacamaia","S. coeruleus","S. coelestinus"))

# create the plot
mgdist_response <- ggplot(mgdist_resp, aes(x = x, y = y, colour = Species)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  labs(x = "Distance to mangrove (m)", y = "Relative suitability")
mgdist_response


## Summer Dissolved Oxygen ------------------------------------------------------------

# get only summer DO response data
summerDO_resp <- rbind(bg_resp[[9]], gs_resp[[9]], rp_resp[[9]], bp_resp[[9]], mp_resp[[9]])

# change species names to look nice
summerDO_resp$Species <- gsub("SCA_COEL", "S. coelestinus", summerDO_resp$Species)
summerDO_resp$Species <- gsub("SCA_COER", "S. coeruleus", summerDO_resp$Species)
summerDO_resp$Species <- gsub("SCA_GUAC", "S. guacamaia", summerDO_resp$Species)
summerDO_resp$Species <- gsub("LUT_GRIS", "L. griseus", summerDO_resp$Species)
summerDO_resp$Species <- gsub("HAE_SCIU", "H. sciurus", summerDO_resp$Species)

# set order for plotting 
summerDO_resp$Species <- factor(summerDO_resp$Species, levels = c("H. sciurus","L. griseus","S. guacamaia","S. coeruleus","S. coelestinus"))

# create the plot
summerDO_response <- ggplot(summerDO_resp, aes(x = x, y = y, colour = Species)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  labs(x = "Summer dissolved oxygen", y = "Relative suitability")
summerDO_response


## ACR Rugosity ------------------------------------------------------------

# get only ACR rugosity response data
rugosity_resp <- rbind(bg_resp[[7]], gs_resp[[7]], rp_resp[[7]], bp_resp[[7]], mp_resp[[7]])

# change species names to look nice
rugosity_resp$Species <- gsub("SCA_COEL", "S. coelestinus", rugosity_resp$Species)
rugosity_resp$Species <- gsub("SCA_COER", "S. coeruleus", rugosity_resp$Species)
rugosity_resp$Species <- gsub("SCA_GUAC", "S. guacamaia", rugosity_resp$Species)
rugosity_resp$Species <- gsub("LUT_GRIS", "L. griseus", rugosity_resp$Species)
rugosity_resp$Species <- gsub("HAE_SCIU", "H. sciurus", rugosity_resp$Species)

# set order for plotting 
rugosity_resp$Species <- factor(rugosity_resp$Species, levels = c("H. sciurus","L. griseus","S. guacamaia","S. coeruleus","S. coelestinus"))

# create the plot
rugosity_response <- ggplot(rugosity_resp, aes(x = x, y = y, colour = Species)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  labs(x = "ACR rugosity", y = "Relative suitability")
rugosity_response


## Depth ------------------------------------------------------------

# get only depth response data
depth_resp <- rbind(bg_resp[[4]], gs_resp[[4]], rp_resp[[4]], bp_resp[[4]], mp_resp[[4]])

# change species names to look nice
depth_resp$Species <- gsub("SCA_COEL", "S. coelestinus", depth_resp$Species)
depth_resp$Species <- gsub("SCA_COER", "S. coeruleus", depth_resp$Species)
depth_resp$Species <- gsub("SCA_GUAC", "S. guacamaia", depth_resp$Species)
depth_resp$Species <- gsub("LUT_GRIS", "L. griseus", depth_resp$Species)
depth_resp$Species <- gsub("HAE_SCIU", "H. sciurus", depth_resp$Species)

# set order for plotting 
depth_resp$Species <- factor(depth_resp$Species, levels = c("H. sciurus","L. griseus","S. guacamaia","S. coeruleus","S. coelestinus"))

# create the plot
depth_response <- ggplot(depth_resp, aes(x = x, y = y, colour = Species)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  labs(x = "Depth (m)", y = "Relative suitability")
depth_response


## Broad Scale BPI ------------------------------------------------------------

# get only broad scale BPI response data
bbpi_resp <- rbind(bg_resp[[1]], gs_resp[[1]], rp_resp[[1]], bp_resp[[1]], mp_resp[[1]])

# change species names to look nice
bbpi_resp$Species <- gsub("SCA_COEL", "S. coelestinus", bbpi_resp$Species)
bbpi_resp$Species <- gsub("SCA_COER", "S. coeruleus", bbpi_resp$Species)
bbpi_resp$Species <- gsub("SCA_GUAC", "S. guacamaia", bbpi_resp$Species)
bbpi_resp$Species <- gsub("LUT_GRIS", "L. griseus", bbpi_resp$Species)
bbpi_resp$Species <- gsub("HAE_SCIU", "H. sciurus", bbpi_resp$Species)

# set order for plotting 
bbpi_resp$Species <- factor(bbpi_resp$Species, levels = c("H. sciurus","L. griseus","S. guacamaia","S. coeruleus","S. coelestinus"))

# create the plot
bbpi_response <- ggplot(bbpi_resp, aes(x = x, y = y, colour = Species)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  labs(x = "Broad scale BPI", y = "Relative suitability")
bbpi_response


## Winter Salinity ------------------------------------------------------------

# get only winter salinity response data
wintersal_resp <- rbind(bg_resp[[12]], gs_resp[[12]], rp_resp[[12]], bp_resp[[12]], mp_resp[[12]])

# change species names to look nice
wintersal_resp$Species <- gsub("SCA_COEL", "S. coelestinus", wintersal_resp$Species)
wintersal_resp$Species <- gsub("SCA_COER", "S. coeruleus", wintersal_resp$Species)
wintersal_resp$Species <- gsub("SCA_GUAC", "S. guacamaia", wintersal_resp$Species)
wintersal_resp$Species <- gsub("LUT_GRIS", "L. griseus", wintersal_resp$Species)
wintersal_resp$Species <- gsub("HAE_SCIU", "H. sciurus", wintersal_resp$Species)

# set order for plotting 
wintersal_resp$Species <- factor(wintersal_resp$Species, levels = c("H. sciurus","L. griseus","S. guacamaia","S. coeruleus","S. coelestinus"))

# create the plot
wintersal_response <- ggplot(wintersal_resp, aes(x = x, y = y, colour = Species)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  labs(x = "Winter salinity", y = "Relative suitability")
wintersal_response








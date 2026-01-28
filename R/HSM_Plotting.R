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
libraries("sf","terra","here","tidyverse","dplyr","ggplot2","gridExtra","cowplot",
          "viridis","svglite", "patchwork")

# set working directory
setwd("Z:/Isla_MSc_Ch1/")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/IslaSpence_PhD_Ch1/R/HSM_Plotting.R")

# set path to Figures folder in GitHub directory
figures_path <- here("GitHub_Repositories/IslaSpence_PhD_Ch1/Figures")

# change where large temporary rasters are saved
terraOptions(tempdir = "Z:/Isla_MSc_Ch1/Temp/")

# make colour palette
cols_sp <- c("#EAC211","#E9850C","#00BDAA","#0A7EC2","#0E323A")

# read in summary stats for HSM results
hsm_summary <- read.csv(here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","MaxEnt_Summary_Subadult.csv")) %>% 
  select(-"X")



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
pi_long_ALL <- pi_long

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
pi_long$Predictor <- gsub("Summer Dissolved Oxygen", "Summer DO", pi_long$Predictor)
pi_long$Predictor <- gsub("BPI Broad", "Broad scale BPI", pi_long$Predictor)
pi_long$Predictor <- gsub("Rugosity ACR", "ACR rugosity", pi_long$Predictor)
pi_long$Predictor <- gsub("Winter Salinity", "Winter salinity", pi_long$Predictor)

# keep only rows for top 8 predictors and "others"
pi_long <- pi_long %>% filter(Predictor %in% c("Habitat type","Slope","Distance to mangrove","Summer DO",
                                               "ACR rugosity","Depth","Broad scale BPI","Winter salinity", "Others"))

# change species names to look nice
pi_long$Species <- gsub("SCA_COEL", "S. coelestinus", pi_long$Species)
pi_long$Species <- gsub("SCA_COER", "S. coeruleus", pi_long$Species)
pi_long$Species <- gsub("SCA_GUAC", "S. guacamaia", pi_long$Species)
pi_long$Species <- gsub("LUT_GRIS", "L. griseus", pi_long$Species)
pi_long$Species <- gsub("HAE_SCIU", "H. sciurus", pi_long$Species)

# Order of stacking
pi_long$Species <- factor(pi_long$Species, levels = c("S. coelestinus","S. coeruleus","S. guacamaia","L. griseus","H. sciurus"))

pi_long$Predictor <- factor(pi_long$Predictor, levels = c("Others","Winter salinity","Broad scale BPI","Depth","ACR rugosity","Summer DO",
                                                          "Distance to mangrove","Slope","Habitat type"))

# Define custom colors
species_colors <- c("HAE_SCIU" = "#EAC211", "LUT_GRIS" = "#E9850C", "SCA_GUAC" = "#00BDAA", "SCA_COER" = "#0A7EC2", "SCA_COEL" = "#0E323A")
predictor_colors <- c("grey", viridis(8, direction = -1))

# Create the plot
p1 <- ggplot(pi_long, aes(x = Species, y = mean, fill = Predictor)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = predictor_colors, guide = guide_legend(reverse = TRUE)) +  # Apply custom colors
  coord_flip() +
  theme_bw() +
  labs(x = "Species", y = "Cumulative permutation importance") +
  theme(axis.text.y = element_text(face = "italic"), legend.position = "right")
p1


## Predictor Importance Variation ------------------------------------------

# pivot to wide 
pi_wide <- pi_long_ALL %>%
  pivot_wider(
    names_from = Species,
    values_from = mean
  )

# add mean and ci value
pi_wide_calc <- pi_wide %>%
  mutate(
    mean = rowMeans(select(., HAE_SCIU:SCA_GUAC), na.rm = TRUE),
    ci = 1.96 * apply(select(., HAE_SCIU:SCA_GUAC), 1, function(x) sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x))))
  )

# change capitalization of predictor names
pi_wide_calc$Predictor <- gsub("Habitat Type", "Habitat type", pi_wide_calc$Predictor) 
pi_wide_calc$Predictor <- gsub("Summer Dissolved Oxygen", "Summer DO", pi_wide_calc$Predictor)
pi_wide_calc$Predictor <- gsub("BPI Broad", "Broad scale BPI", pi_wide_calc$Predictor) 
pi_wide_calc$Predictor <- gsub("Rugosity ACR", "ACR rugosity", pi_wide_calc$Predictor)
pi_wide_calc$Predictor <- gsub("Winter Salinity", "Winter salinity", pi_wide_calc$Predictor)
pi_wide_calc$Predictor <- gsub("Winter Dissolved Oxygen", "Winter DO", pi_wide_calc$Predictor)
pi_wide_calc$Predictor <- gsub("Summer Temperature", "Summer temperature", pi_wide_calc$Predictor)
pi_wide_calc$Predictor <- gsub("BPI Fine", "Fine scale BPI", pi_wide_calc$Predictor)
pi_wide_calc$Predictor <- gsub("Winter Temperature", "Winter temperature", pi_wide_calc$Predictor)
pi_wide_calc$Predictor <- gsub("Curvature", "Mean curvature", pi_wide_calc$Predictor)

# set order for plotting
pi_wide_calc <- pi_wide_calc %>% arrange(desc(mean)) %>%
  mutate(Predictor = factor(Predictor, levels = Predictor))

# add colours
pi_wide_calc$color <- c(rev(predictor_colors), "grey","grey","grey","grey")

# plot
p2 <- ggplot(pi_wide_calc, aes(x = Predictor, y = mean)) +
  geom_point(aes(fill = color), shape = 21, size = 4, color = "black", stroke = 0.5) +  # Customize dots
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2) +  # Add error bars
  scale_fill_identity() +  # Use the fill colors from the 'color' column directly
  theme_bw() +
  coord_flip() +
  labs(x = "Predictor", y = "Mean permutation importance ± 95% CI") +
  scale_x_discrete(limits = rev(levels(pi_wide_calc$Predictor)))  # Reverse x-axis order

p2

# remove legend from p1
p1_nl <- p1 + theme(legend.position = "none")

# plot without legend - preferred by Stephanie
var_import <- plot_grid(p2, p1_nl, ncol = 1, labels = c("a","b"))
var_import

ggsave("VariableImportance.png", var_import, path = figures_path, width = 8, height = 5, units = "in", dpi = 600, bg = "white")



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
habitat_resp$x <- gsub("5", "Sediment", habitat_resp$x)
habitat_resp$x <- gsub("6", "Aggregate reef", habitat_resp$x)
habitat_resp$x <- gsub("8", "Pavement", habitat_resp$x)

# set order for plotting based on suitability values
habitat_resp$x <- factor(habitat_resp$x, levels = c("Patch reef","Aggregate reef","Scattered coral/rock","Pavement",
                                                    "Reef rubble","Ridge","Artificial","Mangrove","Seagrass (continuous)",
                                                    "Sediment","Seagrass (discontinuous)"))

# create the plot
habitat_response <- ggplot(habitat_resp, aes(x = x, y = y, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  scale_fill_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  theme(legend.position = "right") +
  labs(x = "Habitat type", y = "Relative suitability") +
  scale_x_discrete(labels = label_wrap_gen(10, multi_line = T)) +  #expand = expansion(mult = c(0.05, 0.05))
  guides(fill = guide_legend(theme = theme(
    legend.text = element_text(size = 10, face = "italic")))) +
  theme(plot.margin = margin(c(10, 40, 0, 10)))

habitat_response

# save the plot
#ggsave("Habitat_Type.png", width = 6.5, height = 3, units = "in", dpi = 500)


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
  theme(legend.position = "none") +
  labs(x = "Slope (degrees)", y = "Relative suitability") +
  theme(plot.margin = margin(c(10, 0, 0, 10)))
slope_response

# save the plot
#ggsave("Slope.png", width = 5.5, height = 3, units = "in", dpi = 500)


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
mgdist_response <- ggplot(mgdist_resp, aes(x = x/1000, y = y, colour = Species)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_sp) +  # Apply custom colors
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Distance to mangrove (km)", y = element_blank()) +
  theme(plot.margin = margin(c(10, 0, 0, 10)))
mgdist_response

# save the plot
#ggsave("Mangrove_Distance.png", width = 4, height = 3, units = "in", dpi = 500)


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
  theme(legend.position = "none") +
  labs(x = "Broad scale BPI", y = element_blank()) +
  theme(plot.margin = margin(c(10, 0, 0, 10)))
bbpi_response

# save the plot
#ggsave("Broad_BPI.png", width = 4, height = 3, units = "in", dpi = 500)


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
  theme(legend.position = "none") +
  labs(x = "Winter salinity (psu)", y = element_blank()) +
  theme(plot.margin = margin(c(10, 5, 0, 10)))
wintersal_response

# save the plot
#ggsave("Winter_Salinity.png", width = 4, height = 3, units = "in", dpi = 500)


## Combining Response Curves -----------------------------------------------

bottom_respcurvs <- plot_grid(slope_response, mgdist_response, bbpi_response, wintersal_response, 
                              ncol = 4, rel_widths = c(1,1,1,1), axis = "t",
                              labels = c("b","c","d","e"), label_y = 1)
print(bottom_respcurvs)

response_curves <- plot_grid(habitat_response, bottom_respcurvs, 
                             ncol = 1, labels = c("a",""))
response_curves

ggsave("Response_Curves.png", path = figures_path, width = 10, height = 5, units = "in", dpi = 600)



# Suitability Overlap Metrics ---------------------------------------------


# import suitability overlap metric dataset
suit_overlap <- read.csv(here("GitHub_Repositories/Turcke_MSc_Ch1/Data_SmallFiles/SuitabilityOverlapMetrics.csv"))

# set order for plotting 
suit_overlap$Species_Pair <- factor(suit_overlap$Species_Pair, 
                                    levels = c("BG-GS","BP-MP","MP-RP","RP-BP","BG-BP","BG-MP","BG-RP","GS-BP","GS-MP","GS-RP"))
suit_overlap$Comparison_Type <- factor(suit_overlap$Comparison_Type,
                                       levels = c("Intra-functional group","Inter-functional group"))

# calculate means for intra- and inter-functional group comparisons
IntraInter_means <- suit_overlap %>% 
  group_by(Comparison_Type) %>% 
  summarize(Mean_D = mean(D), Mean_I = mean(I), Mean_Rho = mean(Rho))

intra_D <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Intra-functional group", "Mean_D"])
intra_D <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Intra-functional group", "Mean_D"])
intra_D <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Intra-functional group", "Mean_D"])
intra_I <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Intra-functional group", "Mean_I"])
intra_I <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Intra-functional group", "Mean_I"])
intra_I <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Intra-functional group", "Mean_I"])
intra_R <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Intra-functional group", "Mean_Rho"])
intra_R <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Intra-functional group", "Mean_Rho"])
intra_R <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Intra-functional group", "Mean_Rho"])

inter_D <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Inter-functional group", "Mean_D"])
inter_D <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Inter-functional group", "Mean_D"])
inter_D <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Inter-functional group", "Mean_D"])
inter_I <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Inter-functional group", "Mean_I"])
inter_I <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Inter-functional group", "Mean_I"])
inter_I <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Inter-functional group", "Mean_I"])
inter_R <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Inter-functional group", "Mean_Rho"])
inter_R <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Inter-functional group", "Mean_Rho"])
inter_R <- as.numeric(IntraInter_means[IntraInter_means$Comparison_Type == "Inter-functional group", "Mean_Rho"])


# plot D
D_plot <- ggplot(suit_overlap, aes(x = Species_Pair, y = D, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  scale_fill_manual(values = c("#769772","#867A90")) +  
  theme_classic() +
  theme(legend.position = "top") +
  labs(x = element_blank(), y = "D") +
  geom_segment(aes(x = "BG-GS", xend = "RP-BP", y = intra_D, yend = intra_D), color = "#475E45", linetype = "dashed", linewidth = 0.7) +
  geom_segment(aes(x = "BG-BP", xend = "GS-RP", y = inter_D, yend = inter_D), color = "#52495A", linetype = "dashed", linewidth = 0.7) +
  guides(fill = guide_legend(theme = theme(
    legend.title = element_blank())))
D_plot

# plot I
I_plot <- ggplot(suit_overlap, aes(x = Species_Pair, y = I, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  scale_fill_manual(values = c("#769772","#867A90")) +  
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = element_blank(), y = "I") +
  geom_segment(aes(x = "BG-GS", xend = "RP-BP", y = intra_I, yend = intra_I), color = "#475E45", linetype = "dashed", linewidth = 0.7) +
  geom_segment(aes(x = "BG-BP", xend = "GS-RP", y = inter_I, yend = inter_I), color = "#52495A", linetype = "dashed", linewidth = 0.7)
I_plot

# plot Rho
R_plot <- ggplot(suit_overlap, aes(x = Species_Pair, y = Rho, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  scale_fill_manual(values = c("#769772","#867A90")) +  
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Species Comparison", y = "Rho") +
  geom_segment(aes(x = "BG-GS", xend = "RP-BP", y = intra_R, yend = intra_R), color = "#475E45", linetype = "dashed", linewidth = 0.7) +
  geom_segment(aes(x = "BG-BP", xend = "GS-RP", y = inter_R, yend = inter_R), color = "#52495A", linetype = "dashed", linewidth = 0.7)
R_plot

# combine into one figure
suit_overlap_fig <- plot_grid(D_plot, I_plot, R_plot, ncol = 1)
suit_overlap_fig

ggsave("Suitability_Overlap_Metrics.png", path = figures_path, width = 5, height = 6, units = "in", dpi = 600)



# Identity Test -----------------------------------------------------------


# import and combine data
ID_paths <- list.files(here("HSM_Analysis", "Identity_Test", "Results"), full.names = TRUE, pattern = "^OverlapValues_.*\\.csv$")

ID_data <- map_dfr(ID_paths, function(path) {
  df <- read_csv(path, col_names = TRUE)
  
  # Extract species pair name from the filename
  file_name <- tools::file_path_sans_ext(basename(path))
  species_pair <- str_remove(file_name, "OverlapValues_")
  df$species_pair <- species_pair
  
  return(df)
})

# set column names
colnames(ID_data) <- c("row","D","I","R","species_pair")

# Step 2: Reshape the data
ID_long <- ID_data %>%
  pivot_longer(cols = c(D, I, R), names_to = "metric", values_to = "value")

# Separate empirical and permuted values
permuted_data <- ID_long %>% filter(row == 1)
empirical_data <- ID_long %>% filter(row == "empirical")

# set pair order
pair_order <- c("GS_BG","BP_RP","BP_MP","MP_RP","GS_BP","GS_MP", "GS_RP", "BG_BP", "BG_MP", "BG_RP")

permuted_data$species_pair <- factor(permuted_data$species_pair, levels = pair_order)
empirical_data$species_pair <- factor(empirical_data$species_pair, levels = pair_order)

# asign colours for intra- vs inter-functional group pairs
permuted_data <- permuted_data %>% 
  mutate(colour_group = ifelse(species_pair %in% pair_order[1:4], "A", "B"))
empirical_data <- empirical_data %>% 
  mutate(colour_group = ifelse(species_pair %in% pair_order[1:4], "A", "B"))


## plots for D, I, R -----------------------------------------------------------------------

make_metric_plot <- function(metric_code, metric_name, show_legend) {

  violin_data <- permuted_data %>% filter(metric_code == metric)
  point_data <- empirical_data %>% filter(metric_code == metric)
  
  ggplot(violin_data, aes(x = value, y = fct_rev(species_pair), 
                          fill = colour_group, colour = colour_group)) +
    
    geom_violin(data = violin_data, alpha = 0.6, draw_quantiles = c(0.5), scale = "width") +
    
    geom_point(data = point_data, aes(x = value, y = fct_rev(species_pair)),
               size = 2, shape = 21, fill = "white", stroke = 2) +
    
    scale_fill_manual(values = c("A" = "#1f78b4", "B" = "#b2df8a"),
                      labels = c("A" = "Intra-functional group", "B" = "Inter-functional group"),
                      name = NULL) +
    scale_color_manual(values = c("A" = "#1f78b4", "B" = "#b2df8a"),
                       labels = c("A" = "Intra-functional group", "B" = "Inter-functional group"),
                       name = NULL) +
    
    labs(x = metric_name, y = NULL) +
    theme_bw(base_size = 12) +
    theme(legend.position = if(show_legend) "bottom" else "none", 
          legend.direction = "horizontal")
}

plot_D <- make_metric_plot(metric_code = "D", "Schoener's D", TRUE)
plot_I <- make_metric_plot(metric_code = "I", "Warren's I", FALSE)
plot_R <- make_metric_plot(metric_code = "R", "Spearman's Rho", FALSE)


## Combine D, I, Rho -------------------------------------------------------

# using patchwork
ID_plot <- (plot_D + plot_spacer() + plot_I + plot_spacer() + plot_R + plot_spacer()) +
  plot_layout(nrow = 1, widths = c(1, 0.01, 1, 0.01, 1, 0.01), guides = "collect", axes = "collect") &
  theme(legend.position = "bottom")

ID_plot

ggsave("Identity_Test.png", path = figures_path, width = 9, height = 5, units = "in", dpi = 600)



# Suitability Histograms --------------------------------------------------


## Blue Parrotfish ---------------------------------------------------------

# read in suitability raster
suit_bp <- rast("Z:/Isla_MSc_Ch1/HSM_Results/Subadult_BlueParrotfish/SCA_COER_avg.asc")

# extract values to a dataframe and remove NAs
vals_bp <- terra::values(suit_bp, dataframe = TRUE, na.rm = TRUE)
above_thresh <- mean(vals_bp[[1]] > 0.5)*100
above_thresh <- paste0(round(above_thresh, digits = 2), "%")
rm(suit_bp)
gc()

# make histogram
hist_bp <- ggplot(vals_bp, aes(x = SCA_COER_avg)) +
  geom_histogram(bins = 100, fill = "#0A7EC2") +
  labs(x = "Relative suitability", y = NULL) +
  coord_cartesian(ylim = c(0, 18000000)) +
  theme_bw() + 
  geom_vline(xintercept = 0.5, linetype = 3) + 
  annotate("text", x = 0.8, y = 6e6, label = above_thresh)

hist_bp

#ggsave("SuitabilityDist_BP.png", path = figures_path, width = 5, height = 5, units = "in", dpi = 600)

rm(vals_bp)


## Midnight Parrotfish ---------------------------------------------------------

# read in suitability raster
suit_mp <- rast("Z:/Isla_MSc_Ch1/HSM_Results/Subadult_MidnightParrotfish/SCA_COEL_avg.asc")

# extract values to a dataframe and remove NAs
vals_mp <- terra::values(suit_mp, dataframe = TRUE, na.rm = TRUE)
above_thresh <- mean(vals_mp[[1]] > 0.5)*100
above_thresh <- paste0(round(above_thresh, digits = 2), "%")
rm(suit_mp)
gc()

# make histogram
hist_mp <- ggplot(vals_mp, aes(x = SCA_COEL_avg)) +
  geom_histogram(bins = 100, fill = "#0E323A") +
  labs(x = "Relative suitability", y = NULL) +
  coord_cartesian(ylim = c(0, 18000000)) +
  theme_bw() + 
  geom_vline(xintercept = 0.5, linetype = 3) + 
  annotate("text", x = 0.8, y = 6e6, label = above_thresh)

#ggsave("SuitabilityDist_MP.png", path = figures_path, width = 5, height = 5, units = "in", dpi = 600)

rm(vals_mp)


## Rainbow Parrotfish ---------------------------------------------------------

# read in suitability raster
suit_rp <- rast("Z:/Isla_MSc_Ch1/HSM_Results/Subadult_RainbowParrotfish/SCA_GUAC_avg.asc")

# extract values to a dataframe and remove NAs
vals_rp <- terra::values(suit_rp, dataframe = TRUE, na.rm = TRUE)
above_thresh <- mean(vals_rp[[1]] > 0.5)*100
above_thresh <- paste0(round(above_thresh, digits = 2), "%")
rm(suit_rp)
gc()

# make histogram
hist_rp <- ggplot(vals_rp, aes(x = SCA_GUAC_avg)) +
  geom_histogram(bins = 100, fill = "#00BDAA") +
  labs(x = "Relative suitability", y = "Pixel count") +
  coord_cartesian(ylim = c(0, 18000000)) +
  theme_bw() + 
  geom_vline(xintercept = 0.5, linetype = 3) + 
  annotate("text", x = 0.8, y = 6e6, label = above_thresh)

#ggsave("SuitabilityDist_RP.png", path = figures_path, width = 5, height = 5, units = "in", dpi = 600)

rm(vals_rp)


## Bluestriped Grunt ---------------------------------------------------------

# read in suitability raster
suit_bg <- rast("Z:/Isla_MSc_Ch1/HSM_Results/Subadult_BluestripedGrunt/HAE_SCIU_avg.asc")

# extract values to a dataframe and remove NAs
vals_bg <- terra::values(suit_bg, dataframe = TRUE, na.rm = TRUE)
above_thresh <- mean(vals_bg[[1]] > 0.5)*100
above_thresh <- paste0(round(above_thresh, digits = 2), "%")
rm(suit_bg)
gc()

# make histogram
hist_bg <- ggplot(vals_bg, aes(x = HAE_SCIU_avg)) +
  geom_histogram(bins = 100, fill = "#EAC211") +
  labs(x = NULL, y = "Pixel count") +
  coord_cartesian(ylim = c(0, 18000000)) +
  theme_bw() + 
  geom_vline(xintercept = 0.5, linetype = 3) + 
  annotate("text", x = 0.8, y = 6e6, label = above_thresh)

#ggsave("SuitabilityDist_bg.png", path = figures_path, width = 5, height = 5, units = "in", dpi = 600)

rm(vals_bg)


## Gray Snapper ---------------------------------------------------------

# read in suitability raster
suit_gs <- rast("Z:/Isla_MSc_Ch1/HSM_Results/Subadult_GraySnapper/LUT_GRIS_avg.asc")

# extract values to a dataframe and remove NAs
vals_gs <- terra::values(suit_gs, dataframe = TRUE, na.rm = TRUE)
above_thresh <- mean(vals_gs[[1]] > 0.5)*100
above_thresh <- paste0(round(above_thresh, digits = 2), "%")
rm(suit_gs)
gc()

# make histogram
hist_gs <- ggplot(vals_gs, aes(x = LUT_GRIS_avg)) +
  geom_histogram(bins = 100, fill = "#E9850C") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(ylim = c(0, 18000000)) +
  theme_bw() + 
  geom_vline(xintercept = 0.5, linetype = 3) + 
  annotate("text", x = 0.8, y = 6e6, label = above_thresh)

#ggsave("SuitabilityDist_gs.png", path = figures_path, width = 5, height = 5, units = "in", dpi = 600)

rm(vals_gs)


## Combine into Figure -----------------------------------------------------
# 
# habitat_getlegend <- ggplot(habitat_resp, aes(x = x, y = y, fill = Species)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.75) +
#   scale_fill_manual(values = cols_sp) +  # Apply custom colors
#   theme_classic() +
#   theme(legend.position = "right",
#         legend.title = element_text(size = 14),
#         legend.key.size = unit(1.0, "cm")) +
#   labs(x = "Habitat type", y = "Relative suitability") +
#   scale_x_discrete(labels = label_wrap_gen(10, multi_line = T)) +  #expand = expansion(mult = c(0.05, 0.05))
#   guides(fill = guide_legend(theme = theme(
#     legend.text = element_text(size = 12, face = "italic")))) +
#   theme(plot.margin = margin(c(10, 40, 0, 10)))
# 
# habitat_getlegend

legend <- get_legend(habitat_response)

hists <- plot_grid(hist_bg, hist_gs, legend, hist_rp, hist_bp, hist_mp, 
                   ncol = 3, align = "hv", axis = "t",
                   labels = c("a","b","","c","d","e"), label_y = 1)

ggsave("Suitability_Histograms.png", path = figures_path, width = 9, height = 5, units = "in", dpi = 600)




# Management Zones --------------------------------------------------------


# read in data
by_name <- read.csv(here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","ManagementZones_byName_SuitabilityData.csv"))
by_type <- read.csv(here("GitHub_Repositories","Turcke_MSc_Ch1","Data_SmallFiles","ManagementZones_byType_SuitabilityData.csv")) %>% 
  filter(ZONE_TYPE %in% c("M:IR","SPA","ROA","WMA","ER","Seascape"))

by_type$ZONE_TYPE <- factor(by_type$ZONE_TYPE, levels = by_type$ZONE_TYPE)

seascape_mean <- by_type[by_type$ZONE_TYPE == "Seascape","MEAN"] 
seascape_std <- by_type[by_type$ZONE_TYPE == "Seascape", "STD"]


## Mean vs Std for each zone -----------------------------------------------

mean_std <- ggplot(data = by_name[-nrow(by_name),], aes(group = ZONE_TYPE)) +
  geom_point(aes(x = MEAN_ALL, y = STD_ALL, colour = ZONE_TYPE, size = AREA)) +
  labs(x = "Mean suitability", y = "Standard deviation in suitability") +
  coord_cartesian(xlim = c(0, 1)) +
  annotate(geom = "text", x = 0.9, y = 0.085, label = "Eastern Dry Rocks", size = 3)
mean_std

# switch x and y axes
std_mean <- ggplot(data = by_name[-nrow(by_name),], aes(group = ZONE_TYPE)) +
  geom_point(aes(x = STD_ALL, y = MEAN_ALL, colour = ZONE_TYPE, size = AREA)) +
  labs(x = "Standard deviation in suitability", y = "Mean suitability") +
  coord_cartesian(ylim = c(0, 1)) +
  annotate(geom = "text", x = 0.09, y = 0.95, label = "Eastern Dry Rocks", size = 3)
std_mean


## Zone types vs seascape average -----------------------------------------------

zone_types <- ggplot(data = by_type[-nrow(by_type),], aes(x = ZONE_TYPE)) +
  geom_linerange(aes(ymin = PCT10, ymax = PCT90), linewidth = 0.5, colour = "grey") +
  geom_crossbar(aes(y = MEDIAN, ymin = MEDIAN, ymax = MEDIAN), 
                width = 0.4,color = "black", middle.linewidth = 0.5) +
  geom_point(aes(y = MEAN), shape = 21, fill = "black", size = 1.5) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_hline(yintercept = seascape_mean, color = "coral1", linetype = "dashed") +
  geom_rect(aes(xmin = -Inf, xmax = Inf, 
                ymin = seascape_mean - seascape_std, ymax = seascape_mean + seascape_std),
            fill = "coral1", alpha = 0.05) +
  labs(y = "Predicted Suitability", x = "Management Zone Type") +
  theme_classic()
zone_types

ggsave("ManagementZones_Suitability.png", path = figures_path, width = 4.5, height = 3.5, units = "in", dpi = 600)




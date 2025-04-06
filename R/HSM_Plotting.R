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
libraries("sf","terra","here","tidyverse","dplyr","ggplot2","gridExtra","cowplot","viridis","svglite")

# set working directory
setwd("Z:/Isla_MSc_Ch1/")

# SET UP RELATIVE PATHS TO DIRECTORIES USING 'HERE'
# set the Isla_MSc_Ch1 folder as the root directory 
here::i_am("GitHub_Repositories/Turcke_MSc_Ch1/R/HSM_Plotting.R")

# set path to Figures folder in GitHub directory
figures_path <- here("GitHub_Repositories/Turcke_MSc_Ch1/Figures")

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

# edit dataset 
data_pi <- hsm_summary[, grep("permutation.importance_mean", names(hsm_summary))]
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

# change capitalization of predictor names
perm_import$variable <- gsub("Habitat Type", "Habitat type", perm_import$variable) 
perm_import$variable <- gsub("Summer Dissolved Oxygen", "Summer DO", perm_import$variable)
perm_import$variable <- gsub("BPI Broad", "Broad scale BPI", perm_import$variable) 
perm_import$variable <- gsub("Rugosity ACR", "ACR rugosity", perm_import$variable)
perm_import$variable <- gsub("Winter Salinity", "Winter salinity", perm_import$variable)
perm_import$variable <- gsub("Winter Dissolved Oxygen", "Winter DO", perm_import$variable)
perm_import$variable <- gsub("Summer Temperature", "Summer temperature", perm_import$variable)
perm_import$variable <- gsub("BPI Fine", "Fine scale BPI", perm_import$variable)
perm_import$variable <- gsub("Winter Temperature", "Winter temperature", perm_import$variable)

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

# remove legend from p1
p1_nl <- p1 + theme(legend.position = "none")

# plot without legend - preferred by Stephanie
var_import <- plot_grid(p2, p1_nl, ncol = 1, labels = c("a","b"))
var_import

# plot with legend
#legend <- get_legend(p1)
#horiz <- plot_grid(p2, legend, rel_widths = c(3, 1), axis = "t")
#horiz
#var_import_top <- plot_grid(p1_nl, horiz, ncol = 1)
#var_import_top

ggsave("VariableImportance_Apr2025.png", var_import, path = figures_path, width = 8, height = 5, units = "in", dpi = 600, bg = "white")



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


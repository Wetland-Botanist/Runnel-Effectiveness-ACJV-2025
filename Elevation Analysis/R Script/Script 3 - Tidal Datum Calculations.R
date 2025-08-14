#Project: Trustees Salt Marsh Restoration - Elevation Analysis
#Script: Script 2 - Tidal Datum Calculations
#Author: Grant McKown (james.mckown@unh.edu)

#Script Description: The script imports two pieces of datasets from the Hydrology and Vegetation Analysis in order
# to calculate the difference in tidal datums to the elevations profiles of the 'impact' plots. 


# Chapter 1: Package library----------------------------------------------------

#Data Organization
library(tidyverse)
library(dplyr)
library(broom)

#Data visualization
library(ggplot2)
library(patchwork)
library(wesanderson)


# Chapter 2: Import the Hydrology and Vegetation Datasets--------------------------------------

#Task 1: Import the Tidal Datums from the Hydrology Dataset

# The tidal datums were calculated from the creek water level recorders at each site. The focus of 
# the script will be on Mean High Tide (MHT), Mean Higher High Tide (MHHT), and Max Observable Tide
# (Spring Tide). 


tidal_datums <- read.csv("Input Data\\Compiled Tidal Datums 2021 - 2024.csv") %>%
  filter(
      !is.na(SpringTide_NAVD88m)) %>%
  mutate(Site = ifelse(Sub_Site == "", Site,
                       paste(Site, Sub_Site, sep = " - "))) %>%
  dplyr::select(
       Site, State, Year, Tidal_Regime,
       MHT_NAVD88m, MHHT_NAVD88m, SpringTide_NAVD88m, Tidal_Range_m) %>%
       mutate(across(MHT_NAVD88m:Tidal_Range_m,
                ~as.numeric(.))) %>%
  filter(
          Year == 2021 |
           Year == 2022) %>%
  rename(
    MHT = MHT_NAVD88m,
    MHHT = MHHT_NAVD88m,
    Spring_Tide = SpringTide_NAVD88m,
    Tidal_Range = Tidal_Range_m)


glimpse(tidal_datums)


# Task 2: Import the Vegetation Dataset Summarized at Site - Treatment Level

# Plot elevations were manually entered into the vegetation dataset from the elevation surveys,
# filtered according to 'impact' plot status, and then averaged across transects for each
# site - treatment. 

elevs <- read.csv("Input Data\\Preliminary Data Site - Treatment Summary of Vegetation Plot Data.csv") %>%
  filter(Season == 2021 |
           Season == 2022) %>%
  filter(Site != "Plum Island - DPR - North" & Year_0 != 2019) %>%
  mutate(Site = ifelse(Sub_Site == "", Site,
                       paste(Site, Sub_Site, sep = " - ")),
         Site_Treatment = paste(Site, Overall_Treatment, sep = " - ")) %>%
  dplyr::select(
    Site, Site_Treatment, Site_Treatment, Season, Year_0, Time_Since_Restoration, 
    Overall_Treatment, Marsh_Condition, Elevation_mean) %>%
  rename(Year = Season)

glimpse(elevs)


# Chapter 3: Create the Elevation - Tidal Datum Data Set, Calculate Differences


# Task 1: Merge the two datasets together according to Site and Year

# The elevation and tidal datum datasets will be merged, where each treatment of a particular site
# will have the exact same tidal datums, since they all shared a single creek water level recorder


elevs_tides <- elevs %>%
  merge(tidal_datums, 
        by = c("Site", "Year"))

glimpse(elevs_tides)

# Task 2: Calculate the difference in the marsh platform from MHT, MHHT, and Spring Tide

elevs_tides_diff <- elevs_tides %>%
  mutate(MHT_diff = Elevation_mean - MHT,
         MHHT_diff = Elevation_mean - MHHT,
         Spring_Tide_diff = Elevation_mean - Spring_Tide)


glimpse(elevs_tides_diff)


# Task 2: Relativise the difference in tidal datums and marsh platform elevations by the tidal range

# Tidal range was gathered from the nearest NOAA Tidal Gauge (NAVD88m) in the Tidal Epoch (2001 - Present)

diff_range_relative <- elevs_tides_diff %>%
  mutate(MHT_rel = (MHT_diff / Tidal_Range) * 100,
         MHHT_rel = (MHHT_diff / Tidal_Range) * 100,
         Spring_Tide_rel = (Spring_Tide_diff / Tidal_Range) * 100   ) %>%
  mutate(across(
    MHT_rel:Spring_Tide_rel,
    ~round(., 2)))


glimpse(diff_range_relative)


write.csv(diff_range_relative,
          "Formatted Datasets\\Relativized Differences in Marsh Platform and Tidal Datums - 2021 - 2022.csv")



# Chapter 4: Visualize the difference in tidal datums from the marsh platform

# Task 1 - Visualize the Difference in Datums between Treatments and Tidal Ranges

diff_tidal_range <- diff_range_relative %>%
  mutate(Overall_Treatment = factor(Overall_Treatment,
                                    levels = c("No Action", "Reference", "Runnel"))) %>%
  dplyr::select(Site, Overall_Treatment, Tidal_Regime,
                MHT_rel, MHHT_rel, Spring_Tide_rel) %>%
  gather(MHT_rel:Spring_Tide_rel,
         key = Tidal_Metric,
         value = Tidal_Difference) %>%
  mutate(Tidal_Metric = factor(ifelse(Tidal_Metric == "MHT_rel", "Mean High Tide",
                                      ifelse(Tidal_Metric == "MHHT_rel", "Mean Higher High Tide",
                                             "Spring Tide")))) %>%
  filter(Tidal_Regime == "Microtidal")

diff_tidal_range$Overall_Treatment <- droplevels(diff_tidal_range$Overall_Treatment)


diff_viz <- ggplot(
  aes(x = Site),
      data = diff_tidal_range) + 
  geom_hline(yintercept = 0,
             linetype = "dashed", size = 1.5) + 
  geom_point(
    aes(y = Tidal_Difference,
        fill = Overall_Treatment),
    size = 7,
    stroke = 1.5, color = "black", shape = 21) + 
  scale_fill_manual(values = c("tan4", "yellow2", "darkgreen"), drop = FALSE) + 
  labs(y = "Platform Elevation - Tidal Datum Difference (%)", 
       x = "") + 
  theme_bw() +
  theme(
    legend.position = c(0.90, 0.20),
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text.x = element_text(size = 15, colour = "black", angle = 25, hjust = 1),
    axis.text.y = element_text(size = 15, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, colour = "black")) +
  facet_wrap(~Tidal_Metric,
             nrow = 3,
             scales = "free_y")

diff_viz


ggsave(diff_viz, 
       height = 14, width = 10, dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Microtidal Tidal Datum Relative Differences.jpg")


# Task 2 - Visualize the Relative Difference in Datums between Treatments and Tidal Ranges (By Tidal Range)

rel_tidal_range <- diff_range_relative %>%
  mutate(Overall_Treatment = factor(Overall_Treatment,
                                    levels = c("No Action", "Reference", "Runnel"))) %>%
  dplyr::select(Site, Overall_Treatment, Tidal_Regime, 
                MHT_rel, MHHT_rel) %>%
  gather(MHT_rel:MHHT_rel,
         key = Tidal_Metric,
         value = Tidal_Difference) %>%
  mutate(Tidal_Metric = factor(ifelse(Tidal_Metric == "MHHT_rel", "MHHT", 
                                      "MHT"))) %>%
  filter(Tidal_Regime == "Microtidal" & 
           Tidal_Metric == "MHT")




diff_viz <- ggplot(
  aes(x = Site),
  data = rel_tidal_range) + 
  geom_hline(yintercept = 0,
             linetype = "dashed", size = 1.5) + 
  geom_point(
    aes(y = Tidal_Difference,
        fill = Overall_Treatment),
    size = 7, position = position_jitter(0.20),
    stroke = 1.5, color = "black", shape = 21) + 
  scale_fill_manual(values = c("tan4", "yellow2", "darkgreen"), drop = FALSE) + 
 # scale_shape_manual(values = c(21, 22), drop = FALSE) + 
  labs(y = "Relative Difference of Platform and Tidal Datum (%)", 
       x = "") + 
  theme_bw() +
  theme(
    legend.position = c(0.90, 0.85),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle = 25, hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"))

diff_viz


ggsave(diff_viz, 
       height = 10, width = 14, dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Microtidal MHT - Platform Relative Differences.jpg")


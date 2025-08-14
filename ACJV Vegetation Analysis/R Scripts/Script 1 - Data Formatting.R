# Project: Atlantic Coast Joint Venture - Assessment of Runnel Effectiveness
# Analysis: Vegetation Analysis
# Script: Script 1 - Data Formatting

# Author: Grant McKown (james.mckown@unh.edu)

# Purpose:




# Chapter 1: Script Package Library ------------------------------------------

#Data Organization & Formatting
library(tidyverse)
library(stringr)

#Statistics and Data Analysis
library(ggfortify)

#Data Visualization
library(ggplot2)
library(viridis)


# Chapter 2: Import Plot Vegetation Dataset, Format Dataset ---------------------------


veg <- read.csv("Input Data\\ACJV Vegetation Plot Dataframe.csv") %>%
  filter(Site != "") %>%
  mutate(Site_SubSite = ifelse(Sub_Site != "", 
                       paste(Site, Sub_Site, sep = " - "),
                             Site),
         Site_Treatment = paste(Site_SubSite, Overall_Treatment, sep = " - ")) %>%
  filter(Site != "Plum Island - DPR" | Sub_Site != "North" | Overall_Treatment != "Runnel" | Transect != "T3")

glimpse(veg)


# Dataset Overview

veg_overview <- veg %>%
  summarise(
    Plots = n(),
    Sites = length(unique(Site_SubSite)),
    Years = length(unique(Year)),
    Site_Treatment = length(unique(Site_Treatment)),
    Age = length(unique(Time_Since_Restoration)),
    Age_Span = paste(min(Time_Since_Restoration), max(Time_Since_Restoration), sep = " - "))

glimpse(veg_overview)


# Add zeroes to all of the species data
# Removes all of the non-impact plots from the dataframe
# Calculate vegetation metrics

veg_format <- veg %>%
  mutate(across(MUD:UNK,
                 ~ifelse(is.na(.) == TRUE, 0, .))) %>%
  filter(Impacted == "Yes") %>%
  mutate(
    abiotic_cover = rowSums(across(c(MUD, DEAD, WRACK, ALGAL_MAT, GREEN_ALG, VAUCH, ASCO))), 
    live_cover = rowSums(across(SPALT:UNK)),
    high_marsh_grams = SPPAT + DISPI + AGSTO + JUGER + FERUB + PUMAR,
    salicornia = SADEP + SABIG, 
    other = rowSums(across(c(MUD:UNK, -MUD, -DEAD, -WRACK, -ALGAL_MAT, -GREEN_ALG, -VAUCH, -ASCO,
                             -SPPAT, -DISPI, -AGSTO, -JUGER, -FERUB, -PUMAR, -SPALT, -SADEP, -SABIG))),
    algae_cover = VAUCH + GREEN_ALG + ASCO + ALGAL_MAT,
    richness = rowSums(across(SPALT:UNK) > 0)) %>%
  mutate(
    Canopy_Height_cm = as.numeric(Canopy_Height_cm)) %>%
  dplyr::select(
    Site, Sub_Site, Site_SubSite, Overall_Treatment, Site_Treatment, Year, 
    Year_0, Time_Since_Restoration, Tidal_Regime, Marsh_Condition, Transect,
    Distance_m, Impacted, Elevation, Local_Tideshed_ID, Study_Tideshed_ID,
    MUD:Thatch_cm, abiotic_cover:richness)

glimpse(veg_format)

write.csv(veg_format,
          "Formatted Datasets\\Formatted Vegetation Plot Dataframe.csv")


# Recreate the dataset overview table for the formatted dataset after impact plots were removed

veg_overview_format <- veg_format %>%
  summarise(
    Plots = n(),
    Sites = length(unique(Site_SubSite)),
    Site_Treatment = length(unique(Site_Treatment)),
    Years = length(unique(Year)),
    Age = length(unique(Time_Since_Restoration)),
    Age_Span = paste(min(Time_Since_Restoration), max(Time_Since_Restoration), sep = " - "))

glimpse(veg_overview_format)



# Chapter 3: Data summary on formatted dataset by site - overall treatment -------------------

veg_site_treatment_summary <- veg_format %>%
  dplyr::select(Site, Sub_Site, Site_SubSite, Overall_Treatment, Site_Treatment, Year, 
                 Year_0, Time_Since_Restoration, Tidal_Regime, Marsh_Condition,
                Elevation, abiotic_cover, live_cover, 
                SPALT, high_marsh_grams, other, salicornia, 
                algae_cover, richness, Canopy_Height_cm) %>%
  group_by(Site, Sub_Site, Site_SubSite, Overall_Treatment, Site_Treatment, Year, 
           Year_0, Time_Since_Restoration, Tidal_Regime, Marsh_Condition) %>%
  summarise(Plot_count = n(),
            across(c(Elevation:Canopy_Height_cm),
                   list(
                     mean = ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE) / sqrt(n())))) %>%
  mutate(
    across(abiotic_cover_mean:Canopy_Height_cm_se, ~round(., 1)),
    across(c(Elevation_mean, Elevation_se),
           ~round(., 3))) 

glimpse(veg_site_treatment_summary)

write.csv(veg_site_treatment_summary,
          "Formatted Datasets\\Preliminary Data Summary of Vegetation Plot Data.csv")


veg_site_treatment_summary_reduced <- veg_site_treatment_summary %>%
  select( Site, Sub_Site, Site_SubSite, Overall_Treatment, Site_Treatment, Year, 
          Year_0, Time_Since_Restoration, Tidal_Regime,
         Elevation_mean, SPALT_mean, abiotic_cover_mean, live_cover_mean, high_marsh_grams_mean, 
         other_mean, salicornia_mean, algae_cover_mean, richness_mean, Canopy_Height_cm_mean)

write.csv(veg_site_treatment_summary_reduced,
          "Formatted Datasets\\Reduced Data Summary of Vegetation Plot Data.csv")



# Chapter 5: Data summary on formatted dataset by overall treatment -------------------------------

# The overall treatment summarized data is based on the averaged site - treatment, 
# since site - treatment is the replicate! 

veg_treatment_summary <- veg_site_treatment_summary %>%
  dplyr::select(Time_Since_Restoration,
                Overall_Treatment,
                abiotic_cover_mean, live_cover_mean, 
                SPALT_mean, high_marsh_grams_mean, other_mean, 
                algae_cover_mean, richness_mean, Canopy_Height_cm_mean) %>%
  rename_with(~str_remove(., "_mean")) %>%
  group_by(Time_Since_Restoration, Overall_Treatment) %>%
  summarise(
    Plot_count = n(),
    across(c(abiotic_cover:Canopy_Height_cm),
           list(
             mean = ~mean(., na.rm = TRUE),
             se = ~sd(., na.rm = TRUE) / sqrt(n())))) %>%
  mutate(
    across(abiotic_cover_mean:Canopy_Height_cm_se, ~round(., 1)))

glimpse(veg_treatment_summary)

write.csv(veg_treatment_summary,
          "Formatted Datasets\\Overall Treatment Summary By Site Treatment Summary Data.csv")


veg_treatment_summary_reduced <- veg_treatment_summary %>%
  select(Time_Since_Restoration, Overall_Treatment,
         SPALT_mean, abiotic_cover_mean, live_cover_mean, high_marsh_grams_mean, 
         other_mean, algae_cover_mean, richness_mean, Canopy_Height_cm_mean)

write.csv(veg_treatment_summary_reduced,
          "Formatted Datasets\\Reduced Overall Treatment Summary By Site Treatment Summary Data.csv")

# Chapter 6: Visualize the structure of the dataset

# Task 1: Graph the distribution of sites for each treatment and year

veg_site_histo <- veg_site_treatment_summary %>%
  group_by(Overall_Treatment, Time_Since_Restoration) %>%
  summarise(
    Site_Count = n()) %>%
  ungroup()

glimpse(veg_site_histo)


site_histogram <- ggplot(
  aes(x = Time_Since_Restoration, 
      y = Site_Count,
      group = Overall_Treatment), 
  data = veg_site_histo) + 
  geom_bar(aes(fill = Overall_Treatment),
    position = position_dodge(0.9), linewidth = 1.25,
    colour = "black", 
    stat = "identity") +
  geom_text(aes(label = Site_Count),
            vjust = -1,
            size = 6,
            fontface = "bold") +
  scale_x_continuous(limits = c(-0.5, 15), 
                     breaks = seq(0, 15, 2),
                     expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, 11), 
                     breaks = seq(0, 12, 2),
                     expand = c(0,0)) +
  labs(x = "Time Since Restoration (yrs)",
         y = "Number of Sites") + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  scale_colour_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Overall_Treatment,
             nrow = 3)

site_histogram



ggsave(site_histogram, 
       height = 8, width = 12, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Site Count Histogram.jpg")



# Task 2: Graph the change in vegetation community structure between treatments over time

veg_treatment_summary_viz_mean <- veg_treatment_summary %>%
  dplyr::select(Time_Since_Restoration, Overall_Treatment, 
                abiotic_cover_mean, SPALT_mean, high_marsh_grams_mean) %>%
  rename(Abiotic_Cover = abiotic_cover_mean,
         SPALT = SPALT_mean,
         High_Marsh_Grams = high_marsh_grams_mean) %>%
  gather(c(Abiotic_Cover, SPALT, High_Marsh_Grams),
         key = "Vegetation_Species",
         value = "Mean_Cover")

veg_treatment_summary_viz_se <- veg_treatment_summary %>%
  dplyr::select(Time_Since_Restoration, Overall_Treatment, 
                abiotic_cover_se, SPALT_se, high_marsh_grams_se) %>%
  rename(Abiotic_Cover = abiotic_cover_se,
         SPALT = SPALT_se,
         High_Marsh_Grams = high_marsh_grams_se) %>%
  gather(c(Abiotic_Cover, SPALT, High_Marsh_Grams),
         key = "Vegetation_Species",
         value = "SE_Cover")



veg_treatment_summary_viz <- veg_treatment_summary_viz_mean %>%
  merge(veg_treatment_summary_viz_se,
        by = c("Time_Since_Restoration", "Overall_Treatment", "Vegetation_Species")) %>%
  mutate(Vegetation_Species = ifelse(Vegetation_Species == "Abiotic_Cover", "Abiotic Cover",
                                    ifelse(Vegetation_Species == "SPALT", "Spartina alterniflora",
                                           "High Marsh Graminoids"))) %>%
  mutate(Vegetation_Species = factor(Vegetation_Species,
                                     levels = c("Spartina alterniflora", "High Marsh Graminoids", "Abiotic Cover")))


glimpse(veg_treatment_summary_viz)


rm(veg_treatment_summary_viz_mean, veg_treatment_summary_viz_se)

treatment_veg_time_viz <- ggplot(
  aes(x = Time_Since_Restoration, 
      y = Mean_Cover,
      group = Overall_Treatment), 
  data = veg_treatment_summary_viz) +
  geom_errorbar(
    aes(ymin = Mean_Cover - SE_Cover, ymax = Mean_Cover + SE_Cover,
        colour = Vegetation_Species),
    width = 0.25, linewidth = 1.25) +
  geom_point(aes(fill = Vegetation_Species),
             shape = 21, size = 6, stroke = 1.5) + 
  scale_x_continuous(limits = c(-0.5, 15), 
                     breaks = seq(0, 15, 2),
                     expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) +
  labs(x = "Time Since Restoration (yrs)",
       y = "Visual Cover (%)") + 
  scale_fill_manual(values = c("green4", "yellow2", "tan4")) + 
  scale_colour_manual(values = c("green4", "yellow2", "tan4")) + 
  theme_bw() +
  theme(
    legend.position = c(0.175, 0.95),
    legend.title = element_blank(),
    legend.text = element_text(colour = "black", size = 15),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Overall_Treatment,
             nrow = 3)

treatment_veg_time_viz



ggsave(treatment_veg_time_viz, 
       height = 14, width = 10, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Vegetation Cover Classes Over Time.jpg")



# Task 3: Graph the live cover over time 


live_veg_time_viz <- ggplot(
  aes(x = Time_Since_Restoration, 
      y = live_cover_mean,
      group = Overall_Treatment), 
  data = veg_treatment_summary) +
 geom_errorbar(
   aes(ymin = live_cover_mean - live_cover_se, 
        ymax = live_cover_mean + live_cover_se,
        colour = Overall_Treatment),
    width = 0.25, linewidth = 1.25) +
  geom_point(aes(fill = Overall_Treatment),
             shape = 21, size = 6, stroke = 1.5) + 
  scale_x_continuous(limits = c(-0.5, 15), 
                     breaks = seq(0, 15, 2),
                     expand = c(0,0)) + 
  scale_y_continuous(limits = c(-4, 102), 
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) +
  labs(x = "Time Since Restoration (yrs)",
       y = "Total Live Cover (%)") + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  scale_colour_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  theme_bw() +
  theme(
    legend.position = c(0.15, 0.15),
    legend.title = element_blank(),
    legend.text = element_text(colour = "black", size = 18),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black"))



live_veg_time_viz



ggsave(live_veg_time_viz, 
       height = 9, width = 14, 
       dpi = 300, limitsize = FALSE, units = "in",
       filename = "Output Figures\\Live Cover Cover by Treatment Over Time.jpg")


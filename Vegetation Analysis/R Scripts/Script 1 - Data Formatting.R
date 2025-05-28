# Project: Atlantic Coast Joint Venture - Assessment of Runnel Effectiveness
# Analysis: Vegetation Analysis
# Script: Script 1 - Data Formatting

# Author: Grant McKown (james.mckown@unh.edu)

# Purpose:




# Chapter 1: Script Package Library ------------------------------------------

#Data Organization & Formatting
library(tidyverse)

#Statistics and Data Analysis
library(ggfortify)

#Data Visualization
library(ggplot2)
library(viridis)


# Chapter 2: Import Plot Vegetation Dataset, Format Dataset ---------------------------


veg <- read.csv("Input Data\\ACJV Vegetation Plot Dataframe.csv") %>%
  filter(Site != "") %>%
  mutate(Site_Specific = ifelse(Sub_Site != "", 
                       paste(Site, Sub_Site, sep = " - "),
                             Site))

glimpse(veg)


# Dataset Overview

veg_overview <- veg %>%
  summarise(
    Plots = n(),
    Sites = length(unique(Site_Specific)),
    Estuary = length(unique(Salt_Marsh)),
    Seasons = length(unique(Season)),
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
    algae_cover = VAUCH + GREEN_ALG + ASCO + ALGAL_MAT,
    richness = rowSums(across(SPALT:UNK) > 0)) %>%
  mutate(
    Canopyo_Height_cm = as.numeric(Canopyo_Height_cm),
    Thatch_cm = as.numeric(Thatch_cm))

glimpse(veg_format)

write.csv(veg_format,
          "Formatted Datasets\\Formatted Vegetation Plot Dataframe.csv")



# Chapter 3: Data summary on formatted dataset by site - overall treatment -------------------


veg_treatment_summary <- veg_format %>%
  group_by(Season, Year_0, Time_Since_Restoration, Salt_Marsh, 
           Site, State, Overall_Treatment,
           Marsh_Condition, Tidal_Regime) %>%
  summarise(Plot_count = n(),
            across(c(Elevation, MUD:richness),
                   list(
                     mean = ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE) / sqrt(n())))) %>%
  mutate(
    across(MUD_mean:richness_se, ~round(., 1)),
    across(c(Elevation_mean, Elevation_se),
           ~round(., 3))) 

glimpse(veg_treatment_summary)

write.csv(veg_treatment_summary,
          "Formatted Datasets\\Preliminary Data Summary of Vegetation Plot Data.csv")


veg_treatment_summary_reduced <- veg_treatment_summary %>%
  select(Season, Year_0, Time_Since_Restoration,
         Site, State, Overall_Treatment, Tidal_Regime, Marsh_Condition, 
         Elevation_mean, SPALT_mean, abiotic_cover_mean, live_cover_mean, high_marsh_grams_mean, 
         algae_cover_mean, richness_mean, Canopyo_Height_cm_mean, Thatch_cm_mean)

write.csv(veg_treatment_summary_reduced,
          "Formatted Datasets\\Reduced Data Summary of Vegetation Plot Data.csv")





# Chapter 4: Data summary on formatted dataset by transect ----------------------------


# Count the number of plots in each type of treatment in each site

veg_transect_summary <- veg_format %>%
  group_by(Season, Year_0, Time_Since_Restoration, Salt_Marsh, 
           Site, State, Overall_Treatment, Tidal_Regime, Marsh_Condition, Transect) %>%
  summarise(Plot_count = n(),
            across(c(Elevation, MUD:richness),
                   list(
                     mean = ~mean(., na.rm = TRUE),
                   se = ~sd(., na.rm = TRUE) / sqrt(n())))) %>%
  mutate(
    across(MUD_mean:richness_se, ~round(., 1)),
    across(c(Elevation_mean, Elevation_se),
                       ~round(., 3))) 

glimpse(veg_transect_summary)

write.csv(veg_transect_summary,
          "Formatted Datasets\\Preliminary Data Transect Summary of Vegetation Plot Data.csv")


veg_transect_summary_reduced <- veg_transect_summary %>%
  select(Season, Year_0, Time_Since_Restoration,
         Site, State, Overall_Treatment, Tidal_Regime, Marsh_Condition, 
         Elevation_mean, SPALT_mean, abiotic_cover_mean, live_cover_mean, high_marsh_grams_mean, 
         algae_cover_mean, richness_mean, Canopyo_Height_cm_mean, Thatch_cm_mean)

write.csv(veg_transect_summary_reduced,
          "Formatted Datasets\\Reduced Data Transect Summary of Vegetation Plot Data.csv")





# Chapter 5: Data summary on formatted dataset by site - individual treatments -------------------------------


veg_site_treatment_summary <- veg_format %>%
  group_by(Season, Year_0, Time_Since_Restoration, Salt_Marsh,
           Site, State, Sub_Site, Site_Treatment, Tidal_Regime, Marsh_Condition, Overall_Treatment) %>%
  summarise(Plot_count = n(),
            across(c(Elevation, MUD:richness),
                   list(
                     mean = ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE) / sqrt(n())))) %>%
  mutate(
    across(MUD_mean:richness_se, ~round(., 1)),
    across(c(Elevation_mean, Elevation_se),
           ~round(., 3))) 

glimpse(veg_site_treatment_summary)

write.csv(veg_site_treatment_summary,
          "Formatted Datasets\\Preliminary Data Site - Treatment Summary of Vegetation Plot Data.csv")


veg_site_treatment_summary_reduced <- veg_site_treatment_summary %>%
  select(Season, Year_0, Time_Since_Restoration,
         Site, Sub_Site, State, Overall_Treatment, Tidal_Regime, Marsh_Condition, 
         Elevation_mean, SPALT_mean, abiotic_cover_mean, live_cover_mean, high_marsh_grams_mean, 
         algae_cover_mean, richness_mean, Canopyo_Height_cm_mean, Thatch_cm_mean)

write.csv(veg_site_treatment_summary_reduced,
          "Formatted Datasets\\Reduced Data Site - Treatment Summary of Vegetation Plot Data.csv")


# Chapter 6: Visualize the structure of the dataset

# Task 1: Graph the distribution of sites for each treatment and year

veg_site_histo <- veg_site_treatment_summary_reduced %>%
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
  scale_y_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, 3),
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

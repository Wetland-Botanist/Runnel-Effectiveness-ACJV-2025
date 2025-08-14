#Project: Trustees Salt Marsh Restoration - Elevation Analysis
#Script: Script 3 - Summary of Tidal Datum Elevations
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


# Chapter 2: Import the tidal datum elevation dataset -------------------------

# Task 1: Import the Dataset
elevs <- read.csv("Formatted Datasets\\Relativized Differences in Marsh Platform and Tidal Datums - 2021 - 2022.csv") %>%
  select(-X)

glimpse(elevs)

# Chapter 5: Calculate descriptive statistics for various groups

# First, we should calculate the descriptive statistics for the following groups, merge
# the data tables, and export for reporting: Tidal Regime (Runnel Only), Treatment, and Marsh Condition (Runnel Only)


# Task 1: Calculate the Descriptive Statistics for Overall Treatment and Tidal Regime

elevs_treatment_tides <- elevs %>%
  group_by(Overall_Treatment, Tidal_Regime) %>%
  summarise(across(MHT_rel:Spring_Tide_rel,
                   list(
                     mean = ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE) / sqrt(n())   ))) %>%
  mutate(across(MHT_rel_mean:Spring_Tide_rel_se,
                ~round(., 2)))

glimpse(elevs_treatment_tides)

elevs_treatment_tides_format <- elevs_treatment_tides %>%
  mutate(MHT_rel = paste(MHT_rel_mean, MHT_rel_se, sep = " +/- "), 
         MHHT_rel = paste(MHHT_rel_mean, MHHT_rel_se, sep = " +/- "), 
         Spring_Tide_rel = paste(Spring_Tide_rel_mean, Spring_Tide_rel_se, sep = " +/- ")) %>%
  select(-c(MHT_rel_mean:Spring_Tide_rel_se))

glimpse(elevs_treatment_tides_format)


# Task 3: Calculate the Descriptive Statistics for Marsh Condition and Tidal Regime (Runnel Only)

elevs_condition_tides <- elevs %>%
  filter(Overall_Treatment == "Runnel") %>%
  group_by(Marsh_Condition, Tidal_Regime) %>%
  summarise(across(MHT_rel:Spring_Tide_rel,
                   list(
                     mean = ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE) / sqrt(n())   ))) %>%
  mutate(across(MHT_rel_mean:Spring_Tide_rel_se,
                ~round(., 2)))

glimpse(elevs_condition_tides)

elevs_condition_tides_format <- elevs_condition_tides %>%
  mutate(MHT_rel = paste(MHT_rel_mean, MHT_rel_se, sep = " +/- "), 
         MHHT_rel = paste(MHHT_rel_mean, MHHT_rel_se, sep = " +/- "), 
         Spring_Tide_rel = paste(Spring_Tide_rel_mean, Spring_Tide_rel_se, sep = " +/- ")) %>%
  select(-c(MHT_rel_mean:Spring_Tide_rel_se))

glimpse(elevs_condition_tides_format)



# Chapter 3: Visualize the Differences ----------------------------------------------

# Task 1: Visualize the differences in relative difference in tidal datums by Treatment and Regime

elevs_treatment_tides_viz <- elevs_treatment_tides_format %>%
  gather(MHT_rel:Spring_Tide_rel, 
         key = Tidal_Datum,
         value = Tidal_Range_Percent) %>%
  mutate(Mean = as.numeric( str_extract(Tidal_Range_Percent, ".*(?= \\+/-)")),
         SE = as.numeric(str_extract(Tidal_Range_Percent, "(?<= \\+/- ).*")),
         Tidal_Datum = str_remove(Tidal_Datum, "_rel"),
         Tidal_Datum = ifelse(Tidal_Datum == "Spring_Tide", "Spring Tide", Tidal_Datum),
         Tidal_Datum = factor(Tidal_Datum,
                              levels = c("MHT", "MHHT", "Spring Tide"))) %>%
  select(-Tidal_Range_Percent) %>%
  ungroup()


glimpse(elevs_treatment_tides_viz) 




elevs_treatment_tides_graph <- ggplot(
  aes(x = Tidal_Datum, 
      y = Mean,
      group = Overall_Treatment), 
  data = elevs_treatment_tides_viz) + 
  geom_errorbar(aes(
    ymin = Mean - SE,
    ymax = Mean + SE), 
    colour = "black", 
    position = position_dodge(width = 0.750),
    width = 0.5, linewidth = 1.1) + 
  geom_point(
    aes(fill = Overall_Treatment), 
    size = 7, position = position_dodge(width = 0.750),
    stroke = 1.5, color = "black", shape = 21) + 
  labs(y = "Relative Difference of Platform and Tidal Datum (%)", 
       x = element_blank()) + 
  scale_fill_manual(values = c("tan4", "yellow2", "darkgreen"), drop = FALSE) + 
  theme_bw() +
  theme(
    legend.position = c(0.90, 0.85),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 25, colour = "black"), 
    axis.title = element_text(size = 25, colour = "black"),
    axis.text = element_text(size = 25, colour = "black")) + 
  facet_wrap(~Tidal_Regime)

elevs_treatment_tides_graph


ggsave(elevs_treatment_tides_graph, 
       height = 8, width = 16, dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Meso and Micro - Differences by Treatment.jpg")





# Task 2s: Visualize the differences in relative difference in tidal datums by Tidal Regime and PreCondition

elevs_condition_tides_viz <- elevs_condition_tides_format %>%
  gather(MHT_rel:Spring_Tide_rel, 
         key = Tidal_Datum,
         value = Tidal_Range_Percent) %>%
  mutate(Mean = as.numeric( str_extract(Tidal_Range_Percent, ".*(?= \\+/-)")),
         SE = as.numeric(str_extract(Tidal_Range_Percent, "(?<= \\+/- ).*")),
         Tidal_Datum = str_remove(Tidal_Datum, "_rel"),
         Tidal_Datum = ifelse(Tidal_Datum == "Spring_Tide", "Spring Tide", Tidal_Datum),
         Tidal_Datum = factor(Tidal_Datum,
                              levels = c("MHT", "MHHT", "Spring Tide"))) %>%
  select(-Tidal_Range_Percent) %>%
  ungroup()


glimpse(elevs_condition_tides_viz) 




elevs_condition_tides_graph <- ggplot(
  aes(x = Tidal_Datum, 
      y = Mean,
      group = Marsh_Condition), 
  data = elevs_condition_tides_viz) + 
  geom_errorbar(aes(
    ymin = Mean - SE,
    ymax = Mean + SE), 
    colour = "black", 
    position = position_dodge(width = 0.750),
    width = 0.5, linewidth = 1.1) + 
  geom_point(
    aes(fill = Marsh_Condition), 
    size = 7, position = position_dodge(width = 0.750),
    stroke = 1.5, color = "black", shape = 21) + 
  labs(y = "Relative Difference of Platform and Tidal Datum (%)", 
       x = element_blank()) + 
  scale_fill_manual(values = c("orange", "darkgreen")) +
  theme_bw() +
  theme(
    legend.position = c(0.90, 0.85),
    legend.title = element_blank(),
    legend.text = element_text(size = 25, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 25, colour = "black"), 
    axis.title = element_text(size = 25, colour = "black"),
    axis.text = element_text(size = 25, colour = "black")) + 
  facet_wrap(~Tidal_Regime)

elevs_condition_tides_graph


ggsave(elevs_condition_tides_graph, 
       height = 8, width = 16, dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Meso and Micro - Differences by Marsh Condition.jpg")
















#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 5 - Visualizing Compiled Flooding Statistics Over Time

#Authors: Grant McKown (james.mckown@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire


# Chapter 1: Library of packages for necessary work -------------

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(stringr)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(ggplot2)
library(scales)


#Chapter 2: Import the Compiled Flooding Statistics Dataset -------------

Site_Name <- "Kents Island"

# Flooding Statistics dataset is compiled in Microsoft Excel after running all water level recorders of 
# a Trustees site for all monitoring seasons. The dataset is composed of water level recorder, overall treatment, and 
# the calculated metrics of flooding duration, high tide frequency, and drainage depth for marsh platform and root zone
# elevation. 


flooding_stats <- read.csv("Input Data\\Trustees Flooding Statistics.csv") %>%
  filter(Marsh == Site_Name)

glimpse(flooding_stats)


# Chapter 3: Flooding Duration Visualization ------------

#Task 1: Format the flooding statistics dataset for the flooding duration graph

# The main transformation of the dataset is going from a wide --> long dataset

flood_dur_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Season, Platform_Flood_Dur, Root_Flood_Dur) %>%
  gather(Platform_Flood_Dur:Root_Flood_Dur, 
         key = "Zone",
         value = "Flood_Duration") %>%
  mutate(Zone = ifelse(Zone == "Platform_Flood_Dur", "Marsh Platform", "Root Zone")) %>%
  filter(!str_detect(Treatment_Name, "Runnel"))

# Task 2: Graph the flooding statistics for individual treatment 

flood_dur_viz <- ggplot(
  data = flood_dur_format,
  aes(x = Season,
      y = Flood_Duration)) +
  geom_point(aes(fill = Treatment_Name),
    shape = 21, 
    size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "Flooding Duration Time (%)") +
  scale_y_continuous(
    limits = c(0, 102),
    breaks = seq(0, 100, 20)) + 
    theme_bw() + 
    theme(
      legend.position = c(0.125, 0.925),
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 16, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title = element_text(size = 16, colour = "black"),
      axis.text = element_text(size = 16, colour = "black"), 
      strip.background = element_blank(),
      strip.text = element_text(size = 16)) +
    facet_wrap(~Zone,
               nrow = 2)

flood_dur_viz



ggsave(flood_dur_viz,
       filename = paste("Figures\\", Site_Name,"Non-Runnel Flooding Duration Graph.jpg", sep = " "), 
       height = 10, width = 12, 
       units = "in", dpi = 300)



# Chapter 4: High Tide Frequency Visualization ---------

ht_freq_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Season, Platform_HT_Freq, Root_HT_Freq) %>%
  gather(Platform_HT_Freq:Root_HT_Freq, 
         key = "Zone",
         value = "High_Tide_Freq") %>%
  mutate(Zone = ifelse(Zone == "Platform_HT_Freq", "Marsh Platform", "Root Zone"))%>%
  filter(!str_detect(Treatment_Name, "Runnel"))


ht_freq_viz <- ggplot(
  data = ht_freq_format,
  aes(x = Season,
      y = High_Tide_Freq)) +
  geom_point(aes(fill = Treatment_Name),
             shape = 21, 
             size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "High Tide Flooding Frequency (%)") +
  scale_y_continuous(
    limits = c(0, 102),
    breaks = seq(0, 100, 20)) + 
  theme_bw() + 
  theme(
    legend.position = c(0.125, 0.15),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 16)) +
  facet_wrap(~Zone,
             nrow = 2)

ht_freq_viz



ggsave(ht_freq_viz,
       filename = paste("Figures\\", Site_Name," Non-runnel High Tide Frequency Graph.jpg", sep = " "), 
       height = 10, width = 12, 
       units = "in", dpi = 300)



#Chapter 5: Visualize the Drainage Depth ----------

drain_depth_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Season, Platform_Drain_Depth, Root_Drain_Depth) %>%
  gather(Platform_Drain_Depth:Root_Drain_Depth, 
         key = "Zone",
         value = "Drain_Depth") %>%
  mutate(Zone = ifelse(Zone == "Platform_Drain_Depth", "Marsh Platform", "Root Zone")) %>%
  mutate(Drain_Depth = Drain_Depth * -1) %>%
  filter(Treatment_Name != "No Action (ACJV)") %>%
  filter(!str_detect(Treatment_Name, "Runnel")) %>%
  filter(Zone == "Marsh Platform")


drain_depth_viz <- ggplot(
  data = drain_depth_format,
  aes(x = Season,
      y = Drain_Depth)) +
    geom_hline(yintercept = 0, linetype = "dashed", 
             color = "black", size = 1.25) +
  geom_point(aes(fill = Treatment_Name),
             shape = 21, 
             size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "Low Tide Drainage Depth (cm)") +
  scale_y_continuous(
    limits = c(-20, 10),
    breaks = seq(-30, 20, 5)) + 
  theme_bw() + 
  theme(
    legend.position = c(0.10, 0.90),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 16)) +
  facet_wrap(~Zone,
             nrow = 2)

drain_depth_viz


ggsave(drain_depth_viz,
       filename = paste("Figures\\", Site_Name," Non-Runnel Drainage Depth Graph.jpg", sep = " "), 
       height = 8, width = 12, 
       units = "in", dpi = 300)



#Chapter 6: Visualize the Drainage Amplitude ----------

drain_amplitude_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Season, Drainage_Amplitude) %>%
  filter(Treatment_Name != "No Action (ACJV)")


drain_amplitude_viz <- ggplot(
  data = drain_amplitude_format,
  aes(x = Season,
      y = Drainage_Amplitude)) +
  geom_point(aes(fill = Treatment_Name),
             shape = 21, 
             size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "Drainage Amplitude (cm)") +
  scale_y_continuous(
    limits = c(-0.5, 25),
    breaks = seq(0, 25, 5)) + 
  theme_bw() + 
  theme(
    legend.position = c(0.125, 0.85),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 16))

drain_amplitude_viz


ggsave(drain_amplitude_viz,
       filename = paste("Figures\\", Site_Name,"Drainage Amplitude Graph.jpg", sep = " "), 
       height = 10, width = 14, 
       units = "in", dpi = 300)

#Project: Trustees Salt Marsh Restoration - Elevation Analysis
#Script: Script 2 - Analysis of Ditch Center Elevations
#Author: Grant McKown (james.mckown@unh.edu)

#Script Description: The script visualizes the first and last season transect profiles for each treatment

#---------------------------------------------------------
# Chapter 1: Package library
#---------------------------------------------------------

#Data Organization
library(tidyverse)
library(dplyr)

#Data visualization
library(ggplot2)
library(patchwork)
library(wesanderson)
library(ggforce)


#------------------------------------------------------
# Chapter 2: Import the Corrected Elevation Dataset
#----------------------------------------------------

Site_Name <- "Broad Cove"

elevs <- read.csv(paste("Formatted Datasets\\", Site_Name, " Corrected Elevations.csv", sep = "")) %>%
                    select(-X) %>%
                    select(Season, Salt_Marsh, SubSite, Site_Treatment, Overall_Treatment, 
                           Transect, Distance_m, elev_corrected)
 # mutate(Transect = paste("Transect ", Transect, sep = ""))

glimpse(elevs)


#---------------------------------------------------
# Chapter 3: Visualize the Elevation Profiles
#---------------------------------------------------

Treatment_Name <- "Reference"

elevs_viz <- elevs %>%
  filter(Site_Treatment == Treatment_Name, 
         Season == min(Season) | Season == max(Season)) %>%
  mutate(Distance_m = ifelse(Season == max(Season),
                             Distance_m + 1.0, Distance_m)) %>%
  mutate(Season = as.character(Season))



elevations_profiles <- ggplot(data = elevs_viz,
                          aes(x = Distance_m, 
                              y = elev_corrected,
                              group = Season)) +
  geom_line(aes(color = Season),
            linewidth = 1.25) + 
  scale_color_manual(values = c("black", "orange")) + 
  scale_y_continuous(limits = c(round(min(elevs_viz$elev_corrected) - 0.2, 1), 
                                round(max(elevs_viz$elev_corrected) + 0.2, 1)),
                     breaks = seq(round(min(elevs_viz$elev_corrected) - 0.2, 1),
                                    round(max(elevs_viz$elev_corrected) + 0.2, 1), 
                                    0.20),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(-2, 
                                round(max(elevs_viz$Distance_m, 0)) + 10),
                     breaks = seq(0, 60, 10),
                     expand = c(0,0)) + 
  labs(y = "Elevation (NAVD88 m)",
       x = "Distance along Transect (m)") +
  theme_bw() +
  theme(
    legend.position = c(0.925, 0.075),
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black",),
    strip.background = element_blank(),
    strip.text = element_text(size = 18)) +
  facet_wrap_paginate(~Transect, 
                      ncol = 1, nrow = 2)

elevations_profiles

ggsave(elevations_profiles, 
       height = 10, width = 14, dpi = 300,
       limitsize = FALSE, units = "in",
       filename = paste("Output Figures\\", Site_Name," ", Treatment_Name, " Elevation Profile.jpg", sep = ""))
       
       
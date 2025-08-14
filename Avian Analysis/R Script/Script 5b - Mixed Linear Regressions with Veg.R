#Project: Avian Community Analysis of SHARP Point Counts of ACJV Project
#Script: Part V - Mixed Linear Regressions of Vegetation Metrics
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 12th, 2024
#Date Last Edited: March 22nd, 2024


# Purpose: Conduct Mixed Linear Two-WAY ANCOVAs on the avian community metrics, Graph the results


# Chapter 1: Package Library ------------------------------------------------
rm(list = ls())

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(vegan)
library(broom)
library(lme4)
library(afex)
library(ggeffects)
library(stringr)
library(MuMIn)

#Graphing Packages
library(ggplot2)
library(patchwork)
library(ggrepel)
library(ggResidpanel)




#Chapter 2: Import the 0 - 50 m SHARP Point Count dataset -----------------
# Dataset is formatted to remove feeding habits and individual species
# Only SHARP points of 'runnel' treatment are retained for the regressions

#Upon exploratory analysis of the data over runnel age, it seemed that there might be a different trajectory
# for sites in the North Shore of Massachusetts (north of Cape Cod) and in the Narragansett Bay (south of Cape Cod)

#When importing the bird dataset, we will do the following:
# Remove the Moody Marsh Site (not tied to either Mass or Narragansett Bay),
# Change the state of Broad Cove to 'Rhode Island' since it is in the Narragansett Bay Estuary system
# Add a 'Region' classifier that divides the sites between North Shore Mass and Narragansett Bay

birds <- read.csv("Formatted Datasets\\SHARP Bird 50m Dataset - Surveys Averaged.csv") %>%
  select(-X, -c(Aerial:Wading), -c(AGWT:YEWA)) %>%
  mutate(State = ifelse(str_starts("Broad Cove", Site), "RI", State)) %>%
  mutate(Tidal_Regime = ifelse(State == "RI", "Microtidal", 
                         ifelse (State == "MA", "Mesotidal", "Mesotidal"))) %>%
  filter(Year == 2023) 


# Task 2: Import the SHARP vegetation dataset 

veg <- read.csv("Input Data\\SHARP_Veg_2023_HabCovers.csv") %>%
  mutate(across(low_marsh:SCMAR,
                ~. * 100),
         high_marsh_grams = SPPAT + JUGER + DISPI)

glimpse(veg)

# Task 3: Combine the Bird and Vegetation Dataset

birds_veg <- birds %>%
  merge(select(veg, 
               c(PointID, high_marsh_grams, open_water, SPALT_short)),
        by = "PointID")

#Chapter 3: Mixed Linear Regressions of Avian Community Metrics over Runnel Age

#Analysis Notes:
# Metrics to be tested: Salt Marsh Sparrows, Total Bird Abundance, Wetland Score, Feeding Habit, Richness, Shannon Diversity

#The fixed effects for the model are: Runnel Age, Region, and their interaction
# The random effects for the model are: Nested design of Site and SHARP Point
# If square-root transformations are conducted and significant regression is found, 
  # the predicted points of the regression and confidence intervals will be back transformed

# After each analysis, the regression is graphed to see the immediate result

#Analysis 1: Total Bird Abundance

#Note - square-root transformation needed to improve QQ Plot and normality

hm_sparrow <- lm(saltysparrow ~ high_marsh_grams,
               data = birds_veg)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(hm_sparrow)

#Returning the ANOVA table for the model
hm_anova <- tidy(anova(hm_sparrow)) %>%
  mutate(metric = 'high_marsh')

hm_anova


# Graph the linear regression (feel free to remove hashtag to see individual site labels)

high_marsh_graph <- ggplot(birds_veg,
                      aes(x = high_marsh_grams, y = saltysparrow)) +
  geom_point(aes(fill = Treatment),
             pch = 21, 
             size = 6,
             alpha = 0.75) + 
  scale_y_continuous(limits = c(-1, 20.5),
                     breaks = seq(0, 20, 5),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-1, 101),
                     breaks = seq(0, 101, 20),
                     expand = c(0,0)) + 
  labs(y = "Sparrow Abundance",
       x = "High Marsh Cover (%)") +
  theme_bw() +
  theme(
    legend.text = element_text(size = 18, colour = "black"),
    legend.title = element_blank(),
    legend.position = "blank",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

high_marsh_graph

ggsave(high_marsh_graph,
       filename = "Figures\\Sparrow Abundance Over High Marsh Grams.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 8, width = 12)



#Analysis 2: Short-form Spartina alterniflora

#Note - square-root transformation needed to improve QQ Plot and normality

spalt_sparrow <- lm(saltysparrow ~ SPALT_short,
                 data = birds_veg)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(spalt_sparrow)

#Returning the ANOVA table for the model
spalt_anova <- tidy(anova(spalt_sparrow)) %>%
  mutate(metric = 'Short SPALT')

spalt_anova


# Graph the linear regression (feel free to remove hashtag to see individual site labels)

spalt_graph <- ggplot(birds_veg,
                           aes(x = SPALT_short, y = saltysparrow)) +
  geom_point(aes(fill = Treatment),
             pch = 21, 
             size = 6,
             alpha = 0.75) + 
  scale_y_continuous(limits = c(-1, 20.5),
                     breaks = seq(0, 20, 5),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-1, 101),
                     breaks = seq(0, 101, 20),
                     expand = c(0,0)) + 
  labs(y = "Sparrow Abundance",
       x = "Short Spartina alterniflora Cover (%)") +
  theme_bw() +
  theme(
    legend.text = element_text(size = 18, colour = "black"),
    legend.title = element_blank(),
    legend.position = "blank",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

spalt_graph

ggsave(spalt_graph,
       filename = "Figures\\Sparrow Abundance Over Short SPALT.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 8, width = 12)






#Analysis 3: Open Water

water_sparrow <- lm(saltysparrow ~ open_water,
                    data = birds_veg)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(spalt_sparrow)

#Returning the ANOVA table for the model
water_anova <- tidy(anova(water_sparrow)) %>%
  mutate(metric = 'Open WAter')

water_anova


# Graph the linear regression (feel free to remove hashtag to see individual site labels)

water_graph <- ggplot(birds_veg,
                      aes(x = open_water, y = saltysparrow)) +
  geom_point(aes(fill = Treatment),
             pch = 21, 
             size = 6,
             alpha = 0.75) + 
  scale_y_continuous(limits = c(-1, 20.5),
                     breaks = seq(0, 20, 5),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-1, 101),
                     breaks = seq(0, 101, 20),
                     expand = c(0,0)) + 
  labs(y = "Sparrow Abundance",
       x = "Open Water Cover (%)") +
  theme_bw() +
  theme(
    legend.text = element_text(size = 18, colour = "black"),
    legend.title = element_blank(),
    legend.position = "blank",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

water_graph

ggsave(water_graph,
       filename = "Figures\\Sparrow Abundance Over Open Water.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 8, width = 12)



# Task 4: Compile and export the anova tables

compiled_anova <- rbind(hm_anova, spalt_anova, water_anova)

write.csv(compiled_anova, 
          "Output Stats\\Compiled Sparrow Regression ANOVAs.csv")

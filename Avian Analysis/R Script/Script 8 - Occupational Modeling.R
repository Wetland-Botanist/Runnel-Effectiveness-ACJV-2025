#Project: Atlantic Coast Joint Venture - SHARP Analysis
#Title:  Prediction, Occupancy, and Abundance Modelling with 'unmarked' package
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: March 4th, 2024



#Purpose: 

# (1) Model the prediction, occupancy, and abundance probability of the salt marsh sparrow and 
#    salty sparrows across the entire dataset
# (2) Model the occupancy and abundance probability of the salt marsh sparrow and salty sparrows
#     across vegetation metrics using the 2023 SHARP datasets

rm(list = ls())

# Chapter 1: Set up Code

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(vegan)
library(broom)
library(ggeffects)
library(stringr)
library(lubridate)

#Graphing Packages
library(ggplot2)
library(patchwork)

#Modeling Packages
library(unmarked)
library(auk)
library(MuMIn)

#Chapter 2: Import and format the 50 m SHARP Dataset & 2023 SHARP Vegetation Dataset

# Formatting will include:
# Removal of unwanted sites (Ipswich RUN West, Moody Marsh), Note - Broad Cove NAC is not in the 2023 dataset
# Filter birding dataset to 2023 monitoring
# Creating a 'time elapsed' variable from the Site Date column
# Selecting only the bird, site, and observation variables


#Note, the dataset used for occupational modeling is the 50 m SHARP with BOTH site visits in a single season

sparrows <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird 50 m Distance.csv") %>%
  select(-X) %>%
  filter(Year != 2023) %>%
  filter(PointID != "Ipswich RUN West") %>%
  filter(PointID != "Broad Cove" | Treatment != "No Action") %>%
  filter(Site != "Moody Marsh") %>%
  mutate(State = ifelse(str_starts("Broad Cove", Site), "RI", State)) %>%
  mutate(Region = ifelse(State == "RI", "Narragansett Bay", 
                         ifelse (State == "MA", "North Shore Mass", "North Shore Mass"))) %>%
  mutate(survey_hour= hour(as.POSIXct(SurveyTime, format = '%I:%M')),
         survey_min = minute(as.POSIXct(SurveyTime, format = '%I:%M')) / 60,
         survey_elapsed = round(survey_hour + survey_min, 2)) %>%
select(PointID, Year, VisitNum, State, Site, Region, Treatment, 
         SurveyWindow, survey_elapsed, TempF:Noise, saltysparrow)

glimpse(sparrows)

#Import the vegetation dataset

veg <- read.csv("Avian Analysis\\Input Data\\SHARP_Veg_2023_HabCovers.csv")

#Merge the Birds and Vegetation Dataset together

# Merging the two datasets by the PointID identifier
# Changing saltysparrow variable from abundance to absence/presence

sparrows_veg <- sparrows %>%
  merge(select(veg, PointID, low_marsh:SCMAR), by = 'PointID') %>%
  select(PointID:saltysparrow, 
         low_marsh, high_marsh, pannes, open_water,
         SPALT_short, SPPAT, DISPI)%>%
  mutate(saltysparrow = ifelse(saltysparrow > 0, 1, 0))


#Chapter 3: Formatting the dataset into the Occu Dataframe Format for Single Season Occupancy Model

# auk package from the Cornell Lab of Ornithology created a neat function that immediately creates
# the proper dataframe. The dataframe contains: 

# (1) site_id --> PointID (site identifier to gather and spread the data)
# (2) response --> saltysparrow (absence/presence of endemic salt marsh sparrows)
# (3) site_covs --> vegetation data (site characteristics that might impact sparrow occupancy)
# (4) obs_covs --> visit data (unique visit characteristics that might impact sparrow detection by observers)


sparrow_occu <- format_unmarked_occu(sparrows_veg,
                                     site_id = 'PointID',
                                     response = 'saltysparrow',
                                     site_covs = c('low_marsh', 'high_marsh', 'pannes', 'open_water',
                                                   'SPALT_short', 'SPPAT', 'DISPI',
                                                   'Treatment', 'Region'),
                                     obs_cov = c('SurveyWindow', 'survey_elapsed', 'TempF', 'Sky', 'WindSp', 'Noise'))

sparrow_occu_data <- formatWide(sparrow_occu, type ='unmarkedFrameOccu')


#Chapter 4: Single Season Occupancy Modeling

# Single Season Occupancy will be broken down into:

# (1) Calculation of overall detection and occupancy probability for the study in 2023
# (2) Occupancy probability modeling for marsh habitats (low marsh, high marsh, pannes, and open water)
# (3) Occupancy probability modeling for marsh vegetation (Short form SPALT, S. patens, and Distichlis spicata)


#Task 1 - Calculation of overall detection and occupancy probability for 2023


occu_model <- occu(formula = ~1
                            ~ 1,
                   data = sparrow_occu_data)

summary(occu_model)

#Back-calculate the Real estimate of occupancy with 95% CI

predict(occu_model,
        newdata = data.frame(site = 1),
        type = 'state')

# Occupancy probability of 77% with a 95% CI of 64% - 87%

# Back-calculate the real estimate of detection with 95% CI

predict(occu_model,
        newdata = data.frame(site = 1),
        type = 'det')

# Detection probability of 68% with a 95% CI of 75% - 93%


# Task 2 - Model selection for observation covariates

#Before proceeding to the marsh habitats and vegetation occupation models, we need to select the proper 
# survey covariates to include in the model. Using the dredge() function of the MuMin package, we will
# test all model combinations of the survey covariates

#Step 1: Select the Detection Model with the lowest AIC Value 
# Due to previous research and requirements of SHARP monitoring, the following will be included
# in the detection model: Noise, Temperature, Wind speed, Cloud Cover, and Survey Window

survey_detection <- occu(formula = ~ Noise + WindSp + TempF + SurveyWindow + survey_elapsed + Sky
                    ~ 1,
                  data = sparrow_occu_data)

summary(survey_detection)


survey_dredge <- dredge(global.model = survey_detection,
                      rank = 'AICc')

summary(survey_dredge)

survey_dredge[1:5, ]

# Overall, the simplest detection model included just Noise as a covariate. 

# Step 2: Select the Occupation Model with the lowest AIC Value (Habitat Model)

#Habitat covariates selected include: low_marsh, high_marsh, pannes, and open_water

survey_occupation <- occu(formula = ~ 1
                          ~low_marsh + high_marsh + open_water + pannes,
                          data = sparrow_occu_data)

summary(survey_occupation)

survey_dredge <- dredge(global.model = survey_occupation,
              rank = 'AICc')

summary(survey_dredge)

survey_dredge[1:5, ]

# Based off the lowest AIC values, Low Marsh and Panne were selected as habitat covariates


# Step 2.5: Select the Occupation Model with the lowest AIC Value (Veg Species Model)

#Vegetation Species covariates include: SPALT_short, SPPAT, and DISPI
survey_species <- occu(formula = ~ 1
                       ~ SPALT_short + SPPAT + DISPI,
                       data = sparrow_occu_data)

summary(survey_species)

survey_dredge <- dredge(global.model = survey_species,
                        rank = 'AICc')

survey_dredge[1:5, ]


#Step 3: Combine the Models for a Full Occupany Model (Habitat Model)

survey_full <- occu(formula = ~ Noise ~  low_marsh + pannes + high_marsh,
                    data = sparrow_occu_data)

summary(survey_full)

new.data = expand.grid(
  cover = seq(0, 100, by = 2),
  habitat = c('low_marsh', 'pannes'))

Habitat_Model_Predict <- predict(survey_full,
                                 new_ata = new.data,
        type = 'det', na.rm = TRUE) %>%
  cbind(new.data) %>%
  filter(is.na(Predicted))

predict_m2_low <- cbind(predict(survey_full,
                                   newdata = data.frame(low_marsh = seq(min(sparrows_veg$low_marsh, 
                                                                         na.rm = TRUE),
                                                                     max(sparrows_veg$low_marsh, 
                                                                         na.rm = TRUE), 
                                                                     by = 0.01),
                                                        pannes = mean(sparrows_veg$pannes)),
                                   type = "state"),
                           data.frame(low_marsh = seq(min(sparrows_veg$low_marsh, 
                                                       na.rm = TRUE),
                                                   max(sparrows_veg$low_marsh, 
                                                       na.rm = TRUE), 
                                                   by = 0.01),
                                      pannes = mean(sparrows_veg$pannes)))

ggplot(data = predict_m2_low, aes(x = low_marsh, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  stat_smooth(method = "loess", col = "black", se = FALSE) +
  labs(x = "Forest (scaled)", y = "Predicted Occupancy Probability") +
  theme_classic()


ggplot(region,
       aes(y = Predicted, group = Treatment)) +
  geom_bar(aes(x = Treatment, fill = Treatment),
           stat = 'identity') + 
  geom_errorbar(aes(ymin = lower, ymax = upper, x = Treatment),
                stat = 'identity')





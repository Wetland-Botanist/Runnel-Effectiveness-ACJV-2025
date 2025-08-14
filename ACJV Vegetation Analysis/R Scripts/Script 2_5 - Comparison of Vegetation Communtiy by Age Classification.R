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
library(purrr)
library(broom.mixed)
library(lme4)
library(emmeans)
library(multcomp)

#Data Visualization
library(ggplot2)
library(viridis)


# Chapter 2: Import Plot Vegetation Dataset, Format Dataset ---------------------------

# Task 1: the vegetation plot dataset will be formatted properly for ordination and
# to be merged with the tidal and flooding datasets

veg <- read.csv("Formatted Datasets\\Reduced Data Site - Treatment Summary of Vegetation Plot Data.csv") %>%
  select(-X) %>%
  rename_with( ~ str_remove(., "_mean"), everything()) %>%
  mutate(Site_SubSite = ifelse(Sub_Site != "", 
                               paste(Site, Sub_Site, sep = " - "),
                               Site),
         Site_Treatment = paste(Site_SubSite, Overall_Treatment, sep = " - ")) %>%
  mutate(Age_Classification = ifelse(Overall_Treatment == "Runnel" & Time_Since_Restoration <= 4,
                                     "Early Restoration", 
                                     ifelse(Overall_Treatment == "Runnel" & Time_Since_Restoration > 4,
                                            "Older Restoration",
                                            ifelse(Overall_Treatment == "Reference", "Reference", "Pre-Restoration")))) %>%


# Chapter 3: Calculate Descriptive Statistics for Vegetation Components by Age Classification ------------

age_veg <- veg %>%
  group_by(Age_Classification) %>%
  summarise(across(SPALT:Canopy_Height_cm,
                  list(
                    mean = ~ mean(., na.rm = TRUE),
                    se = ~ sd(., na.rm = TRUE) / sqrt(n())   ))) %>%
  mutate(across(SPALT_mean:Canopy_Height_cm_se,
                ~ round(., 3)))
                    
glimpse(age_veg)


age_veg_mean <- age_veg %>%
  select(Age_Classification, contains("_mean")) %>%
  gather(contains("_mean"),
         key = Veg_Metric,
         value = Mean_Metric) %>%
  mutate(Veg_Metric = str_remove(Veg_Metric, "_mean"))

age_veg_se <- age_veg %>%
  select(Age_Classification, contains("_se")) %>%
  gather(contains("_se"),
         key = Veg_Metric,
         value = SE_Metric) %>%
  mutate(Veg_Metric = str_remove(Veg_Metric, "_se"))

age_veg_format <- age_veg_mean %>%
  merge(age_veg_se, by = c("Age_Classification", "Veg_Metric")) %>%
  mutate(Descriptive_Stat = paste(Mean_Metric, SE_Metric, sep = " +/- ")) %>%
  select(Age_Classification, Veg_Metric, Descriptive_Stat) %>%
  pivot_wider( 
    names_from = Veg_Metric,
    values_from = Descriptive_Stat)

glimpse(age_veg_format)

rm(age_veg_mean, age_veg_se)

write.csv(age_veg_format, 
          "Output Stats\\Summarized Veg Data by Age Classification.csv")


# Chapter 4: Conduct mixed linear ANOVAs for the vegetation components

# Task 1: Mixed Linear ANOVAs across all four vegetation components 

age_anovas_tukeys <- veg %>%
  gather(c(abiotic_cover, SPALT, high_marsh_grams, Canopy_Height_cm),
         key = Veg_Metric,
         value = Mean_Metric) %>%
  dplyr::select(Age_Classification, Site_Treatment, Veg_Metric, Mean_Metric) %>%
  group_by(Veg_Metric) %>%
  nest() %>%
  mutate(
    ANOVAs = map(.x = data,
               ~lmer(Mean_Metric ~ Age_Classification + (1 | Site_Treatment),
                     data = .x) %>% anova(.))) %>%
  mutate(
    Tukeys = map(.x = data,
                 ~lmer(Mean_Metric ~ Age_Classification + (1 | Site_Treatment),
                       data = .x) %>% 
                   emmeans(., pairwise ~ Age_Classification,
                           adjust = "tukey") %>% cld(.) ))

# Task 2: Extract the ANOVA tables and export

age_anovas <- age_anovas_tukeys %>%
  select(Veg_Metric, ANOVAs) %>%
  unnest(ANOVAs)


write.csv(age_anovas,
          "Output Stats\\Age Classification Veg Community ANOVA Table.csv")

# Task 3: Extract the pariwise comparisons from post-hoc tukeys

age_tukey <- age_anovas_tukeys %>%
  dplyr::select(Veg_Metric, Tukeys) %>%
  unnest(Tukeys)
  

write.csv(age_tukey,
          "Output Stats\\Age Classification Veg Community Tukey Table.csv")
























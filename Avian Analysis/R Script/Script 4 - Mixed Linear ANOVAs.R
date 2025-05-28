#Project: Avian Community Analysis of SHARP Point Counts of ACJV Project
#Script: Part IV - Mixed Linear Two-Way ANOVAs
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 12th, 2024
#Date Last Edited: March 21st, 2024


# Purpose: Conduct Mixed Linear Two-WAY ANOVAs on the avian community metrics

# Chapter 1: Set up Code

rm(list = ls())

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(vegan)
library(broom)
library(lme4)
library(afex)
library(emmeans)
library(ggResidpanel)


#Chapter 2: Load the 0 - 50 m SHARP Point Count dataset (surveys averaged per monitoring season)

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird 50m Dataset - Surveys Averaged.csv") %>%
  select(-X) %>%
  select(PointID :saltysparrow, -c(Aerial:Wading)) %>%
  mutate(Year = as.factor(Year)) %>%
  filter(Site != 'Moody Marsh') %>%
  filter(Site != 'Broad Cove'| Treatment != 'No Action')

glimpse(birds)



#Analysis 1: Total Bird Abundance

#Before final version of R code, I checked the residual plots of total bird abundance and found 
# a square-transformation was required

total <- lmer(sqrt(totalbirds) ~ Year * Treatment + (1|Site/PointID),
              data = birds)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(total)

#Returning the ANOVA table for the model
total_anova <- tidy(anova(total)) %>%
  mutate(metric = 'totalbirds')

total_anova

#Returning the Fixed and Random Effects table of the model
total_tidy <- broom.mixed::tidy(total) %>%
  mutate(metric = 'totalbirds')

total_tidy


#Post-hoc analysis between just Years for the model
total_posthoc <- emmeans(total, specs = pairwise ~ Year)

total_posthoc <- pairs(total_posthoc, adjust = "tukey")

total_posthoc


#Analysis 2: Salt Marsh Sparrows

#Before final version of R code, I checked the residual plots of 'saltysparrow' and found 
# a square-transformation was required

salty <- lmer(sqrt(saltysparrow) ~ Treatment * Year + (1|Site/PointID),
              data = birds)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(salty)

boxplot.stats(birds$saltysparrow)$out

#Returning the ANOVA table for the model
salty_anova <- tidy(anova(salty)) %>%
  mutate(metric = 'saltysparrow')

salty_anova

#Returning the Fixed and Random Effects table of the model
salty_tidy <- broom.mixed::tidy(salty) %>%
  mutate(metric = 'saltysparrow')

salty_tidy


#Post-hoc analysis between just Years for the model
salty_posthoc <- emmeans(salty, specs = pairwise ~ Treatment)

pairs(salty_posthoc, adjust = 'Tukey')

salty_posthoc

salty_posthoc_years <- emmeans(salty, specs = pairwise ~ Year)

salty_posthoc_years <- pairs(salty_posthoc_years, adjust = 'tukey')

salty_posthoc_years


#Analysis 3: Panne and Pool species

#Analysis Notes - square root transformation to improve anova assumptions of normality of data


panne <- lmer(sqrt(Panne_Dependent) ~ Treatment * Year + (1|Site/PointID),
              data = birds)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(panne)

boxplot.stats(birds$Panne_Dependent)$out

#Returning the ANOVA table for the model
panne_anova <- tidy(anova(panne)) %>%
  mutate(metric = 'panne_dependent')

panne_anova

#Returning the Fixed and Random Effects table of the model
panne_tidy <- broom.mixed::tidy(panne) %>%
  mutate(metric = 'panne_dependent')

panne_tidy

#No post-hoc analysis needed due to insignificance across all three fixed factors




#Analysis 4: Weighed Wetland Score

#Analysis notes - no transformation or outliers removed

wetland <- lmer(weighted_wetland_score ~ Treatment * Year + (1|Site/PointID),
              data = birds)


#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(wetland)

boxplot.stats(sqrt(birds$weighted_wetland_score))$out

#Returning the ANOVA table for the model
wetland_anova <- tidy(anova(wetland)) %>%
  mutate(metric = 'weighted_wetland_score')

wetland_anova

#Returning the Fixed and Random Effects table of the model
wetland_tidy <- broom.mixed::tidy(wetland) %>%
  mutate(metric = 'weighted_wetland_score')

wetland_tidy


#No post-hoc test needed 


#Analysis 5: Shannon Diversity

#Analysis notes - no need for transformation, outlier removal

shannon <- lmer(shannon ~ Treatment * Year + (1|Site/PointID),
              data = birds)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(shannon)

boxplot.stats(birds$shannon)$out

#Returning the ANOVA table for the model
shannon_anova <- tidy(anova(shannon)) %>%
  mutate(metric = 'shannon')

shannon_anova

#Returning the Fixed and Random Effects table of the model
shannon_tidy <- broom.mixed::tidy(shannon) %>%
  mutate(metric = 'shannon')

shannon_tidy


#No need for post-hoc analysis



#Analysis 6: Species Richness

#Before final version of R code, I checked the residual plots of 'saltysparrow' and found 
# a square-transformation was required

species_richness <- lmer(richness ~ Treatment * Year + (1|Site/PointID),
              data = birds)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(species_richness)

boxplot.stats(birds$richness)$out

#Returning the ANOVA table for the model
species_richness_anova <- tidy(anova(species_richness)) %>%
  mutate(metric = 'richness')

species_richness_anova

#Returning the Fixed and Random Effects table of the model
species_richness_tidy <- broom.mixed::tidy(species_richness) %>%
  mutate(metric = 'richness')

species_richness_tidy


#No need for post-hoc analysis




#Chapter 5: Compile the ANOVA, Tidy, and post-hoc tables together and export


anova_tables <- rbind(total_anova, salty_anova, panne_anova, wetland_anova, species_richness_anova, shannon_anova)

write.csv(anova_tables,
          "Avian Analysis\\Output Stats\\Mixed ANOVA Tables Compiled.csv")

tidy_tables <- rbind(total_tidy, salty_tidy, panne_tidy, wetland_tidy, species_richness_tidy, shannon_tidy)

write.csv(tidy_tables,
          "Avian Analysis\\Output Stats\\Mixed Tidy Tables Compiled.csv")

posthoc_tables <- rbind(total_posthoc, salty_posthoc)

write.csv(anova_tables,
          "Avian Analysis\\Output Stats\\ANOVA Posthoc Tables Compiled.csv")
  

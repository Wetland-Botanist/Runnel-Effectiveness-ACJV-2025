# Project: Atlantic Coast Joint Venture - Assessment of Runnel Effectiveness
# Analysis: Vegetation Analysis
# Script: Script 9 - Model Selection
# Author: Grant McKown (james.mckown@unh.edu)

# Purpose: Conduct non-metric dimensional ordination on vegetation community

# Chapter 1: Package Library ---------------------------------------------------


#Data Organization & Formatting
library(tidyverse)
library(stringr)

#Statistics and Data Analysis
library(lme4)
library(car)
library(MuMIn)
library(performance)
library(afex)


#Data Visualization
library(ggplot2)
library(viridis)
library(patchwork)



# Chapter 1: Import the Vegetation Data Summarized by Site - Treatment

# The dataset is reduced to only contain the means of the main components of the 
# vegetation community: abiotic community, Spartina alterniflora, high marsh
# graminoids, and other species

# Task 1: the vegetation plot dataset will be formatted properly for ordination and
# to be merged with the tidal and flooding datasets
veg <- read.csv("Formatted Datasets\\Reduced Data Summary of Vegetation Plot Data.csv") %>%
  # Remove extraneous column from saving process
  dplyr::select(-X, -richness_mean, -algae_cover_mean) %>%
  #Remove the extraneous "_mean" from column names
  rename_with(~str_remove(., "_mean$")) %>%
  # Create new identifier and classifier columns for merging other dataframes
  mutate(Site_SubSite = ifelse(Sub_Site != "", 
                               paste(Site, Sub_Site, sep = " - "),
                               Site),
         Site_Treatment = paste(Site_SubSite, Overall_Treatment, sep = " - ")) %>%
  mutate(Age_Classification = ifelse(Overall_Treatment == "Runnel" & Time_Since_Restoration <= 4,
                                     "Early Restoration", 
                                     ifelse(Overall_Treatment == "Runnel" & Time_Since_Restoration > 4,
                                            "Older Restoration",
                                            ifelse(Overall_Treatment == "Reference", "Reference", "Pre-Restoration")))) %>%
  filter(Overall_Treatment == "Runnel")

glimpse(veg)


# Task 3: Import the tidal datum dataset

tides <- read.csv("Input Data\\ACJV Site Treatment Metadata.csv") %>%
  mutate(Site_SubSite = ifelse(Subsite != "", 
                               paste(Site, Subsite, sep = " - "),
                               Site),
         Site_Treatment = paste(Site_SubSite, Overall_Treatment, sep = " - ")) %>%
  filter(Site != "Plum Island - DPR" | Subsite != "North" | Overall_Treatment != "Runnel" | Year_0 != "2019") %>%
  filter(Overall_Treatment == "Runnel")

glimpse(tides)

# Task 4: Import the flooding metrics dataset

flooding <- read.csv("Input Data\\ACJV Master Hydrology Flooding Metrics.csv") %>%
  mutate(Site_SubSite = ifelse(Sub_Site != "", 
                               paste(Site, Sub_Site, sep = " - "),
                               Site),
         Site_Treatment = paste(Site_SubSite, Overall_Treatment, sep = " - ")) %>%
  filter(Include_Analysis == "Yes",
         Overall_Treatment == "Runnel")

glimpse(flooding)


# Task 4: Merge the Tidal Datum and Flooding Metrics dataset to the Vegetation Dataset

# First, the tidal datum dataset will be merged based on the Site_Treatment characteristic,
# since the 2021.2022 tidal and elevation datums will be used for 2021/2022 and 2024

# Second, flooding statistics will be merged based on the Site_Treatment and Year characteristics. 

veg_compiled <- veg %>%
  merge(dplyr::select(tides, c(Site_Treatment, Runnel_Marsh_Precondition,
                               MHT_NAVD88m:Spring_Tide_Difference_Relative)), 
        by = c("Site_Treatment")) %>%
  merge(dplyr::select(flooding,
                      c(Site_Treatment, Year, Platform_Flood_Duration:Drainage_Amplitude_m)),
        by = c("Site_Treatment", "Year")) %>%
  filter(!is.na(Drainage_Amplitude_m)) %>%
  filter(!is.na(MHT_Difference_Relative))

glimpse(veg_compiled)

write.csv(veg_compiled, 
          "Formatted Datasets\\Model Selection Dataset.csv")



# Chapter 3: Abiotic Cover Model Selection ------------------------------------

# Task 1: Variance Inflation Factor

# Before preceeding in the model selection proceses with dredge(), we need to process
# multi-collinearity betweten the predictors. Collinearity will be assessed with the vif()
# function of the cars package.


vif_check <- lmer(abiotic_cover ~ Time_Since_Restoration + Tidal_Regime + 
                    Runnel_Marsh_Precondition + Root_Drainage_Depth_m + 
                    Drainage_Amplitude_m + Platform_HT_Frequency + 
                    MHT_Difference_Relative + (1 | Site_Treatment),
                  data = veg_compiled,
                  REML = TRUE)

vif(vif_check)  




global_mod <- lmer(abiotic_cover ~ Time_Since_Restoration + Tidal_Regime + Runnel_Marsh_Precondition +
                     Root_Drainage_Depth_m + Drainage_Amplitude_m + Platform_HT_Frequency + 
                     MHT_Difference_Relative +
                     Time_Since_Restoration:Tidal_Regime +
                     Time_Since_Restoration:Runnel_Marsh_Precondition +
                     Time_Since_Restoration:Root_Drainage_Depth_m +
                     Time_Since_Restoration:Drainage_Amplitude_m +
                     Time_Since_Restoration:MHT_Difference_Relative +
                     Tidal_Regime:Runnel_Marsh_Precondition +
                     Runnel_Marsh_Precondition:Root_Drainage_Depth_m +
                     Runnel_Marsh_Precondition:Drainage_Amplitude_m +
                     Year_0:Platform_HT_Frequency + 
                     (1 | Site_Treatment),
                   data = veg_compiled,
                   REML = TRUE)

# No factors had a VIF score of greater than 5. All variables were retained. 
                     
# Task 2: Conduct Model Selection with dredge()

# For now, we will perform the model selection based solely on additive (no interactions) factors

# Set the settings of na.action to align with dredge() function
options(na.action = na.fail)

# Perform model selection process with dredge()
abiotic_dredge <- dredge(global_mod)

# Select all models within 2 AIC scores of the lowest AIC score
models <- subset(abiotic_dredge, delta < 2)                     

# After reviewing the models dataset, selected the simplest model and plugged it into R

abiotic_bestmod <- lmer(abiotic_cover ~ Time_Since_Restoration + Tidal_Regime + Runnel_Marsh_Precondition +
                          Root_Drainage_Depth_m + Drainage_Amplitude_m +
                          Time_Since_Restoration:Tidal_Regime +
                          Time_Since_Restoration:Root_Drainage_Depth_m +
                          Time_Since_Restoration:Drainage_Amplitude_m +
                          Tidal_Regime:Runnel_Marsh_Precondition +
                          Runnel_Marsh_Precondition:Root_Drainage_Depth_m +
                          Runnel_Marsh_Precondition:Drainage_Amplitude_m +
                          (1 | Site_Treatment),
                        data = veg_compiled)

# Diagnostic testing for the best model
performance::check_model(abiotic_bestmod)

# Model meets the assumptions of parametric linear models but proceed with caution
# Model does not look great with homogeneity of variance...

# Task 3: Review the summary and ANOVA table of the model
                     
summary(abiotic_bestmod)                     

anova(abiotic_bestmod)                     


# For abiotic cover, the factors that significantly impacted the restoration
# trajectory were Time Since Restoration, Drainage Amplitude, and Runnel Marsh
# Pre-Condition
                     
                     


                     
# Chapter 4: High Marsh Graminoid Model Selection ------------------------------------

# Task 1: Variance Inflation Factor

# Before preceeding in the model selection proceses with dredge(), we need to process
# multi-collinearity betweten the predictors. Collinearity will be assessed with the vif()
# function of the cars package.


vif_check <- lmer(high_marsh_grams ~ Time_Since_Restoration + Year_0 + Tidal_Regime + 
                    Runnel_Marsh_Precondition + Root_Drainage_Depth_m + 
                    Drainage_Amplitude_m + Platform_HT_Frequency + 
                    MHT_Difference_Relative + (1 | Site_Treatment),
                   data = veg_compiled,
                   REML = TRUE)

vif(vif_check)  





global_mod <- lmer(high_marsh_grams ~ Time_Since_Restoration + 
                     Year_0 + 
                     Tidal_Regime + 
                     Runnel_Marsh_Precondition +
                     Drainage_Amplitude_m + 
                     Platform_HT_Frequency + 
                     MHT_Difference_Relative +
                     Time_Since_Restoration:Tidal_Regime +
                     Time_Since_Restoration:Runnel_Marsh_Precondition +
                     Time_Since_Restoration:Drainage_Amplitude_m +
                     Time_Since_Restoration:MHT_Difference_Relative +
                     Tidal_Regime:Runnel_Marsh_Precondition +
                     Runnel_Marsh_Precondition:Drainage_Amplitude_m +
                     Year_0:Platform_HT_Frequency + 
                     (1 | Site_Treatment),
                   data = veg_compiled,
                   REML = TRUE)

# No factors had a VIF score of greater than 5. All variables were retained. 

# Task 2: Conduct Model Selection with dredge()

# For now, we will perform the model selection based solely on additive (no interactions) factors

# Set the settings of na.action to align with dredge() function
options(na.action = na.fail)

# Perform model selection process with dredge()
hmg_dredge <- dredge(global_mod)

# Select all models within 2 AIC scores of the lowest AIC score
models <- subset(hmg_dredge, delta < 2)                     

# After reviewing the models dataset, selected the simplest model and plugged it into R

hmg_bestmod <- lmer(high_marsh_grams ~ Time_Since_Restoration + 
                      Tidal_Regime + 
                      Runnel_Marsh_Precondition +
                      Drainage_Amplitude_m + 
                      Time_Since_Restoration:Tidal_Regime +
                      Time_Since_Restoration:Drainage_Amplitude_m +
                      Tidal_Regime:Runnel_Marsh_Precondition +
                      Runnel_Marsh_Precondition:Drainage_Amplitude_m +
                          (1 | Site_Treatment),
                        data = veg_compiled)

# Diagnostic testing for the best model
performance::check_model(hmg_bestmod)

# Model meets the assumptions of parametric linear models but proceed with caution
# Model does not look great with homogeneity of variance...

# Model had to be square-root transformed to meet linear assumptions

# Task 3: Review the summary and ANOVA table of the model

summary(hmg_bestmod)                     

anova(hmg_bestmod)      

# Task 4: Re-run the model with only significant factors

hmg_rerun_bestmod <- lmer(high_marsh_grams ~ Tidal_Regime + 
                      Drainage_Amplitude_m + 
                      Time_Since_Restoration:Tidal_Regime +
                      Time_Since_Restoration:Drainage_Amplitude_m +
                      Tidal_Regime:Runnel_Marsh_Precondition +
                      (1 | Site_Treatment),
                    data = veg_compiled)


summary(hmg_rerun_bestmod)                     

anova(hmg_rerun_bestmod)   

# For abiotic cover, the factors that significantly impacted the restoration
# trajectory were Time Since Restoration, and Runnel Marsh Pre-Condition



# Chapter 5: Spartina alterniflora Model Selection ------------------------------------

# Task 1: Variance Inflation Factor

# Before preceeding in the model selection proceses with dredge(), we need to process
# multi-collinearity betweten the predictors. Collinearity will be assessed with the vif()
# function of the cars package.



vif_check <- lmer(abiotic_cover ~ Time_Since_Restoration + Year_0 + Tidal_Regime + 
                    Runnel_Marsh_Precondition + Root_Drainage_Depth_m + 
                    Drainage_Amplitude_m + Platform_HT_Frequency + 
                    MHT_Difference_Relative + (1 | Site_Treatment),
                  data = veg_compiled,
                  REML = TRUE)

vif(vif_check)  


global_mod <- lmer(SPALT ~ Time_Since_Restoration + 
                     Year_0 + 
                     Tidal_Regime + 
                     Runnel_Marsh_Precondition +
                     Drainage_Amplitude_m + 
                     Platform_HT_Frequency + 
                     MHT_Difference_Relative +
                     Time_Since_Restoration:Tidal_Regime +
                     Time_Since_Restoration:Runnel_Marsh_Precondition +
                     Time_Since_Restoration:Drainage_Amplitude_m +
                     Time_Since_Restoration:MHT_Difference_Relative +
                     Tidal_Regime:Runnel_Marsh_Precondition +
                     Runnel_Marsh_Precondition:Drainage_Amplitude_m +
                     Year_0:Platform_HT_Frequency + 
                     (1 | Site_Treatment),
                   data = veg_compiled,
                   REML = TRUE)

# Time Since Restoration had a VIF score of greater than 5. It was removed. 

# Task 2: Conduct Model Selection with dredge()

# For now, we will perform the model selection based solely on additive (no interactions) factors

# Set the settings of na.action to align with dredge() function
options(na.action = na.fail)

# Perform model selection process with dredge()
spalt_dredge <- dredge(global_mod)

# Select all models within 2 AIC scores of the lowest AIC score
models <- subset(spalt_dredge, delta < 2)                     

# After reviewing the models dataset, selected the simplest model and plugged it into R

spalt_bestmod <- lmer(SPALT ~ Time_Since_Restoration + 
                        Tidal_Regime + 
                        Runnel_Marsh_Precondition +
                        Drainage_Amplitude_m + 
                        Time_Since_Restoration:Drainage_Amplitude_m +
                        Tidal_Regime:Runnel_Marsh_Precondition +
                        Runnel_Marsh_Precondition:Drainage_Amplitude_m +
                      (1 | Site_Treatment),
                    data = veg_compiled)

# Diagnostic testing for the best model
performance::check_model(spalt_bestmod)

# Model meets the assumptions of parametric linear models but proceed with caution
# Model does not look great with homogeneity of variance...

# Model had to be square-root transformed to meet linear assumptions

# Task 3: Review the summary and ANOVA table of the model

summary(spalt_bestmod)                     

anova(spalt_bestmod)                     


# For abiotic cover, the factors that significantly impacted the restoration
# trajectory was Root Drainage Depth



# Chapter 6: Canopy Height Model Selection ------------------------------------

# Task 1: Variance Inflation Factor

# Before preceeding in the model selection proceses with dredge(), we need to process
# multi-collinearity betweten the predictors. Collinearity will be assessed with the vif()
# function of the cars package.


vif_check <- lmer(abiotic_cover ~ Time_Since_Restoration + Year_0 + Tidal_Regime + 
                    Runnel_Marsh_Precondition + Root_Drainage_Depth_m + 
                    Drainage_Amplitude_m + Platform_HT_Frequency + 
                    MHT_Difference_Relative + (1 | Site_Treatment),
                  data = veg_compiled,
                  REML = TRUE)

vif(vif_check)  


global_mod <- lmer(Canopy_Height_cm ~ Time_Since_Restoration + 
                     Year_0 + 
                     Tidal_Regime + 
                     Runnel_Marsh_Precondition + 
                     Drainage_Amplitude_m + 
                     Platform_HT_Frequency + 
                     MHT_Difference_Relative +
                     Time_Since_Restoration:Tidal_Regime +
                     Time_Since_Restoration:Runnel_Marsh_Precondition +
                     Time_Since_Restoration:Drainage_Amplitude_m +
                     Time_Since_Restoration:MHT_Difference_Relative +
                     Tidal_Regime:Runnel_Marsh_Precondition +
                     Runnel_Marsh_Precondition:Drainage_Amplitude_m +
                     Year_0:Platform_HT_Frequency + 
                     (1 | Site_Treatment),
                   data = veg_compiled,
                   REML = TRUE)

# Task 2: Conduct Model Selection with dredge()

# For now, we will perform the model selection based solely on additive (no interactions) factors

# Set the settings of na.action to align with dredge() function
options(na.action = na.fail)

# Perform model selection process with dredge()
canopy_dredge <- dredge(global_mod)

# Select all models within 2 AIC scores of the lowest AIC score
models <- subset(canopy_dredge, delta < 2)                     

# After reviewing the models dataset, selected the simplest model and plugged it into R

canopy_bestmod <- lmer(Canopy_Height_cm ~ Time_Since_Restoration +
                         Tidal_Regime + 
                         Runnel_Marsh_Precondition +
                         Drainage_Amplitude_m + 
                         Time_Since_Restoration:Drainage_Amplitude_m +
                         Tidal_Regime:Runnel_Marsh_Precondition +
                         Runnel_Marsh_Precondition:Drainage_Amplitude_m +
                        (1 | Site_Treatment),
                      data = veg_compiled)

# Diagnostic testing for the best model
performance::check_model(canopy_bestmod)

# Model meets the assumptions of parametric linear models but proceed with caution
# Model does not look great with homogeneity of variance...

# Model had to be square-root transformed to meet linear assumptions

# Task 3: Review the summary and ANOVA table of the model

summary(canopy_bestmod)                     

anova(canopy_bestmod)


# Task 4: Re-run the model with only the significant factors and assess

canopy_rerun_bestmod <- lmer(Canopy_Height_cm ~ Time_Since_Restoration +
                         Drainage_Amplitude_m + 
                         Time_Since_Restoration:Drainage_Amplitude_m +
                         (1 | Site_Treatment),
                       data = veg_compiled)


summary(canopy_rerun_bestmod)                     

anova(canopy_rerun_bestmod)


# For abiotic cover, the factors that significantly impacted the restoration
# trajectory were Time Since Restoration
                     
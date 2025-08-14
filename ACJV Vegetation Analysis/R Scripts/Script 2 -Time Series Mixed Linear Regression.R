# Project: Atlantic Coast Joint Venture - Assessment of Runnel Effectiveness
# Analysis: Vegetation Analysis
# Script: Script 2 - Time Series Regression

# Author: Grant McKown (james.mckown@unh.edu)

# Purpose: Conduct mixed linear time series regressions of vegetation metrics from the ACJV project. 

# Chapter 1: Package Library ---------------------------------------------------


#Data Organization & Formatting
library(tidyverse)
library(stringr)


#Statistics and Data Analysis
library(ggfortify)
library(broom.mixed)
library(lme4) 
library(afex)
library(ggfortify)
library(ggeffects)

#Data Visualization
library(ggplot2)
library(viridis)
library(patchwork)




# Chapter 2: Data Input and QAQC ----------------------------------------------

# Import the Dataset Summarized at the Site - Treatment Level

veg <- read.csv("Formatted Datasets\\Reduced Data Site - Treatment Summary of Vegetation Plot Data.csv") %>%
  select(-X) %>%
  rename_with( ~ str_remove(., "_mean"), everything()) %>%
  mutate(Site_Treatment_ID = paste(Site, Site_Treatment, sep = " - "),
         HM_Ratio = high_marsh_grams / (high_marsh_grams + SPALT))

glimpse(veg)



# Chapter 3: Mixed Linear Time Series -----------------------

# Mixed linear regressions are applied over Time Since Restoration for Spartina alterniflora Cover, 
# High mmarsh graminoid cover, abiotic cover, and canopy height 

# Mixed linear model overview:

# Fixed Effects - Time, Treatment, Interaction
# Random Effect - Site - Treatment


# Task 1: Spartina alterniflora cover over time
  
# Create the Spartina alterniflora cover model

spalt_mod <- lmer(SPALT ~ Overall_Treatment * Time_Since_Restoration + (1| Site_Treatment_ID),
                  data = veg)

anova(spalt_mod)  

spalt_anova <- broom.mixed::tidy(anova(spalt_mod)) %>%
  mutate(model = "SPALT")

# The time and interaction factors were non-significant, highlighting little change
# over time for Spartina alterniflora

spalt.preds <- ggpredict(spalt_mod, terms = c( "Overall_Treatment [all]"),
                          type = "fixed", interval = 'confidence') %>%
  rename(Overall_Treatment = x,
         SPALT = predicted,
         Timeline = group) %>%
  arrange(Overall_Treatment)

spalt.preds$conf.low[spalt.preds$conf.low < 0] <- 0

glimpse(spalt.preds)


# Task 2: High marsh graminoid cover over time

# Create the high marsh graminoid cover model

hmg_mod <- lmer(high_marsh_grams ~ Overall_Treatment * Time_Since_Restoration + (1 | Site_Treatment_ID),
                  data = veg)

anova(hmg_mod)  

hmg_anova <- broom.mixed::tidy(anova(hmg_mod)) %>%
  mutate(model = "High Marsh Graminoid")


# Interaction of Time and Treatment was significant, so the model values will be predicted
# with both Time and Treatment considered

hmg.preds <- ggpredict(hmg_mod, terms = c("Time_Since_Restoration [all]", "Overall_Treatment [all]"),
                         type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Overall_Treatment = group,
         high_marsh_grams = predicted) %>%
  arrange(Overall_Treatment)


# Calculate slope of the high marsh graminoid for each overall treatment

hmg.slope <- hmg.preds %>%
  group_by(Overall_Treatment) %>%
  summarise(Slope = (high_marsh_grams[which.max(Timeline)] - high_marsh_grams[which.min(Timeline)]) / (max(Timeline) - min(Timeline)) ) 

hmg.slope


# Task 3: Abiotic  cover over time

# Create the abiotic cover model

abiotic_mod <- lmer(abiotic_cover ~ Overall_Treatment * Time_Since_Restoration + (1 | Site_Treatment_ID),
                data = veg)

anova(abiotic_mod)  

abiotic_anova <- broom.mixed::tidy(anova(abiotic_mod)) %>%
  mutate(model = "Abiotic Cover")

# Interaction of Time and Treatment was approaching significance, so the model values will be predicted
# with both Time and Treatment considered

abiotic.preds <- ggpredict(abiotic_mod, terms = c("Time_Since_Restoration [all]", "Overall_Treatment [all]"),
                       type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Overall_Treatment = group,
         abiotic_cover = predicted) %>%
  arrange(Overall_Treatment)


# Calculate slope of abiotic cover for each treatment over time

abiotic.slope <- abiotic.preds %>%
  group_by(Overall_Treatment) %>%
  summarise(Slope = (abiotic_cover[which.max(Timeline)] - abiotic_cover[which.min(Timeline)]) / (max(Timeline) - min(Timeline)) ) 

abiotic.slope


# Task 4: Canopy height over time

# Create the Canopy Height model

canopy_mod <- lmer(Canopy_Height_cm ~ Overall_Treatment * Time_Since_Restoration + (1| Site_Treatment_ID),
                    data = veg)

anova(canopy_mod)  

canopy_anova <- broom.mixed::tidy(anova(canopy_mod)) %>%
  mutate(model = "Canopy Height")

# Note the ANOVA table showed that only the Time factor was significant, highlighting
# a general trend of canopy height over time but not between treatments

canopy.preds <- ggpredict(canopy_mod, terms = c("Time_Since_Restoration [all]"),
                           type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         Canopy_Height_cm = predicted)

glimpse(canopy.preds)

#Calculate the slope of the Canopy Height model over time

canopy_slope <- (canopy.preds$Canopy_Height_cm[which.max(canopy.preds$Timeline)] - canopy.preds$Canopy_Height_cm[which.min(canopy.preds$Timeline)]) / (max(canopy.preds$Timeline) - min(canopy.preds$Timeline))

canopy_slope

# Task 5: High Marsh Ratio

# Create the Canopy Height model

ratio_mod <- lmer(HM_Ratio ~ Overall_Treatment * Time_Since_Restoration + (1| Site_Treatment_ID),
                   data = veg)

anova(ratio_mod)  

ratio_anova <- broom.mixed::tidy(anova(ratio_mod)) %>%
  mutate(model = "High Marsh Ratio")

# Note the ANOVA table showed that only the Time factor was significant, highlighting
# a general trend of canopy height over time but not between treatments

ratio.preds <- ggpredict(ratio_mod, terms = c("Time_Since_Restoration [all]", "Overall_Treatment [all"),
                          type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         HM_Ratio = predicted,
         Overall_Treatment = group)

ratio.preds$conf.high[ratio.preds$conf.high > 1] <- 1

glimpse(ratio.preds)

# Calculate slope of high marsh ratio for each treatment over time

ratio.slope <- ratio.preds %>%
  group_by(Overall_Treatment) %>%
  summarise(Slope = (HM_Ratio[which.max(Timeline)] - HM_Ratio[which.min(Timeline)]) / (max(Timeline) - min(Timeline)) ) 

ratio.slope



# Task 5: Merge the Mixed Linear Model ANOVA tables together

mixed_time_series_anova <- spalt_anova %>%
  rbind(hmg_anova, abiotic_anova, canopy_anova, ratio_anova) %>%
  select(model, term, sumsq, meansq, NumDF, DenDF, statistic, p.value)


write.csv(mixed_time_series_anova,
          "Output Stats\\Linear Mixed Time Series ANOVAs.csv")









# Chapter 4: Graph the linear mixed model results -----------------------------

# Task 1: Graph the results of the ANCOVA analyses with significant interactions

# Create a plug-n-n play ggplot with facet_wrap

mixed_time_series <- ggplot(data = abiotic.preds,
                            aes(x = Timeline,
                                y = abiotic_cover)) + 
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high,
                  fill = Overall_Treatment),
              alpha = 0.25) +
  geom_line(aes(colour = Overall_Treatment),
    linewidth = 1.5, linetype = "dashed") +
  geom_point(data = veg,
             aes(x = Time_Since_Restoration, 
                 y = abiotic_cover,
                 fill = Overall_Treatment), 
             size = 5.5,
             shape = 21,
             colour = "black") +
  labs(x = "Restoration Age (yrs)", 
       y = "Abiotic Cover (%)") + 
  scale_x_continuous(limits = c(0, 14), 
                     breaks = seq(0, 14, 2)) + 
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  scale_colour_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
 facet_wrap(~Overall_Treatment, 
             nrow = 3, ncol = 1,
            scales = "free")

mixed_time_series


ggsave(mixed_time_series, 
       height = 14, width = 9, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Abiotic Time Series Model - Panel.jpg")




# Task 2: Graph the results of the ANCOVA analyses with significant Time factor

# Create a plug-n-n play ggplot with facet_wrap

mixed_time_series <- ggplot(data = canopy.preds,
                            aes(x = Timeline,
                                y = Canopy_Height_cm)) + 
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high),
              alpha = 0.5,
              fill = "grey") +
  geom_line(linewidth = 1.5, 
            linetype = "dashed") +
  geom_point(data = veg,
             aes(x = Time_Since_Restoration, 
                 y = Canopy_Height_cm,
                 fill = Overall_Treatment),
             size = 5.5,
             shape = 21,
             colour = "black") +
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  labs(x = "Restoration Age (yrs)", 
       y = "Canopy Height (cm)") + 
  scale_x_continuous(limits = c(0, 14), 
                     breaks = seq(0, 14, 2)) + 
  scale_y_continuous(limits = c(0, 50), 
                     breaks = seq(0, 50, 10)) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.15),
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black"))

mixed_time_series


ggsave(mixed_time_series, 
       height = 10, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Canopy Time Series Model - Combined.jpg")




  
  
  
  
  
  
  
  









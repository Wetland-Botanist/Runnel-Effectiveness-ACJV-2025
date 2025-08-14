# Project: Atlantic Coast Joint Venture - Assessment of Runnel Effectiveness
# Analysis: Vegetation Analysis
# Script: Script 4 - Time Series Regression with Tidal Range Factor

# Author: Grant McKown (james.mckown@unh.edu)

# Purpose:


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
  mutate(Site_Treatment_ID = paste(Site, Site_Treatment, sep = " - ")) %>%
  mutate(Site_Treatment_ID = paste(Site, Site_Treatment, sep = " - "),
         HM_Ratio = high_marsh_grams / (high_marsh_grams + SPALT))

# Reduce the dataset to just runnel treatment for the analysis

veg_run <- veg %>%
  filter(Overall_Treatment == "Runnel")

glimpse(veg)


# Dataset Overview

veg_overview <- veg_run %>%
  summarise(
    Plots = n(),
    Estuary = length(unique(Salt_Marsh)),
    Seasons = length(unique(Season)),
    Site_Treatment = length(unique(Site_Treatment_ID)),
    Age = length(unique(Time_Since_Restoration)),
    Age_Span = paste(min(Time_Since_Restoration), max(Time_Since_Restoration), sep = " - "))

glimpse(veg_overview)


# Chapter 3: Visualize the distribution of the dataset --------------------------------

# Graph a histogram of the number of sites in each marsh condition to understand the 
# distribution of sites in the analysis

# Calculate the number of sites for each year and each marsh condition
veg_tides_count <- veg_run %>%
  group_by(Time_Since_Restoration, Tidal_Regime) %>%
  summarise(Site_Count = n()) %>%
  ungroup()

glimpse(veg_tides_count)


# Graph the distribution of sites
tides_histogram <- ggplot(
  aes(x = Time_Since_Restoration, 
      y = Site_Count,
      group = Tidal_Regime), 
  data = veg_tides_count) + 
  geom_bar(aes(fill = Tidal_Regime),
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
  scale_fill_manual(values = c("orange", "darkgreen")) +
  scale_colour_manual(values = c("orange", "darkgreen")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Tidal_Regime,
             nrow = 2)

tides_histogram



ggsave(tides_histogram, 
       height = 8, width = 12, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Tidal Range\\Site Count Histogram.jpg")



# Chapter 4: Mixed Linear Time Series with Marsh Condition -----------------

# The second inspection of the vegetation dataset involves understanding how the marsh
# initial condition impacts the restoration trajectory. The vegetation dataset is reduced
# to just the runnel treatment. 

# Model Components
# Fixed Effects: Time, Tidal Range, Interaction
# Random Effects: Site Treatment ID


# Task 1: Spartina alterniflora cover over time

# Create the Spartina alterniflora cover model

spalt_mod <- lmer(SPALT ~ Time_Since_Restoration * Tidal_Regime + (1| Site_Treatment_ID),
                  data = veg_run)

anova(spalt_mod)  

spalt_anova <- broom.mixed::tidy(anova(spalt_mod)) %>%
  mutate(model = "SPALT")


# Time was the only significant interaction for the dataset

spalt.preds <- ggpredict(spalt_mod, terms = c("Time_Since_Restoration [all]"),
                         type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         SPALT = predicted)

spalt.preds$conf.low[spalt.preds$conf.low < 0] <- 0

glimpse(spalt.preds)

# Task 2: High Marsh Graminoid regression

hmg_mod <- lmer(high_marsh_grams ~ Time_Since_Restoration * Tidal_Regime + (1|Site_Treatment_ID),
                data = veg_run)

anova(hmg_mod)

hmg_anova <- broom.mixed::tidy(anova(hmg_mod)) %>%
  mutate(model = "High Marsh Graminoid")


#Note - only the time component was found to be significant, so only the linear regression
# of the high marsh graminoids were predicted with ggpredict

hmg.preds <- ggpredict(hmg_mod, terms = c("Time_Since_Restoration [all]"),
                       type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         high_marsh_grams = predicted,
         Marsh_Condition = group)

hmg.preds$conf.low[hmg.preds$conf.low < 0] <- 0

# Task 3: Abiotic  cover over time

# Create the abiotic cover model

abiotic_mod <- lmer(abiotic_cover ~ Time_Since_Restoration * Tidal_Regime + (1|Site_Treatment_ID),
                    data = veg_run)

anova(abiotic_mod)  

abiotic_anova <- broom.mixed::tidy(anova(abiotic_mod)) %>%
  mutate(model = "Abiotic Cover")



#Note - the only significant factor was Time

abiotic.preds <- ggpredict(abiotic_mod, terms = c("Time_Since_Restoration [all]"),
                           type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         abiotic_cover = predicted)

abiotic.preds$conf.low[abiotic.preds$conf.low < 0] <- 0

glimpse(abiotic.preds)

# Task 4: Canopy height over time

# Create the Canopy Height model

canopy_mod <- lmer(Canopy_Height_cm ~  Time_Since_Restoration * Tidal_Regime + (1| Site_Treatment_ID),
                   data = veg_run)

anova(canopy_mod)  

canopy_anova <- broom.mixed::tidy(anova(canopy_mod)) %>%
  mutate(model = "Canopy Height")

# Note the ANOVA table showed that only the Time factor was significant, highlighting
# a general trend of canopy height over time but not between treatments

canopy.preds <- ggpredict(canopy_mod, terms = c("Time_Since_Restoration [all]"),
                          type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         canopy_height = predicted)

glimpse(canopy.preds)


# Task 5: High Marsh Ratio

# Create the Canopy Height model

ratio_mod <- lmer(HM_Ratio ~  Time_Since_Restoration * Tidal_Regime + (1| Site_Treatment_ID),
                  data = veg_run)

anova(ratio_mod)  

ratio_anova <- broom.mixed::tidy(anova(ratio_mod)) %>%
  mutate(model = "High Marsh Ratio")

# Note the ANOVA table showed that only the Time factor was significant, highlighting
# a general trend of canopy height over time but not between treatments

ratio.preds <- ggpredict(ratio_mod, terms = c("Time_Since_Restoration [all]"),
                         type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         HM_Ratio = predicted,
         Overall_Treatment = group)

ratio.preds$conf.high[ratio.preds$conf.high > 1] <- 1

glimpse(ratio.preds)



# Task 6: Merge the Mixed Linear Model ANOVA tables together

mixed_time_series_anova <- spalt_anova %>%
  rbind(hmg_anova, abiotic_anova, canopy_anova, ratio_anova) %>%
  select(model, term, sumsq, meansq, NumDF, DenDF, statistic, p.value)


write.csv(mixed_time_series_anova,
          "Output Stats\\Tidal Range Linear Mixed Time Series ANOVAs.csv")


# Chapter 4: Graph the marsh condition mixed models -----------------------------

# Create a plug-n-n play ggplot with facet_wrap

tides_mixed_series <- ggplot() + 
  geom_point(data = veg_run,
             aes(x = Time_Since_Restoration, 
                 y = Canopyo_Height_cm,
                 fill = Tidal_Regime),
             size = 5.5, shape = 21) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Canopy Height (cm)") + 
  scale_x_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, 2)) + 
  scale_y_continuous(limits = c(0, 80), 
                     breaks = seq(0, 80, 20)) +
  scale_fill_manual(values = c("orange", "cornflowerblue")) + 
  scale_colour_manual(values = c("orange", "cornflowerblue")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(), 
    legend.text = element_text(size = 18, colour = "black"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Tidal_Regime,
             nrow = 2, ncol = 1,
             scales = "free")

tides_mixed_series


ggsave(tides_mixed_series, 
       height = 14, width = 10, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Tidal Range\\Canopy Tides - Panel.jpg")





tides_mixed_series <- ggplot(data = canopy.preds,
                                 aes(x = Timeline,
                                     y = canopy_height)) + 
    geom_ribbon(aes(ymin = conf.low, 
                   ymax = conf.high),
              alpha = 0.25) +
  geom_line(linewidth = 1.5, linetype = "dashed",) +
  geom_point(data = veg_run,
             aes(x = Time_Since_Restoration, 
                 y = Canopy_Height_cm,
                 fill = Tidal_Regime),
             size = 5.5, shape = 21) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "Canopy Height (cm)") + 
  scale_x_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, 2)) + 
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c("orange", "cornflowerblue")) + 
  theme_bw() +
  theme(
    legend.position = c(0.10, 0.90),
    legend.title = element_blank(), 
    legend.text = element_text(size = 18, colour = "black"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black"))

tides_mixed_series


ggsave(tides_mixed_series, 
       height = 9, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Tidal Range\\Canopy Height Tides - Combined.jpg")



















# Project: Atlantic Coast Joint Venture - Assessment of Runnel Effectiveness
# Analysis: Vegetation Analysis
# Script: Script 3 - Time Series Regression with Marsh Degradation Factor

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



# Chapter 3: Visualize the distribution of the dataset --------------------------------

# Graph a histogram of the number of sites in each marsh condition to understand the 
# distribution of sites in the analysis

# Calculate the overview of the condition dataset

veg_condition_overview <- veg_run %>%
      summarise(
        Sites = length(unique(Site_Treatment_ID)),
        Estuary = length(unique(Salt_Marsh)),
        Seasons = length(unique(Season)),
        Age = length(unique(Time_Since_Restoration)),
        Age_Span = paste(min(Time_Since_Restoration), max(Time_Since_Restoration), sep = " - "))

glimpse(veg_condition_overview)


# Calculate the number of sites for each year and each marsh condition
veg_condition_count <- veg_run %>%
  group_by(Time_Since_Restoration, Marsh_Condition) %>%
  summarise(Site_Count = n()) %>%
  ungroup()

glimpse(veg_condition_count)


# Graph the distribution of sites
condition_histogram <- ggplot(
  aes(x = Time_Since_Restoration, 
      y = Site_Count,
      group = Marsh_Condition), 
  data = veg_condition_count) + 
  geom_bar(aes(fill = Marsh_Condition),
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
  facet_wrap(~Marsh_Condition,
             nrow = 2)

condition_histogram



ggsave(condition_histogram, 
       height = 8, width = 12, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Marsh Condition Site Count Histogram.jpg")



# Chapter 4: Mixed Linear Time Series with Marsh Condition -----------------

# The second inspection of the vegetation dataset involves understanding how the marsh
# initial condition impacts the restoration trajectory. The vegetation dataset is reduced
# to just the runnel treatment. 

# Model Components
  # Fixed Effects: Time, Marsh Condition, Interaction
  # Random Effects: Site Treatment ID


# Task 1: Spartina alterniflora cover over time

# Create the Spartina alterniflora cover model

spalt_mod <- lmer(SPALT ~ Time_Since_Restoration * Marsh_Condition + (1| Site_Treatment_ID),
                  data = veg_run)

anova(spalt_mod)  

spalt_anova <- broom.mixed::tidy(anova(spalt_mod)) %>%
  mutate(model = "SPALT")


# Time was approaching significance in the model

spalt.preds <- ggpredict(spalt_mod, terms = c("Time_Since_Restoration [all]"),
                         type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         SPALT = predicted)

spalt.preds$conf.low[spalt.preds$conf.low < 0] <- 0

glimpse(spalt.preds)

# Task 2: High Marsh Graminoid regression

hmg_mod <- lmer(high_marsh_grams ~ Time_Since_Restoration * Marsh_Condition + (1|Site_Treatment_ID),
                data = veg_run)

anova(hmg_mod)

hmg_anova <- broom.mixed::tidy(anova(hmg_mod)) %>%
  mutate(model = "High Marsh Graminoid")


#Note - the interaction of Time and Marsh condition was approaching significance

hmg.preds <- ggpredict(hmg_mod, terms = c("Time_Since_Restoration [all]", "Marsh_Condition [all]"),
                       type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         high_marsh_grams = predicted,
         Marsh_Condition = group)

# Task 3: Abiotic  cover over time

# Create the abiotic cover model

abiotic_mod <- lmer(abiotic_cover ~ Time_Since_Restoration * Marsh_Condition + (1|Site_Treatment_ID),
                    data = veg_run)

anova(abiotic_mod)  

abiotic_anova <- broom.mixed::tidy(anova(abiotic_mod)) %>%
  mutate(model = "Abiotic Cover")

tidy(abiotic_mod)

#Note - the only significant factor was Time

abiotic.preds <- ggpredict(abiotic_mod, terms = c("Time_Since_Restoration [all]"),
                           type = "fixed", interval = 'confidence') %>%
  rename(Timeline = x,
         abiotic_cover = predicted)

abiotic.preds$conf.low[abiotic.preds$conf.low < 0] <- 0

glimpse(abiotic.preds)

# Task 4: Canopy height over time

# Create the Canopy Height model

canopy_mod <- lmer(Canopy_Height_cm ~  Time_Since_Restoration * Marsh_Condition + (1| Site_Treatment_ID),
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

ratio_mod <- lmer(HM_Ratio ~  Time_Since_Restoration * Marsh_Condition + (1| Site_Treatment_ID),
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
          "Output Stats\\Marsh Condition Linear Mixed Time Series ANOVAs.csv")


# Chapter 4: Graph the marsh condition mixed models -----------------------------

# Create a plug-n-n play ggplot with facet_wrap

condition_mixed_series <- ggplot(data = hmg.preds,
                            aes(x = Timeline,
                                y = high_marsh_grams,
                                group = Marsh_Condition)) + 
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high,
                  fill = Marsh_Condition),
              alpha = 0.25) +
  geom_line(aes(colour = Marsh_Condition),
    linewidth = 1.5, linetype = "dashed",) +
  geom_point(data = veg_run,
             aes(x = Time_Since_Restoration, 
                 y = high_marsh_grams,
                 fill = Marsh_Condition),
             size = 5.5, shape = 21) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "High Marsh Graminoid Cover (%)") + 
  scale_x_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, 2)) + 
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c("orange", "cornflowerblue")) + 
   scale_colour_manual(values = c("orange", "cornflowerblue")) + 
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

condition_mixed_series


ggsave(condition_mixed_series, 
       height = 9, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Marsh Condition\\High Marsh Graminoid Condition - Combined.jpg")



# Create a plug-n-n play ggplot with facet_wrap

condition_mixed_series <- ggplot(data = ratio.preds,
                                 aes(x = Timeline,
                                     y = HM_Ratio)) + 
#  geom_ribbon(aes(ymin = conf.low, 
 #                 ymax = conf.high),
  #            alpha = 0.25) +
 # geom_line(linewidth = 1.5, linetype = "dashed",) +
  geom_point(data = veg_run,
             aes(x = Time_Since_Restoration, 
                 y = HM_Ratio,
                 fill = Marsh_Condition),
             size = 5.5, shape = 21) +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "High Marsh Ratio") + 
  scale_x_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, 2)) + 
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.20)) +
  scale_fill_manual(values = c("orange", "cornflowerblue")) + 
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
    axis.text = element_text(size = 22.5, colour = "black"))

condition_mixed_series


ggsave(condition_mixed_series, 
       height = 9, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Marsh Condition\\High Marsh Ratio Condition - Combined.jpg")




















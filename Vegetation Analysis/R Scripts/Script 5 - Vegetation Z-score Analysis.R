# Project: Atlantic Coast Joint Venture - Assessment of Runnel Effectiveness
# Analysis: Vegetation Analysis
# Script: Script 5 - Restoration Performance Index

# Author: Grant McKown (james.mckown@unh.edu)

# Purpose:


# Chapter 1: Package Library ---------------------------------------------------


#Data Organization & Formatting
library(tidyverse)
library(stringr)


#Statistics and Data Analysis
library(ggfortify)
library(ggeffects)
library(broom.mixed)
library(lme4)
library(afex)

#Data Visualization
library(ggplot2)
library(viridis)
library(patchwork)


# Chapter 2: Import and Format Site - Treatment Summarized Dataset -----------------------

# Task 1: Import the Dataset Summarized at the Site - Treatment Level

# Note - this is a modified dataset with assignments of no action and reference for each runnel!!
# After data summary in Script 1, I added two new columns - Reference and No_Action - which
# I manually coded the correct corresponding Reference and No Action site for each Runnel site
# for the Restoration Performance Index calculations

veg <- read.csv("Input Data\\Reduced Data Site - Treatment Summary of Vegetation Plot Data.csv") %>%
  rename_with( ~ str_remove(., "_mean"), everything()) %>%
  mutate(Site_Treatment_ID = paste(Site, Site_Treatment, sep = " ")) %>%
  rename(canopy_height = Canopyo_Height_cm)

glimpse(veg)



# Chapter 3: Calculate Restoration Performance Index ---------------------------

# Task 1: Re-configure the data table to join the appropriate reference and no action
# vegetation metric values to each runnel site. 

# In order to easily complete RPI calculations, we will break apart the larger veg table
# into three tables - runnel, reference, and no action. Each table is re-organized and then
# merged back into the runnel table based on the manually coded Reference and No Action columns. 
# Each table is transformed from wide to long where each vegetation metric of each site and season
# is an individual row. This made merging the runnel, reference, and no action tables possible. 


# Create an index table of the runnel treatments

veg_run <- veg %>%
  filter(Overall_Treatment == "Runnel") %>%
  gather(c(SPALT, high_marsh_grams, abiotic_cover, canopy_height),
         key = Metric,
         value = Runnel_Value)

# Create separate datasets for the reference and no action values

veg_ref <- veg %>%
  filter(Overall_Treatment == "Reference") %>%
  select(Site_Treatment_ID, Season, SPALT, high_marsh_grams, abiotic_cover, canopy_height) %>%
    rename(Reference = Site_Treatment_ID) %>%
  gather(c(SPALT:canopy_height),
         key = Metric,
         value = Reference_Value)

veg_nac <- veg %>%
  filter(Overall_Treatment == "No Action") %>%
select(Site_Treatment_ID, Season, SPALT, high_marsh_grams, abiotic_cover, canopy_height) %>%
  rename(No_Action = Site_Treatment_ID) %>%
  gather(c(SPALT:canopy_height),
         key = Metric,
         value = No_Action_Value)



# Lastly, merge the tables together based on:
  # (1) Season
  # (2) Reference or No_Action Assignment Columns
  # (3) Vegetation Metric

veg_rpi_prep <- veg_run %>%
  # Merge the reference table 
  merge(veg_ref, by = c("Reference", "Season", "Metric"), 
        all.x = TRUE) %>%
  # Merge the runnel table
  merge(veg_nac, by = c("No_Action", "Season", "Metric"), 
        all.x = TRUE) %>%
  # Reduce the dataset to only the important columns for further data analysis
  select(Site, Site_Treatment_ID, Sub_Site, Season, Time_Since_Restoration,
         Tidal_Regime, Marsh_Condition, 
         Metric, Runnel_Value, Reference_Value, No_Action_Value) %>%
  # Rename columns for easier coding
  rename(Runnel = Runnel_Value,
         Reference = Reference_Value,
         No_Action = No_Action_Value)

glimpse(veg_rpi_prep)



# Task 2: Calculate the Restoration Performance Index

veg_rpi <- veg_rpi_prep %>%
  mutate(
    # Calculate Restoration Performance Index for each vegetation metric of each site and season
  RPI = (Runnel - No_Action)/ (Reference - No_Action), 
    # Reduce the RPI calculations to the boundaries of 0 - 1
  RPI = ifelse(RPI < 0, 0,
               ifelse(RPI > 1, 1, RPI)),
    # Round the RPI to 2 digits for easier reading
  RPI = round(RPI, 2))

glimpse(veg_rpi)


# Task 3: QAQC RPI scores - ensure that there are no NAs in the dataset

rpi_qaqc <- veg_rpi %>%
  summarise(
    RPI_na_count = sum(is.na(RPI)))

rpi_qaqc


# Task 4: Expand the dataset

# Next, let's transform the dataset from long to wide where each RPI score of each vegetation metric
# is its own column; calculate average RPI score for all four vegetation metrics

veg_rpi_summary <- veg_rpi %>%
  # Reduce the dataset to the important columns for data analysis
  select(Site, Site_Treatment_ID, Sub_Site, Season, Time_Since_Restoration,
         Tidal_Regime, Marsh_Condition, 
         Metric, RPI) %>%
  # Transform the dataset from long to wide for easier calculation of site RPI
  tidyr::spread(Metric, RPI) %>%
  # Calculate site-level RPI by averaging RPI of 4 vegetation metrics
  mutate(RPI_total = (canopy_height + high_marsh_grams + SPALT + abiotic_cover) / 4,
         RPI_total = round(RPI_total, 2)) %>%
  # Add "_RPI" at the end of each column name for easier readier
  rename_with(.cols = c(abiotic_cover:SPALT), ~ paste(., "RPI", sep = "_"))

glimpse(veg_rpi_summary)


write.csv(veg_rpi_summary,
          "Output Stats\\RPI Scores for Vegetation Community.csv")

# Task 5 - Calculate mean +/- standard error for each time point of RPI

rpi_stats <- veg_rpi_summary %>%
  group_by(Time_Since_Restoration) %>%
  summarise(
    across(abiotic_cover_RPI:RPI_total, 
           list(
             mean = ~ mean(., na.rm = TRUE),
             se = ~sd(., na.rm = TRUE) / sqrt(n())  ))) %>%
  mutate(across(abiotic_cover_RPI_mean:RPI_total_se,
                ~round(., 2)))

glimpse(rpi_stats)

write.csv(rpi_stats,
          "Output Stats\\Summarized RPI Dataset Over Time.csv")

# Chapter 4: Mixed linear regressions of Restoration Performance Index -------------------

# Next, I want to conduct mixed linear regressions of RPI scores that may be interesting 
# based on the graphs

# As a catch all and due to limited time, I want to focus on Abiotic Cover and High Marsh Graminoids
  # Models:
    # (1) Overall models
    # (2) Marsh Condition
    # (3) Tidal Regime


# Task 1 - Abiotic Cover - Overall Model

abiotic_overall_mod <- lmer(abiotic_cover_RPI ~ Time_Since_Restoration + (1 | Site_Treatment_ID),
                            data = veg_rpi_summary)

anova(abiotic_overall_mod)

abiotic_anova <- tidy(anova(abiotic_overall_mod)) %>%
  mutate(Metric = "Abiotic",
         Model = "Overall")

# No regression trend was detected for abiotic cover 

# Task 2 - Abiotic - Marsh Condition Model

abiotic_condition_mod <- lmer(abiotic_cover_RPI ~ Time_Since_Restoration * Marsh_Condition + (1 | Site_Treatment_ID),
                              data = veg_rpi_summary)

anova(abiotic_condition_mod)

# Note - time was approaching significance in the model

abiotic_condition_anova <- tidy(anova(abiotic_condition_mod)) %>%
  mutate(Metric = "Abiotic",
         Model = "Condition")

# Task 3 - Abiotic - Tidal Regime model

abiotic_tides_mod <- lmer(abiotic_cover_RPI ~ Time_Since_Restoration * Tidal_Regime + (1 | Site_Treatment_ID),
                          data = veg_rpi_summary)

anova(abiotic_tides_mod)

# Note - no regression trend was detected for abiotic cover

abiotic_tides_anova <- tidy(anova(abiotic_tides_mod)) %>%
  mutate(Metric = "Abiotic",
         Model = "Tides")

# Task 4 - High Marsh Grams- Overall Model

hmg_overall_mod <- lmer(high_marsh_grams_RPI ~ Time_Since_Restoration + (1 | Site_Treatment_ID),
                            data = veg_rpi_summary)

anova(hmg_overall_mod)

# Significant trend over time for high marsh graminoids for overall RPI

hmg_anova <- tidy(anova(hmg_overall_mod)) %>%
  mutate(Metric = "High Marsh Grams",
         Model = "Overall")

# Calculate the predicted values from the model for graphing

hmg_pred <- ggpredict(
  hmg_overall_mod,
  terms = c("Time_Since_Restoration [all]"),
  type = "fixed", interval = "confidence") %>%
  rename(
    Timeline = x,
    high_marsh_grams_RPI = predicted)

hmg_pred$conf.low[hmg_pred$conf.low < 0] <- 0

glimpse(hmg_pred)

# Task 5 - High Marsh Grams - Marsh Condition Model

hmg_condition_mod <- lmer(high_marsh_grams_RPI ~ Time_Since_Restoration * Marsh_Condition + (1 | Site_Treatment_ID),
                              data = veg_rpi_summary)

anova(hmg_condition_mod)

# Only a significant trend over time for high marsh graminoids

hmg_condition_anova <- tidy(anova(hmg_condition_mod)) %>%
  mutate(Metric = "High Marsh Grams",
         Model = "Condition")

# Task 6 - High Marsh Grams - Tidal Regime model

hmg_tides_mod <- lmer(high_marsh_grams_RPI ~ Time_Since_Restoration * Tidal_Regime + (1 | Site_Treatment_ID),
                          data = veg_rpi_summary)

anova(hmg_tides_mod)

# No significant trend detected for high marsh grams

hmg_tides_anova <- tidy(anova(hmg_tides_mod)) %>%
  mutate(Metric = "High Marsh Grams",
         Model = "Tides")


# Task 7 - Combine the ANOVA tables by row and export for further analysis

rpi_anova <- rbind(abiotic_anova, abiotic_condition_anova, abiotic_tides_anova,
                   hmg_anova, hmg_condition_anova, hmg_tides_anova) %>%
  select(Metric, Model, term:p.value)

glimpse(rpi_anova)

write.csv(rpi_anova,
          "Output Stats\\RPI ANOVA - Abiotic & High Marsh Grams.csv")



# Chapter 5: Graph the RPI statistics -------------------------------------------

# Task 1: Graph trends and the scattered distribution of the RPI metrics

# Note - Scatterplot and/or trendlines

rpi_time_series <- ggplot() + 
  geom_ribbon(data = hmg_pred,
              aes(x = Timeline,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.75, fill = "grey") + 
  geom_line(data = hmg_pred,
            aes(x = Timeline,
                y = high_marsh_grams_RPI),
            colour = "black", linewidth = 1.5) + 
  geom_point(data = veg_rpi_summary,
             aes(x = Time_Since_Restoration, 
                 y = high_marsh_grams_RPI),
             size = 5.5, shape = 21,
             colour = "black", fill = "orange") +
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "RPI - High Marsh Graminoid Cover") + 
  scale_x_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, 2)) + 
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.20)) +
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

rpi_time_series


ggsave(rpi_time_series, 
       height = 9, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\RPI\\High Marsh Graminoid RPI - Trendline.jpg")


# Task 2: Graph the summarized RPI metrics 

rpi_summarized_series <- ggplot(
              data = rpi_stats,
              aes(x = Time_Since_Restoration,
                  y = RPI_total_mean))+ 
  geom_errorbar(
    aes(ymin = RPI_total_mean - RPI_total_se,
        ymax = RPI_total_mean + RPI_total_se),
    width = 0.5, size = 1.25) + 
  geom_bar(
      fill = "orange", colour = "black",
      linewidth = 1.25,
      stat = "identity", position = position_dodge(0.90)) + 
  labs(x = "Age Relative to Restoration (yrs)", 
       y = "RPI - Composite Site Score") + 
  scale_x_continuous(limits = c(-0.5, 15), 
                     breaks = seq(0, 15, 2)) + 
  scale_y_continuous(limits = c(0, 1.1), 
                     breaks = seq(0, 1, 0.20),
                     expand = c(0,0)) +
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

rpi_summarized_series


ggsave(rpi_summarized_series, 
       height = 9, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\RPI\\Composite RPI - Summarized.jpg")





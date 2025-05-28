#Project: Trustees Salt Marsh Restoration - Elevation Analysis
#Script: Script 1 - Data Formatting and QAQC
#Author: Grant McKown (james.mckown@unh.edu)

#Script Description: The script imports the compiled elevation datasest for the Trustees, formats column names,
# and QAQCs the elevations with comparsion to 2019 elevations of the transect ends. Subsequent elevation transects are "shifted"
# based on the average difference in the transect ends


# Chapter 1: Package library----------------------------------------------------

#Data Organization
library(tidyverse)
library(dplyr)
library(broom)

#Data visualization
library(ggplot2)
library(patchwork)
library(wesanderson)


# Chapter 2: Import the Elevation Dataset --------------------------------------

Site_Name <- "Jacobs Point"

elevs <- read.csv("Elevation Analysis\\Input Data\\ACJV Elevations Compiled 2021 - 2024.csv") %>%
  filter(Salt_Marsh %in% Site_Name) %>%
  rename(elevation = Elevation_NAVD88m)

glimpse(elevs)

year1_season = min(elevs$Season)


# Chapter 3: Initial Descriptive Statistics of Elevation------------------------

# Before quality control and adjustments to the elevations, first let's conduct an initial inspection
# of the elevation profiles to understand the raw data by calculating mean, min, and max elevation

elevs_initial <- elevs %>%
  group_by(Season, Site_Treatment, Transect) %>%
  summarise(
    mean_elev = mean(elevation,
                     na.rm = TRUE),
    min_elev = min(elevation, 
                   na.rm = TRUE),
    max_elev = max(elevation,
                   na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(mean_elev:max_elev,
                ~round(., 2)))

glimpse(elevs_initial)


write.csv(elevs_initial,
          paste("Elevation Analysis\\Formatted Datasets\\", Site_Name, " Raw Elevations.csv", sep = ""))

#------------------------------------------
#Chapter 3: QAQC Elevation Dataset
#------------------------------------------

#Task 1: Format the dataset, calculate the differences in transect end and start points between Year 1 and Current Season

elevs_qaqc <- elevs %>%
  #Step 1: Remove superflurous elevations including those in the creeks (distance < 0) or NA values
  filter(Distance_m >= 0) %>%
  filter(is.na(Distance_m) != TRUE) %>%
  filter(is.na(elevation) != TRUE) %>%
  #Step 2: Make sure that certain columns are the right data type
  mutate(Distance_m = as.numeric(Distance_m)) %>%
  #Step 3: Calculate the locations of the start and end points of transects in the first year of monitoring
  # We want to compare the elevations of the SAME LOCATIONS. Just a good work around instead of using max() and min() distance
  group_by(Site_Treatment, Transect) %>%
  mutate(
    year1 = min(Season),
    
    year1_transect_start = min(Distance_m[Season == year1]),
    year1_transect_end = max(Distance_m[Season == year1]),
    
    year1_transect_start_elev = elevation[Season == year1 & Distance_m == year1_transect_start],
    year1_transect_end_elev = elevation[Season == year1 & Distance_m == year1_transect_end]) %>%
  ungroup() %>%
  #Step 4: Calculate the elevations of all transect starts and ends over time based on Year 1 Locations
  group_by(Season, Site_Treatment, Transect) %>%
  mutate(
    #Just in case someone forgot to measure a specific distance, the code goes to the nearest distance (min or max)
    transect_start_elev = ifelse(min(Distance_m, na.rm = TRUE) != year1_transect_start,
                                 elevation[Distance_m == min(Distance_m, na.rm = TRUE)],
                                 elevation[Distance_m == year1_transect_start]),
    
    transect_end_elev = ifelse(min(Distance_m, na.rm = TRUE) != year1_transect_end,
                             elevation[Distance_m == max(Distance_m, na.rm = TRUE)],
                             elevation[Distance_m == year1_transect_end]),
    
    transect_start_elev = ifelse(is.na(transect_start_elev) == TRUE,
                                 max(transect_start_elev, na.rm = TRUE),
                                 transect_start_elev),

      transect_end_elev = ifelse(is.na(transect_end_elev) == TRUE,
                             max(transect_end_elev, na.rm = TRUE),
                             transect_end_elev)) %>%
  ungroup() %>%
#Step 4: Calculate the difference in Year 1 and Current Season Transect Elevations, Average the differences
    mutate(
      transect_start_diff = transect_start_elev - year1_transect_start_elev,
       transect_end_diff = transect_end_elev - year1_transect_end_elev,
      transect_endpoints_avg = (transect_start_diff + transect_end_diff) / 2)

glimpse(elevs_qaqc)


#Task 2: Adjust ALL elevations based on the average of the transect endpoints from Year 1
elevs_corrected <- elevs_qaqc %>%
  group_by(Season, Site_Treatment, Transect) %>%
  mutate(
    elev_corrected = elevation - transect_endpoints_avg,
    elev_corrected = round(elev_corrected, 3)) %>%
  ungroup()

glimpse(elevs_corrected)


#Task 3: Export the Formatted QAQC Dataset
write.csv(elevs_corrected,
          paste("Elevation Analysis\\Formatted Datasets\\", Site_Name, " Corrected Elevations.csv", sep = ""))


#Task 4: Generate a summary report of the shifts in elevation by each transect for future analysis

elevs_qaqc_transect <- elevs_qaqc %>%
  group_by(Season, Site_Treatment, Transect) %>%
  summarise(
    transect_shift = -1 * first(transect_endpoints_avg)) %>%
  ungroup() %>%
  mutate(transect_shift = round(transect_shift, 3))

glimpse(elevs_qaqc_transect)

write.csv(elevs_qaqc_transect, 
          paste("Elevation Analysis\\Output Stats\\", Site_Name, " Transect QAQC Report.csv", sep = ""))


#Task 5: Generate a summary report of the shifts in elevation by each treatment for future analysis

elevs_qaqc_treatment  <- elevs_qaqc_transect %>%
  group_by(Season, Site_Treatment) %>%
  summarise(
    transect_shift = mean(transect_shift)) %>%
  ungroup()

elevs_qaqc_treatment_spread <- elevs_qaqc_treatment %>%
  spread(Site_Treatment, transect_shift) %>%
  arrange(Season)

glimpse(elevs_qaqc_treatment)


write.csv(elevs_qaqc_treatment_spread,
          paste("Elevation Analysis\\Formatted Datasets\\", Site_Name, " Treatment Mean QAQC Report.csv", sep = ""))

#Task 5: Visualize the QAQC Report

elevs_qaqc_format <- elevs_qaqc_treatment %>%
  mutate(Season = as.character(Season))

qaqc_viz <- ggplot(data = elevs_qaqc_format,
                              aes(x = Site_Treatment, 
                                  y = transect_shift,
                                  group = Season)) +
  geom_point(aes(fill = Season),
            shape = 21, position = position_jitter(0.1), size = 8) +
  scale_y_continuous(
    limits = c(-0.10, 0.10),
    breaks = seq(-0.10, 0.10, 0.02)) + 
  labs(y = "Transect Shift (m)",
       x = "Treatment") +
  theme_bw() +
  theme(
    legend.position = c(0.925, 0.85),
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.text.x = element_text(size = 14, angle = 15, colour = "black", vjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 18))


qaqc_viz

ggsave(qaqc_viz, 
       height = 10, width = 14, dpi = 300,
       limitsize = FALSE, units = "in",
       filename = paste("Elevation Analysis\\Output Figures\\", Site_Name," ", " QAQC Graph.jpg", sep = ""))















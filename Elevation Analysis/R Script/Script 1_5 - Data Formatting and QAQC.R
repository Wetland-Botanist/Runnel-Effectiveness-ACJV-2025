#Project: Trustees Salt Marsh Restoration - Elevation Analysis
#Script: Script 1 - Data Formatting and QAQC
#Author: Grant McKown (james.mckown@unh.edu)

#Script Description: The script imports the compiled elevation datasest for the Trustees, formats column names,
# and QAQCs the elevations with comparison to 2019 elevations of the transect ends. Subsequent elevation transects are "shifted"
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


# Chapter 2: Import and Merge the Elevation and Vegetation Dataset --------------------------------------

# Task 1: Select the Site the Analysis will be performed on
Site_Name <- "Broad Cove"

# Task 2: Import the elevation dataset and filter to the site
elevs <- read.csv("Input Data\\ACJV Elevations Compiled 2021 - 2024.csv") %>%
  filter(Site %in% Site_Name) %>%
  rename(elevation = Elevation_NAVD88m) %>%
  mutate(Site_Treatment = ifelse(SubSite == "",
                                 paste(Site, Overall_Treatment, sep = " - "),
                                 paste(Site, SubSite, Overall_Treatment, sep = " - ")),
         Distance_m = as.numeric(Distance_m),
         elevation = as.numeric(elevation))

glimpse(elevs)

# Task 3: Import the vegetation dataset
veg <- read.csv("Input Data\\ACJV Vegetation Plot Dataframe.csv") %>%
  filter(Site %in% Site_Name) %>%
  select(-Elevation) %>%
  mutate(Site_Treatment = ifelse(SubSite == "",
                                 paste(Site, Overall_Treatment, sep = " - "),
                                 paste(Site, SubSite, Overall_Treatment, sep = " - ")),
         Distance_m = as.numeric(Distance_m))

glimpse(veg)


# Task 4: Merge the vegetation and elevation datasets by Year, Transect, and Distance

veg_elevs <- veg %>%
  merge(select(elevs,
               Site_Treatment, Year, Transect, Distance_m, elevation, Description),
               by = c("Site_Treatment", "Year", "Transect", "Distance_m"),
        all.x = TRUE) 
  
# Task 5: Inspect the rows of the vegetation dataset that were not merged with elevations

veg_elevs_check1 <- veg_elevs %>%
  select(Site_Treatment, Year, Transect, Distance_m, elevation, Description) %>%
  # Reduce the dataset down to all plots that have a NA assigned to elevation
  filter(is.na(elevation)) %>%
  summarise(length(!is.na(elevation)))

veg_elevs_check1


setdiff(veg$Distance_m, elevs$Distance_m)


# Task 6: Reduce the dataset to just the impacted plots 

elevs_impact <- veg_elevs %>%
  select(Site, SubSite, Overall_Treatment, Site_Treatment, Year, Transect, Distance_m, elevation, Description, Impacted) %>%
  filter(Impacted == "Yes")

glimpse(elevs_impact)
  

veg_elevs_impact_check1 <- veg_elevs %>%
  filter(Impacted == "Yes") %>%
  select(Site_Treatment, Year, Transect, Distance_m, elevation, Description) %>%
  # Reduce the dataset down to all plots that have a NA assigned to elevation
  filter(is.na(elevation)) %>%
  summarise(length(!is.na(elevation)))

veg_elevs_impact_check1


# Chapter 3: Initial Descriptive Statistics of Elevation------------------------

# Before quality control and adjustments to the elevations, first let's conduct an initial inspection
# of the elevation profiles to understand the raw data by calculating mean, min, and max elevation

elevs_initial <- elevs_impact %>%
  group_by(Year, Site_Treatment, Transect) %>%
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
          paste("Output Stats\\", Site_Name, " Raw Elevations Overview.csv", sep = ""))


#Chapter 3: QAQC Elevation Dataset ---------------------------------------------

#Task 1: Calculate the Mean and Standard Error of Impact Plot Elevations, Calculate Difference between Year 1 and Year 2

elevs_mean_transect <- elevs_impact %>%
  #Step 1: Remove superflurous elevations including those in the creeks and low marsh areas (setting distance to 3 m or greater)
  filter(Distance_m >= 0) %>%
  filter(is.na(Distance_m) != TRUE) %>%
  filter(is.na(elevation) != TRUE) %>%
  #Step 2: Make sure that certain columns are the right data type
  mutate(Distance_m = as.numeric(Distance_m)) %>%
  # Step 3: Group and Summarize the impacted plots elevation by transect, year, and treatment
  group_by(Site_Treatment, Transect, Year) %>%
  # Step 4: Calculate the average and standard error
  summarise(
    elevation_mean = mean(elevation, na.rm = TRUE),
    elevation_se = sd(elevation, na.rm = TRUE) / sqrt(n()) ) %>%
  mutate(across(elevation_mean : elevation_se,
                ~round(., 3))) %>%
  # Step 4: Calculate difference in Transects for each transect between Year 1 and Year 2
  group_by(Site_Treatment, Transect) %>%
  mutate(Transect_difference = ifelse(Year == max(Year),
                                      elevation_mean[which(Year == max(Year))] - elevation_mean[which(Year == min(Year))],
                                      0) ) %>%
  ungroup()

glimpse(elevs_mean_transect)

write.csv(elevs_mean_transect,
          paste("Output Stats\\", Site_Name, " QAQC Elevation Report By Transect.csv", sep = ""))

# Task 2: Calculate Mean and Standard ERror of Impact Plot Elevations Across Treatment

elevs_mean_treatment <- elevs_impact %>%
  #Step 1: Remove superflurous elevations including those in the creeks and low marsh areas (setting distance to 3 m or greater)
  filter(Distance_m >= 0) %>%
  filter(is.na(Distance_m) != TRUE) %>%
  filter(is.na(elevation) != TRUE) %>%
  #Step 2: Make sure that certain columns are the right data type
  mutate(Distance_m = as.numeric(Distance_m)) %>%
  # Step 3: Group and Summarize the impacted plots elevation by transect, year, and treatment
  group_by(Site_Treatment, Year) %>%
  # Step 4: Calculate the average and standard error
  summarise(
    elevation_mean = mean(elevation, na.rm = TRUE),
    elevation_se = sd(elevation, na.rm = TRUE) / sqrt(n()) ) %>%
  mutate(across(elevation_mean : elevation_se,
                ~round(., 3))) %>%
  # Step 4: Calculate difference in Transects for each transect between Year 1 and Year 2
  group_by(Site_Treatment) %>%
  mutate(Transect_difference = ifelse(Year == max(Year),
                                      elevation_mean[which(Year == max(Year))] - elevation_mean[which(Year == min(Year))],
                                      0) ) %>%
  ungroup()

glimpse(elevs_mean_treatment)

write.csv(elevs_mean_treatment,
          paste("Output Stats\\", Site_Name, " QAQC Elevation Report By Treatment.csv", sep = ""))




#Task 5: Visualize the QAQC Report

elevs_mean_treatment <- elevs_mean_treatment %>%
  mutate(Year = as.character(Year))

qaqc_viz <- ggplot(data = elevs_mean_treatment,
                              aes(x = Site_Treatment, 
                                  y = Transect_difference * 100,
                                  group = Year)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             size = 1.5) + 
  geom_point(aes(fill = Year),
            shape = 21, position = position_jitter(height = 0, width = 0.2), size = 8) +
  scale_y_continuous(
    limits = c(-20, 20),
    breaks = seq(-20, 20, 5)) + 
  labs(y = "Transect Shift (cm)",
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
       filename = paste("Output Figures\\", Site_Name," ", " QAQC Graph.jpg", sep = ""))















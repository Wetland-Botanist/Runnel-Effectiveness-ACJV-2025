#Project: Trustees Salt Marsh Restoration
#Script: Script 2 - Vegetation Plot Descriptive Statistics
#Author: Grant McKown (james.mckown@unh.edu)

#Script Description: The script calculates standard vegetation metrics (halophyte cover, etc.),
#                    calculates the descriptive statistics, and visualizes the statistics. The
#                    script specifically handles Runnel, No Action, Reference, and the platform
#                    of Ditch Remediation treatments. See Script 3 for ditch center and ditch
#                    edge plots. Descriptive statistics are then visualized with Stacked Bars
#                    and points - error bars of individual treatmetns and overall treatment. 


# Chapter 1: Package library ------

#Data Organization
library(tidyverse)

#Data visualization
library(ggplot2)
library(patchwork)
library(wesanderson)


# Chapter 2: Import and Format Vegetation Dataset ------------------

#Task 1: Prep the dataset

#To prep the dataset for analysis in the script, 

#Select the Trustees site that will be analyzed

Site_Name = "Moody Marsh"

#Import the formatted Trustees Vegetation Dataset from Script 1

veg <- read.csv("Input Data\\ACJV Vegetation Plot Dataframe.csv") %>%
  filter(Site != "") %>%
  mutate(Site_Specific = ifelse(Sub_Site != "", 
                                paste(Site, Sub_Site, sep = " - "),
                                Site),
         Site_Treatment_ID = paste(Site, Site_Treatment, sep = " - ")) %>%
  filter(Site == Site_Name) %>%
  mutate(across(c(MUD:UNK, Canopy_Height_cm_cm),
                ~ifelse(is.na(.), 0, .)))

glimpse(veg)



#Task 2: Calculate the Vegetation Metrics for Analysis

# For data analysis, we will calculate the following parameters based on summation of specific species:
# (1) Live Plant Cover
# (2) Abiotic Cover
# (3) High Marsh Graminoids
# (4) Upper Marsh Border 
# (5) Other species - all species that are not SPALT, high marsh graminoids, and upper marsh border
# (6) Rest species - all species that are not SPALT and high marsh graminoids
# (6) Species Richness (Note, I did not include the unknown category into the richness count)


veg_metrics <- veg %>%
  mutate(
    live_cover = rowSums(select(., c(SPALT:UNK))),
    abiotic_cover = MUD + DEAD + WRACK + DITCH_FILL + GREEN_ALG + ALGAL_MAT + VAUCH + ASCO,
    high_marsh_gram = SPPAT + DISPI + JUGER + AGSTO,
    upper_marsh_border = IVFRU + PAVIR + FERUB + SPPEC + ELREP,
    other_cover = rowSums(select(., -c(Date:TOTAL,
                                       MUD, DEAD, WRACK, DITCH_FILL, GREEN_ALG, ALGAL_MAT, VAUCH, ASCO,
                                       SPALT, SPPAT, DISPI, JUGER, AGSTO,
                                       IVFRU, PAVIR, FERUB, SPPEC, ELREP,
                                        Canopy_Height_cm_cm, Thatch_cm, Site_Specific, Site_Treatment_ID))),
    rest_cover = rowSums(select(., -c(Date:TOTAL,
                                      MUD, DEAD, WRACK, DITCH_FILL, GREEN_ALG, ALGAL_MAT, VAUCH, ASCO,
                                      SPALT, SPPAT, DISPI, JUGER, AGSTO,
                                        Canopy_Height_cm_cm, Thatch_cm, Site_Specific, Site_Treatment_ID))),
    richness = rowSums(select(., c(SPALT:UNK)) > 0))

glimpse(veg_metrics)


# Chapter 3: Calculate Descriptive Statistics of Vegetation Metrics ------------------


# Descriptive statistics (mean +/- standard error) are calculated for the following vegetation metrics:

# (1) Abiotic cover (with algae cover)
# (2) Live cover
# (3) Spartina alterniflora cover
# (4) High marsh gramminoid cover
# (5) Upper marsh border cover
# (6) Other species cover 
# (7) Rest species
# (8) Species richness
# (9) Canopy height

# Descriptive statistics are calculated at two levels:

# (1) Individual Treatment (e.g., Runnel 1, North Ditch Remediation, etc.)
# (2) Overall Treatment (Runnel, Ditch Remediation, etc.)

# Task 1: Descriptive statistics at the treatment level, not overall treatment

# Calculate the descriptive statistics of individual treatments

veg_summarised <- veg_metrics %>%
  group_by(Site, Season, Overall_Treatment) %>%
  summarise(across(c(Elevation, abiotic_cover, live_cover, SPALT, high_marsh_gram,
                     upper_marsh_border, other_cover, rest_cover, richness, Canopy_Height_cm_cm),
                   list(
                     mean = ~ mean(., na.rm = TRUE),
                     se = ~ sd(., na.rm = TRUE)/sqrt(n())
                     ))) %>%
  mutate(across(abiotic_cover_mean:Canopy_Height_cm_cm_se,
                ~round(., 1))) %>%
  ungroup()


glimpse(veg_summarised)


# Export the descriptive statistics for reporting and analysis

write.csv(veg_summarised,
          file = paste("Output Stats\\", Site_Name, " Vegetation Descriptive Stats - Site Treatment.csv", sep = ""))



# Task 2: Descriptive statistics at the overall treatment

#Calculate the descriptive statistics at the Overall Treatment level

veg_summarised_overall <- veg_metrics %>%
  group_by(Site, Season, Overall_Treatment) %>%
  summarise(across(c(abiotic_cover, live_cover, SPALT, high_marsh_gram,
                     upper_marsh_border, other_cover, rest_cover, richness, Canopy_Height_cm),
                   list(
                     mean = ~ mean(., na.rm = TRUE),
                     se = ~ sd(., na.rm = TRUE)/sqrt(n())
                   ))) %>%
  mutate(across(abiotic_cover_mean:Canopy_Height_cm_se,
                ~round(., 1))) %>%
  ungroup()


glimpse(veg_summarised_overall)


# Export the descriptive statistics for reporting and analysis

write.csv(veg_summarised_overall,
          file = paste("Output Stats\\", Site_Name, " Vegetation Stats - Overall Treatment.csv", sep = ""))




# Chapter 4: Visualize the Descriptive Statistics of Individual Treatments ------------------

# Descriptive statistics are visualized as:

# (1) Stacked bar charts showing the vegetation community composition over time for each treatment

# (2) Bar charts with error bars showing changes in vegetation community with error graphed

#Descriptive Statistics are visualized at two levels:

# (1) Individual Treatments
# (2) Overall Treatments


#To create the stacked bar charts appropriate with facet_wrap, the dataset needs to be transformed
# from a wide dataset to a long dataset. I take a two-step process of gathering the average
# visual cover of each vegetation metric and then gather the standard error of each vegetation metric. 


#Task 1: Transform the descriptive stats dataset from wide to long for data visualization


# Gather the average cover of each cover type into one comprehensive column, arrange by characterizing columns,
# and rename the cover type values into proper titles for graphs

visualization_mean <- veg_summarised %>%
  select(-richness_mean, -richness_se, -Canopy_Height_cm_mean, -Canopy_Height_cm_se, -live_cover_mean, -live_cover_se, -other_cover_se, -other_cover_mean) %>%
  select(-abiotic_cover_se, -SPALT_se, -high_marsh_gram_se, -upper_marsh_border_se, -rest_cover_se) %>%
  gather(key = "Species", value = "Average_Cover", 
         abiotic_cover_mean, SPALT_mean, high_marsh_gram_mean, upper_marsh_border_mean, rest_cover_mean) %>%
  arrange(Season, Overall_Treatment, Species) %>%
  mutate(Species = ifelse(Species == "high_marsh_gram_mean", "High Marsh Graminoids", Species),
         Species = ifelse(Species == "upper_marsh_border_mean", "High Marsh Border", Species),
         Species = ifelse(Species == "abiotic_cover_mean", "Nonlive", Species),
         Species = ifelse(Species == "SPALT_mean", "Spartina alterniflora", Species),
         Species = ifelse(Species == "rest_cover_mean", "Other", Species),
         Species = factor(Species, levels = c("Spartina alterniflora", "High Marsh Graminoids", 
                                              "High Marsh Border", "Nonlive", "Other"))) %>%
  arrange(Season, Overall_Treatment, Species)


# Gather the standard error of each cover type...

visualization_se <- veg_summarised %>%
  select(-richness_mean, -richness_se, -Canopy_Height_cm_mean, -Canopy_Height_cm_se, -live_cover_mean, -live_cover_se, -other_cover_se, -other_cover_mean) %>%
  select(-abiotic_cover_mean, -SPALT_mean, -high_marsh_gram_mean, -upper_marsh_border_mean, -rest_cover_mean) %>%
  gather(key = "Species", value = "SE_Cover", 
         abiotic_cover_se, SPALT_se, high_marsh_gram_se, upper_marsh_border_se, rest_cover_se) %>%
  arrange(Season, Overall_Treatment, Species) %>%
  mutate(Species = ifelse(Species == "high_marsh_gram_se", "High Marsh Graminoids", Species),
         Species = ifelse(Species == "upper_marsh_border_se", "High Marsh Border", Species),
         Species = ifelse(Species == "abiotic_cover_se", "Nonlive", Species),
         Species = ifelse(Species == "SPALT_se", "Spartina alterniflora", Species),
         Species = ifelse(Species == "rest_cover_se", "Other", Species),
         #Assign levels for data visualization
         Species = factor(Species, levels = c("Spartina alterniflora", "High Marsh Graminoids", 
                                              "High Marsh Border", "Nonlive", "Other"))) %>%
  arrange(Season, Overall_Treatment, Species)


# Column bind the two datasets into one continuous datasets

veg_visualization_treatment <- cbind(visualization_mean, 
                           select(visualization_se, SE_Cover))

glimpse(veg_visualization_treatment)


#Task 2: Stacked Bar Charts of vegetation community composition

veg_stacked <- ggplot(veg_visualization_treatment,
                             aes(x = Season, 
                                 y = Average_Cover, 
                                 fill = Species)) + 
  geom_bar(stat = 'identity', 
           width = 0.55, color = "black") + 
  scale_y_continuous(limits = c(0, 101),
                     breaks = seq(0, 101, 25),
                     expand = c(0,0)) +
  scale_fill_manual(values = c("green4", "yellow2", 
                               "dodgerblue", "tan4", "grey")) +
  labs(y = "Vegetation Cover (%)", 
       x = "") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_blank()) +
  facet_wrap(~Treatment, 
             ncol = 2, nrow = 5)

veg_stacked


ggsave(veg_stacked,
       units = "in", limitsize = FALSE, 
       width = 12, height = 14,
       filename = paste("Output Figures\\", Site_Name, " Stacked Bar Chart 2024.jpg", sep = ""))





# Task 3: Visualize descriptive statistics of the individual treatment with points and error bars

# The transformed dataset (wide -> long) is also used for the graph
# The number of vegetation metrics in the graph is reduced to the core three metrics that we are 
# concerned about in salt marsh restoration: 

# (1) Spartina alterniflora (Green)
# (2) High marsh graminoids (Yellow2)
# (3) Non-live Cover (Tan4)

# The number of vegetation metrics were reduced to improve data visualization and maintain focus
# on core vegetation metrics 


# Reduce the transformed descriptive statistics dataset to the core vegetation metrics 

veg_treatment_reduced <- veg_visualization_treatment %>%
  filter(Species == 'Spartina alterniflora' |
           Species == "High Marsh Graminoids" |
           Species == "Nonlive") %>%
  filter(Overall_Treatment != "North No Action")

# Visualize the reduced dataset with points and error bars

veg_treatment_time <- ggplot(
  data = veg_treatment_reduced,
  aes(x = Season,
      y = Average_Cover)) +
  geom_errorbar(aes(
    ymin = Average_Cover - SE_Cover, ymax = Average_Cover + SE_Cover,
    x = Season, colour = Species),
    width = 0.25, linewidth = 1.25) +
  geom_point(aes(
    fill = Species),
    shape = 21, size = 6, stroke = 1.5) + 
  scale_fill_manual(values = c("green4", "yellow2", "tan4")) +
  scale_colour_manual(values = c("green4", "yellow2", "tan4")) +
  labs(x = "Year",
       y = "Visual Cover (%)") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)) +
  scale_x_continuous(
    limits = c(min(veg_treatment_reduced$Season) - 0.25, 
               max(veg_treatment_reduced$Season) + 0.25),
    breaks = seq(min(veg_treatment_reduced$Season), 
                 max(veg_treatment_reduced$Season), 1)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_blank()) +
  facet_wrap(~Overall_Treatment,
             ncol = 2)

veg_treatment_time

ggsave(veg_treatment_time,
       units = "in", limitsize = FALSE, 
       width = 14, height = 9,
       filename = paste("Output Figures\\", Site_Name, " Treatment Over Time 2024.jpg", sep = ""))






# Chapter 5: Visualize the Descriptive Statistics of Overall Treatments -------------------------


# Descriptive statistics are visualized as:

# (1) Stacked bar charts showing the vegetation community composition over time for each treatment

# (2) Bar charts with error bars showing changes in vegetation community with error graphed

#Descriptive Statistics are visualized at two levels:

# (1) Individual Treatments
# (2) Overall Treatments


#To create the stacked bar charts appropriate with facet_wrap, the dataset needs to be transformed
# from a wide dataset to a long dataset. I take a two-step process of gathering the average
# visual cover of each vegetation metric and then gather the standard error of each vegetation metric. 

# Task 1: Gather the average cover of each cover type into one comprehensive column, arrange by characterizing columns,
# and rename the cover type values into proper titles for graphs

visualization_mean <- veg_summarised_overall %>%
  select(-richness_mean, -richness_se, -Canopy_Height_cm_mean, -Canopy_Height_cm_se, -live_cover_mean, -live_cover_se, -other_cover_se, -other_cover_mean) %>%
  select(-abiotic_cover_se, -SPALT_se, -high_marsh_gram_se, -upper_marsh_border_se, -rest_cover_se) %>%
  gather(key = "Species", value = "Average_Cover", 
         abiotic_cover_mean, SPALT_mean, high_marsh_gram_mean, upper_marsh_border_mean, rest_cover_mean) %>%
  arrange(Season, Overall_Treatment, Species) %>%
  mutate(Species = ifelse(Species == "high_marsh_gram_mean", "High Marsh Graminoids", Species),
         Species = ifelse(Species == "upper_marsh_border_mean", "High Marsh Border", Species),
         Species = ifelse(Species == "abiotic_cover_mean", "Nonlive", Species),
         Species = ifelse(Species == "SPALT_mean", "Spartina alterniflora", Species),
         Species = ifelse(Species == "rest_cover_mean", "Other", Species),
         Species = factor(Species, levels = c("Spartina alterniflora", "High Marsh Graminoids", 
                                              "High Marsh Border", "Nonlive", "Other"))) %>%
  arrange(Season, Overall_Treatment, Species)


# Gather the standard error of each cover type...

visualization_se <- veg_summarised_overall %>%
  select(-richness_mean, -richness_se, -Canopy_Height_cm_mean, -Canopy_Height_cm_se, -live_cover_mean, -live_cover_se, -other_cover_se, -other_cover_mean) %>%
  select(-abiotic_cover_mean, -SPALT_mean, -high_marsh_gram_mean, -upper_marsh_border_mean, -rest_cover_mean) %>%
  gather(key = "Species", value = "SE_Cover", 
         abiotic_cover_se, SPALT_se, high_marsh_gram_se, upper_marsh_border_se, rest_cover_se) %>%
  arrange(Season, Overall_Treatment, Species) %>%
  mutate(Species = ifelse(Species == "high_marsh_gram_se", "High Marsh Graminoids", Species),
         Species = ifelse(Species == "upper_marsh_border_se", "High Marsh Border", Species),
         Species = ifelse(Species == "abiotic_cover_se", "Nonlive", Species),
         Species = ifelse(Species == "SPALT_se", "Spartina alterniflora", Species),
         Species = ifelse(Species == "rest_cover_se", "Other", Species),
         #Assign levels for data visualization
         Species = factor(Species, levels = c("Spartina alterniflora", "High Marsh Graminoids", 
                                              "High Marsh Border", "Nonlive", "Other"))) %>%
  arrange(Season, Overall_Treatment, Species)


# Column bind the two datasets into one continuous datasets

veg_visualization_overall <- cbind(visualization_mean, 
                           select(visualization_se, SE_Cover)) %>%
  mutate(Overall_Treatment = ifelse(Site == "Old Town Hill", 
                                    ifelse(Overall_Treatment == "Runnel", "Drainage Enhancement"),
                                           Overall_Treatment))

glimpse(veg_visualization_overall)





#Task 2: Stacked Bar Charts of vegetation community composition

veg_stacked <- ggplot(veg_visualization_overall,
                      aes(x = Season, 
                          y = Average_Cover, 
                          fill = Species)) + 
  geom_bar(stat = 'identity', 
           width = 0.55, color = "black") + 
  scale_y_continuous(limits = c(0, 101),
                     breaks = seq(0, 101, 25),
                     expand = c(0,0)) +
  scale_fill_manual(values = c("green4", "yellow2", 
                               "dodgerblue", "tan4", "grey")) +
  labs(y = "Vegetation Cover (%)", 
       x = "") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_blank()) +
  facet_wrap(~Overall_Treatment, 
             ncol = 1, nrow = 5)

veg_stacked


ggsave(veg_stacked,
       units = "in", limitsize = FALSE, 
       width = 12, height = 14,
       filename = paste("Output Figures\\", Site_Name, " Overall Stacked Bar Chart 2024.jpg", sep = ""))




# Task 3: Visualize descriptive statistics of the overall treatment with points and error bars

# The transformed dataset (wide -> long) is also used for the graph
# The number of vegetation metrics in the graph is reduced to the core three metrics that we are 
# concerned about in salt marsh restoration: 

# (1) Spartina alterniflora (Green)
# (2) High marsh graminoids (Yellow2)
# (3) Non-live Cover (Tan4)

# The number of vegetation metrics were reduced to improve data visualization and maintain focus
# on core vegetation metrics 


# Use the 'veg_visualization_treatment' table to average +/- se and graph the individual treatments over time

veg_overall_reduced <- veg_visualization_overall %>%
  filter(Species == 'Spartina alterniflora' |
           Species == "High Marsh Graminoids" |
           Species == "Nonlive")


veg_overall_time <- ggplot(
  data = veg_overall_reduced,
  aes(x = Season,
      y = Average_Cover)) +
  geom_errorbar(aes(
    ymin = Average_Cover - SE_Cover, ymax = Average_Cover + SE_Cover,
    x = Season, colour = Species),
    width = 0.25, linewidth = 1.25) +
  geom_point(aes(
    fill = Species),
    shape = 21, size = 6, stroke = 1.5) + 
  scale_fill_manual(values = c("green4", "yellow2", "tan4")) +
  scale_colour_manual(values = c("green4", "yellow2", "tan4")) +
  labs(x = "Year",
       y = "Visual Cover (%)") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)) +
  scale_x_continuous(
    limits = c(min(veg_treatment_reduced$Season) - 0.25, 
               max(veg_treatment_reduced$Season) + 0.25),
    breaks = seq(min(veg_treatment_reduced$Season), 
                 max(veg_treatment_reduced$Season), 1)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_blank()) +
  facet_wrap(~Overall_Treatment,
             ncol = 2)

veg_overall_time

ggsave(veg_overall_time,
       units = "in", limitsize = FALSE, 
       width = 14, height = 10,
       filename = paste("Output Figures\\", Site_Name, " Treatment Overall Time 2024.jpg", sep = ""))


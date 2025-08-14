#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Script 8 - Analysis of Tidal Datums

#Authors: Grant McKown (james.mckown@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire




# Chapter 1: Library of packages for necessary work -------------

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(stringr)
library(broom)
library(broom.mixed)

#Data Analysis Packages
library(lme4)
library(ggeffects)
library(afex)
library(emmeans)
library(multcomp)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#Data Visualization
library(ggplot2)
library(scales)
library(viridis)
library(patchwork)


# Chapter 2: Import Tidal Datum Dataset ----------------------------------------

tides <- read.csv("Input Data\\Compiled Tidal Datums 2021 - 2024.csv") %>%
  mutate(Year = as.character(Year)) %>%
  # Relativize the tidal datums by tidal range for analysis
  mutate(MHT_rel = (MHT_NAVD88m / Tidal_Range_m) * 100,
         MHHT_rel = (MHHT_NAVD88m / Tidal_Range_m) * 100,
         SpringTide_rel = (SpringTide_NAVD88m / Tidal_Range_m) * 100)

glimpse(tides)


# Chapter 3: ANOVA of Tidal Datums Over Time

# Task 1: ANOVA of Mean Higher Tide over time

mht_mod <- lmer(MHT_rel ~ Year + (1 | Site),
                data = tides) 

mht_anova <- broom.mixed::tidy(anova(mht_mod))

mht_anova

mht_tukey <- emmeans(mht_mod, pairwise ~ Year,
                     adjust = "tukey")

cld(mht_tukey)





# Chapter 4: Visualize the Tidal Datums Over Time

mht_plot <- ggplot(
  aes(x = Year,
      y = MHHT_rel),
  data = tides) + 
  geom_point(
    aes(fill = Year),
    size = 5, colour = "black", shape = 21,  
    position = "jitter") + 
  geom_boxplot(
    width = 0.75, size = 1.5,
    alpha = 0.25) + 
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Mean High Tide Standardized by Tidal Range (%)") + 
  scale_color_manual(values = c("orange", "darkgreen", "cornflowerblue" )) + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 13, colour = "black")) 

mht_plot





gw <- read.csv("Input Data\\ACJV Master Hydrology Flooding Metrics.csv") %>%
  #Remove miscellaneous columns from the dataset
  dplyr::select(-Lingering.Questions, -Analysis.Notes, -Creek, -Spring_Tide_Adjustment_cm, -WLR.Name,
                -Year_0, ) %>%
  # Remove water level recorders that malfunctioned or data was not trusted
  filter(Include_Analysis == "Yes") %>%
  # Create the Site-Treatment column for mixed linear modeling random factor
  mutate(Site_Treatment = ifelse(Sub_Site == "",
                                 paste(Site, Overall_Treatment, sep = " - "),
                                 paste(Site, Sub_Site, Overall_Treatment, sep = " - "))) %>%
  group_by(Site_Treatment) %>%
  summarise(
    Platform_Flooding_Frequency_diff = Platform_HT_Frequency[which.max(Year)] - Platform_HT_Frequency[which.min(Year)] )
  #mutate(Platform_Flooding_Frequency_diff = abs(Platform_Flooding_Frequency_diff)) %>%
  summarise(
    flooding_mean = mean(Platform_Flooding_Frequency_diff, na.rm = TRUE),
    flooding_se = sd(Platform_Flooding_Frequency_diff, na.rm = TRUE) / sqrt(n())
  )

gw


write.csv(gw, "Output Stats\\Flooding Frequency Difference.csv")










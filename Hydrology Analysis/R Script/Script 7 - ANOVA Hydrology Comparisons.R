#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Script 7 - Mixed Linear ANOVA Comparisons

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

# Correlation Analysis
library(corrplot)
library(GGally)
library(Hmisc)



# Chapter 2: Import Hydrology Dataset and Format ------------------------------

# Task 1: Import and format the groundwater dataset

hydrology <- read.csv("Input Data\\ACJV Master Hydrology Flooding Metrics.csv") %>%
  #Remove miscellaneous columns from the dataset
  dplyr::select(-Lingering.Questions, -Analysis.Notes, -Creek, -Spring_Tide_Adjustment_cm, -WLR.Name,
                -Year_0, ) %>%
  # Remove water level recorders that malfunctioned or data was not trusted
  filter(Include_Analysis == "Yes") %>%
  # Create the Site-Treatment column for mixed linear modeling random factor
  mutate(Site_Treatment = ifelse(Sub_Site == "",
                                 paste(Site, Overall_Treatment, sep = " - "),
                                 paste(Site, Sub_Site, Overall_Treatment, sep = " - "))) %>%
  # Convert Drainage Depth and Drainage Amplitude to centimeters
  mutate(Root_Drainage_Depth_cm = Root_Drainage_Depth_m * 100,
         Platform_Drainage_Depth_cm = Platform_Drainage_Depth_m * 100, 
         Drainage_Amplitude_cm = Drainage_Amplitude_m * 100) %>%
  # Rearrange dataframe for easier viewing and possible export
  dplyr::select(Site, Sub_Site, Overall_Treatment, Site_Treatment,
                Year, Time_Since_Restoration, Tidal_Range, 
                Platform_Flood_Duration, Platform_HT_Frequency, Platform_Drainage_Depth_cm,
                Root_Flood_Duration, Root_HT_Frequency, Root_Drainage_Depth_cm, Drainage_Amplitude_cm) %>%
  mutate(Time_Relative_To_Metonic_Low_2015 = Year - 2015)

glimpse(hydrology)  


write.csv(hydrology, "Formatted Datasets\\Formatted Root Zone Hydrology Flooding Metrics.csv")

# Task 2: Create the Age Classifications for the different treatments

hydrology <- hydrology %>%
  mutate(Age_Classification = ifelse(Overall_Treatment == "Runnel" & Time_Since_Restoration <= 4,
                                     "Early Restoration", 
                                     ifelse(Overall_Treatment == "Runnel" & Time_Since_Restoration > 4,
                                      "Older Restoration",
                                      ifelse(Overall_Treatment == "Reference", "Reference", "Pre-Restoration"))))


glimpse(hydrology)


# Task 3: Calculate descriptive statistics according to age classification

hydrology_age_descrip <- hydrology %>%
  group_by(Age_Classification) %>%
  summarise(across(Platform_Flood_Duration:Drainage_Amplitude_cm, 
                   list(
                     mean = ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE) / sqrt(n())
                   ))) %>%
  mutate(across(Platform_Flood_Duration_mean:Drainage_Amplitude_cm_se,
                ~round(., 1)))

glimpse(hydrology_age_descrip)     



hydrology_descrip_format <- hydrology_age_descrip %>%
  mutate(
    Platform_Flood_Duration = paste(Platform_Flood_Duration_mean, Platform_Flood_Duration_se, sep = " +/- "),
    Platform_HT_Frequency = paste(Platform_HT_Frequency_mean, Platform_HT_Frequency_se, sep = " +/- "),
    Platform_Drainage_Depth_cm = paste(Platform_Drainage_Depth_cm_mean, Platform_Drainage_Depth_cm_se, sep = " +/- "),
    Root_Flood_Duration = paste(Root_Flood_Duration_mean, Root_Flood_Duration_se, sep = " +/- "),
    Root_HT_Frequency = paste(Root_HT_Frequency_mean, Root_HT_Frequency_se, sep = " +/- "),
    Root_Drainage_Depth_cm = paste(Root_Drainage_Depth_cm_mean, Root_Drainage_Depth_cm_se, sep = " +/- "),
    Drainage_Amplitude_cm = paste(Drainage_Amplitude_cm_mean, Drainage_Amplitude_cm_se, sep = " +/- ")) %>%
  dplyr::select(
    Age_Classification, Platform_Flood_Duration, Platform_HT_Frequency, Platform_Drainage_Depth_cm,
    Root_Flood_Duration, Root_HT_Frequency, Root_Drainage_Depth_cm, Drainage_Amplitude_cm)

glimpse(hydrology_descrip_format)

write.csv(hydrology_descrip_format,
          "Output Stats\\Hydrology Descriptive Stats by Age Classification.csv")

# Chapter 3: Mixed Linear ANOVA Comparison ------------------------------------

# Conducting ANOVAs to compare the hydrology of pre-restoration (No Action),
# early restoration (Runnel aged <= 4 years), older restoration (Runnel aged > 4 years),
# and restoration goals (Reference). 

# ANOVAs are conducted for Root Zone Drainage Depth and Drainage Amplitude


# Task 1: Mixed Linear ANOVA for Root Zone Drainage Depth

drain_mod <- lmer(Root_Drainage_Depth_cm ~ Age_Classification + (1|Site_Treatment),
                  data = hydrology)

drain_anova <- broom.mixed::tidy(anova(drain_mod))%>%
  mutate(Model = "Root Zone Drainage Depth")

drain_anova


drain_tukey <- emmeans(drain_mod, pairwise ~ Age_Classification,
                       adjust = "tukey")

cld(drain_tukey)


# Task 2: Mixed Linear ANOVA for Platform Flooding Frequency

platform_ht_mod <- lmer(Platform_HT_Frequency ~ Age_Classification + (1|Site_Treatment),
                  data = hydrology)

platform_ht_anova <- broom.mixed::tidy(anova(platform_ht_mod)) %>%
  mutate(Model = "Platform Flooding Frequency")

platform_ht_anova



# Task 3: Mixed Linear ANOVA for Root Zone Flooding Duration

flood_mod <- lmer(Root_Flood_Duration ~ Age_Classification + (1|Site_Treatment),
                        data = hydrology)

flood_anova <- broom.mixed::tidy(anova(flood_mod)) %>%
  mutate(Model = "Root Zone Flooding Duration")

flood_anova

flood_tukey <- emmeans(flood_mod, pairwise ~ Age_Classification,
                       adjust = "tukey")

cld(flood_tukey)


# Task 4: Mixed Linear ANOVA for Drainage Amplitude

amplitude_mod <- lmer(Drainage_Amplitude_cm ~ Age_Classification + (1|Site_Treatment),
                  data = hydrology)

amplitude_anova <- broom.mixed::tidy(anova(amplitude_mod)) %>%
  mutate(Model = "Drainage Amplitude")

amplitude_anova

amplitude_tukey <- emmeans(amplitude_mod, pairwise ~ Age_Classification,
                       adjust = "tukey")

cld(amplitude_tukey)


# Task 5: Combine the ANOVA tables together

anova_compiled <- platform_ht_anova %>%
  rbind(flood_anova, drain_anova, amplitude_anova)

glimpse(anova_compiled)

write.csv(anova_compiled,
          "Output Stats\\Hydrology Comparisons ANOVA Compiled.csv")

# Chapter 4: Visualization of Age Classification Breakdown of Dataset by Bar Graph ------------------------

hydrology_age_mean <- hydrology_age_descrip %>%
  gather(c(Platform_HT_Frequency_mean, Root_Flood_Duration_mean, 
         Root_Drainage_Depth_cm_mean, Drainage_Amplitude_cm_mean),
         key = Hydrology_Metric, 
         value = Mean_Metric) %>%
  select(-c(Platform_Flood_Duration_mean:Drainage_Amplitude_cm_se)) %>%
  mutate(Hydrology_Metric = str_remove(Hydrology_Metric, "_mean$"))

hydrology_age_se <- hydrology_age_descrip %>%
  gather(c(Platform_HT_Frequency_se, Root_Flood_Duration_se, 
           Root_Drainage_Depth_cm_se, Drainage_Amplitude_cm_se),
         key = Hydrology_Metric, 
         value = SE_Metric) %>%
  select(-c(Platform_Flood_Duration_mean:Drainage_Amplitude_cm_mean)) %>%
  mutate(Hydrology_Metric = str_remove(Hydrology_Metric, "_se$"))


hydrology_age_viz <- hydrology_age_mean %>%
  merge(hydrology_age_se,
        c("Age_Classification", "Hydrology_Metric")) %>%
  mutate(Hydrology_Metric = ifelse(Hydrology_Metric == "Platform_HT_Frequency",
                                   "Platform Flooding Frequency",
                                   ifelse(Hydrology_Metric == "Root_Drainage_Depth_cm",
                                          "Root Zone Drainage Depth",
                                          ifelse(Hydrology_Metric == "Root_Flood_Duration",
                                                 "Root Zone Flooding Duration", 
                                                 "Drainage Amplitude")))) %>%
  mutate(Age_Classification = factor(Age_Classification,
                                     levels = c("Pre-Restoration", "Early Restoration",
                                                "Older Restoration", "Reference")))
        



# Task 2: Visualize Platform Flooding Frequency (%)

platform_freq_plot <- ggplot(
  aes(x = Age_Classification,
      y = Mean_Metric,
      fill = Age_Classification),
  data = filter(hydrology_age_viz,
                Hydrology_Metric == "Platform Flooding Frequency")) + 
  geom_errorbar(
    aes(y = Mean_Metric,
        ymin = Mean_Metric - SE_Metric,
        ymax = Mean_Metric + SE_Metric),
    width = 0.75, size = 1.5, colour = "black") + 
  geom_bar(
    stat = "identity", position = position_dodge(0.9),
    size = 1.5, colour = "black") + 
  scale_y_continuous(limits = c(0, 102), 
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Platform Flooding Frequency (%)") + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 13, colour = "black")) 

platform_freq_plot




# Task 3: Visualize Root Zone Flooding Duration (%)

root_flood_dur_plot <- ggplot(
  aes(x = Age_Classification,
      y = Mean_Metric,
      fill = Age_Classification),
  data = filter(hydrology_age_viz,
                Hydrology_Metric == "Root Zone Flooding Duration")) + 
  geom_errorbar(
    aes(y = Mean_Metric,
        ymin = Mean_Metric - SE_Metric,
        ymax = Mean_Metric + SE_Metric),
    width = 0.75, size = 1.5, colour = "black") + 
  geom_bar(
    stat = "identity", position = position_dodge(0.9),
    size = 1.5, colour = "black") + 
  scale_y_continuous(limits = c(0, 102), 
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Root Zone Flooding Duration (%)") + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 13, colour = "black")) 

root_flood_dur_plot


# Task 4: Visualize Root Zone Drainage Depth (cm)

root_drain_depth_plot <- ggplot(
  aes(x = Age_Classification,
      y = Mean_Metric,
      fill = Age_Classification),
  data = filter(hydrology_age_viz,
                Hydrology_Metric == "Root Zone Drainage Depth")) + 
  geom_errorbar(
    aes(y = Mean_Metric,
        ymin = Mean_Metric - SE_Metric,
        ymax = Mean_Metric + SE_Metric),
    width = 0.75, size = 1.5, colour = "black") + 
  geom_bar(
    stat = "identity", position = position_dodge(0.9),
    size = 1.5, colour = "black") + 
  scale_y_continuous(limits = c(-10, 10), 
                     breaks = seq(-10, 10, 5),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Root Zone Drainage Depth (cm)") + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 13, colour = "black")) 

root_drain_depth_plot



# Task 5: Visualize Drainage Amplitude (cm)

drain_amplitude_plot <- ggplot(
  aes(x = Age_Classification,
      y = Mean_Metric,
      fill = Age_Classification),
  data = filter(hydrology_age_viz,
                Hydrology_Metric == "Drainage Amplitude")) + 
  geom_errorbar(
    aes(y = Mean_Metric,
        ymin = Mean_Metric - SE_Metric,
        ymax = Mean_Metric + SE_Metric),
    width = 0.75, size = 1.5, colour = "black") + 
  geom_bar(
    stat = "identity", position = position_dodge(0.9),
    size = 1.5, colour = "black") + 
  scale_y_continuous(limits = c(0, 20), 
                     breaks = seq(0, 20, 5),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Drainage Amplitude (cm)") + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 13, colour = "black")) 

drain_amplitude_plot



# Task 6: Combine for one big plot!

hydrology_plot <- (platform_freq_plot + root_flood_dur_plot) / (root_drain_depth_plot + drain_amplitude_plot)

hydrology_plot

ggsave(hydrology_plot,
       height = 10, width = 14, dpi = 300,
       filename = "Output Figures\\Composite Hydrology Bar Graphs.jpg")






# Chapter 5: Visualization of Age Classification Breakdown of Dataset by Box Plot ------------------------

hydrology$Age_Classification <- factor(hydrology$Age_Classification,
                                          levels = c("Pre-Restoration", "Early Restoration",
                                                     "Older Restoration", "Reference"))



# Task 2: Visualize Platform Flooding Frequency (%)

platform_freq_plot <- ggplot(
  aes(x = Age_Classification,
      y = Platform_HT_Frequency),
  data = hydrology) + 
  geom_point(
    aes(fill = Age_Classification),
    size = 5, colour = "black", shape = 21,  
    position = "jitter") + 
  geom_boxplot(
    width = 0.75, size = 1.5,
    alpha = 0.25) + 
  scale_y_continuous(limits = c(0, 102), 
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Platform Flooding Frequency (%)") + 
  scale_color_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 13, colour = "black")) 

platform_freq_plot




# Task 3: Visualize Root Zone Flooding Duration (%)

root_flood_dur_plot <- ggplot(
  aes(x = Age_Classification,
      y = Root_Flood_Duration),
  data = hydrology) + 
  geom_point(
    aes(fill = Age_Classification),
    size = 5, colour = "black", shape = 21,  
    position = "jitter") + 
  geom_boxplot(
    width = 0.75, size = 1.5,
    alpha = 0.25) + 
  scale_y_continuous(limits = c(0, 102), 
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Root Zone Flooding Duration (%)") + 
  scale_color_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 13, colour = "black")) 

root_flood_dur_plot


# Task 4: Visualize Root Zone Drainage Depth (cm)

root_drain_depth_plot <- ggplot(
  aes(x = Age_Classification,
      y = Root_Drainage_Depth_cm),
  data = hydrology) + 
  geom_point(
    aes(fill = Age_Classification),
    size = 5, colour = "black", shape = 21,  
    position = "jitter") + 
  geom_boxplot(
    width = 0.75, size = 1.5,
    alpha = 0.25) + 
  scale_y_continuous(limits = c(-25, 25), 
                     breaks = seq(-25, 25, 5),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Root Zone Drainage Depth (cm)") + 
  scale_color_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 13, colour = "black")) 

root_drain_depth_plot




# Task 5: Visualize Drainage Amplitude (cm)

drain_amplitude_plot <- ggplot(
  aes(x = Age_Classification,
      y = Drainage_Amplitude_cm),
  data = hydrology) + 
  geom_point(
    aes(fill = Age_Classification),
    size = 5, colour = "black", shape = 21,  
    position = "jitter") + 
  geom_boxplot(
    width = 0.75, size = 1.5,
    alpha = 0.25) + 
  scale_y_continuous(limits = c(0, 32), 
                     breaks = seq(0, 30, 5),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Drainage Amplitude (cm)") + 
  scale_color_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue", "yellow")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 13, colour = "black")) 

drain_amplitude_plot


# Task 6: Combine for one big plot!

hydrology_plot <- (platform_freq_plot + root_flood_dur_plot) / (root_drain_depth_plot + drain_amplitude_plot)

hydrology_plot

ggsave(hydrology_plot,
       height = 10, width = 14, dpi = 300,
       filename = "Output Figures\\Composite Hydrology Box Plots.jpg")






























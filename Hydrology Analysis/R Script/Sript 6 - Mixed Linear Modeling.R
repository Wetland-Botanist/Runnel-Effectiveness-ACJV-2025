#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 6 - Mixed Linear Modeling of Flooding and Drainage Metrics

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

#DescTools is a special package that allows for some cool functions
library(DescTools)

#Data Visualization
library(ggplot2)
library(scales)
library(viridis)

# Correlation Analysis
library(corrplot)
library(GGally)
library(Hmisc)



# Chapter 2: Import Hydrology Dataset and Format ------------------------------

# Import and format the groundwater dataset

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
  
  
# Task 2: Import the Site - Treatment Metadata Dataset

# Site Treatment Metadata is a compiled dataset of the site characteristics including
# information from the elevation and tidal datum calculations in the Elevation Project

# The elevations and tidal datum data is from the first survey (2021 or 2022) or a site

metadata <- read.csv("Input Data\\ACJV Site Treatment Metadata.csv")

# Merge the hydrology and metadata datasets

hydrology_metadata <- hydrology %>%
  left_join(select(metadata, c(Site_Treatment, Restoration_Relative_To_Metonic_Low_2015,
                               Elevation_mean, MHT_NAVD88m:Spring_Tide_Difference_Relative)), 
                   by = "Site_Treatment")

glimpse(hydrology_metadata)

# Visualize the distribution of the dataset

# Calculate the number of sites for each year and treatment
hydro_count <- hydrology %>%
  group_by(Overall_Treatment, Time_Since_Restoration) %>%
  summarise(
    Site_Count = n()) %>%
  ungroup()

glimpse(hydro_count)


hydro_histogram <- ggplot(
  aes(x = Time_Since_Restoration, 
      y = Site_Count,
      group = Overall_Treatment), 
  data = hydro_count) + 
  geom_bar(aes(fill = Overall_Treatment),
           position = position_dodge(0.9), linewidth = 1.25,
           colour = "black", 
           stat = "identity") +
  geom_text(aes(label = Site_Count),
            vjust = -1,
            size = 6) + 
  labs(x = "Time Since Restoration (yrs)",
       y = "Number of Sites") + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  scale_colour_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 22.5, colour = "black"),
    axis.text = element_text(size = 22.5, colour = "black")) + 
  facet_wrap(~Overall_Treatment,
             nrow = 3)

hydro_histogram



ggsave(hydro_histogram, 
       height = 8, width = 12, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Hydrology Histogram.jpg")

rm(hydro_histogram, hydro_count)




# Chapter 3: Review Co-linearity of Flooding Metrics for Entire Dataset --------------------

# I was curious if any of the flooding metrics were correlated with eachother...

# Task 1: Conduct pearson correlation 
pearson_corr <- rcorr(x = as.matrix(
                              select(hydrology, 
                                     c(Root_Flood_Duration, Platform_HT_Frequency,
                                       Root_Drainage_Depth_cm, Drainage_Amplitude_cm))),
                       type = "pearson")

glimpse(pearson_corr)

pearson_r <- tidy(pearson_corr$r)

pearson_p <- tidy(pearson_corr$P)

write.csv(pearson_r, 
          "Output Stats\\Hydrology Pearson Correlation - r values.csv")

write.csv(pearson_p,
          "Output Stats\\Hydrology Pearson Correlation - p values.csv")
  

#Task 2: Create Correlation Figure

# function to format the scatterplots
# could change method to lm, gam, whatever other things you
# can use in geom_smooth. can also change colors, transparency, etc.
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "navy", alpha = 0.8) +
    geom_smooth(method = method, color = "darkorange", 
                se = FALSE, linewidth = 0.9, ...)
  p
}

# build the plot and correlation matrix
# with 'method' can use pearson, spearman, kendall
# make sure your data frame is either all numeric or
# that you subset the numeric columns
hydro_pearson_corr <- hydrology |>
  select(c(Root_Flood_Duration, Platform_HT_Frequency,
                  Root_Drainage_Depth_cm, Drainage_Amplitude_cm)) |>
  ggpairs(upper = list(continuous = wrap("cor", 
                                         method = "pearson", 
                                         use = "pairwise.complete.obs",
                                         colour = "black")),
          lower = list(continuous = wrap(lowerFn)))

hydro_pearson_graph <- hydro_pearson_corr + 
  theme(
    axis.text = element_text(size = 10, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, colour = "black")
  )

hydro_pearson_graph

ggsave(hydro_pearson_graph,
       height = 10, width = 14, dpi = 300,
       filename = "Output Figures\\Hydro Pearson Correlation - Mini Graph Compilation.jpg")

rm(pearson_corr, pearson_p, pearson_r, hydro_pearson_corr, hydro_pearson_graph)  
  
  
corr_graph <- ggplot(
  aes(x = Root_Flood_Duration,
      y = Root_Drainage_Depth_cm,
      fill = Overall_Treatment),
  data = hydrology) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             size = 1.5) + 
  geom_point(
    size = 5.5,
    shape = 21,
    colour = "black") +   
  labs(x = "Root Zone Flooding Duration (%)", 
      y = "Root Zone Drainage Depth (cm)") + 
  scale_x_continuous(limits = c(0, 102), 
                     breaks = seq(0, 100, 20)) + 
  scale_y_continuous(limits = c(-21, 25), 
                     breaks = seq(-20, 25, 5)) +
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  scale_colour_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  theme_bw() +
  theme(
    legend.position = c(0.15, 0.15),
    legend.title = element_blank(),
    legend.text = element_text(size = 18, colour = "black"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

corr_graph


ggsave(corr_graph,
       height = 8, width = 12, dpi = 300,
       filename = "Output Figures\\Hydro Pearson Correlation - Duration v Drain Depth.jpg")

  
  
# Chapter 4: Quality Control for Tidal Datums between years

# To ensure that the site-wide tidal conditions for each were similar for comparison,
# we will take the difference between MHT, MHHT, and Spring Tide for each site based
# on the metadata. 

tidal_datums <- read.csv("Input Data\\Compiled Tidal Datums 2021 - 2024.csv")

glimpse(tidal_datums)

datum_qaqc <- tidal_datums %>%
  mutate(Site_Subsite = ifelse(Sub_Site == "",
                                 paste(Site,sep = " - "),
                                 paste(Site, Sub_Site, sep = " - ")),
         Year_Type = ifelse(Year == "2024", "Second Year", 
                            "First Year")) %>%
  group_by(Site_Subsite) %>%
  summarise(
    MHT_diff = MHT_NAVD88m[which(Year_Type == "Second Year")] - MHT_NAVD88m[which(Year_Type == "First Year")],
    MHHT_diff = MHHT_NAVD88m[which(Year_Type == "Second Year")] - MHHT_NAVD88m[which(Year_Type == "First Year")],
    SpringTide_diff = SpringTide_NAVD88m[which(Year_Type == "Second Year")] - SpringTide_NAVD88m[which(Year_Type == "First Year")] )
  summarise(
    across(MHT_diff:SpringTide_diff,
           list(
             mean = ~mean(., na.rm = TRUE),
             se = ~sd(., na.rm = TRUE) / sqrt(n()) ))) %>%
  mutate(across(MHT_diff_mean:SpringTide_diff_se,
                ~round(., 4)))

glimpse(datum_qaqc)

write.csv(datum_qaqc, 
          "Output Stats\\List of Tidal Datum QAQC Check.csv")



# Chapter 4: Mixed Linear Modeling of Hydrology Metrics -------------------

# Metrics to be evaluated are:
    # Flooding Duration of Root Zone (%)
    # High Tide Flooding Frequency of Root Zone (%)
    # Drainage Depth of Root Zone (m)
    # Drainage 

# Factors in Model
    # Time Since Restoration - 0 - 13 yaers
    # Treatment - Runnel, Reference, No Action


# Task 1: Analysis of Flooding Duration of Root Zone (%)

flood_dur_mod <- lmer(Root_Flood_Duration ~ Time_Since_Restoration * Overall_Treatment + (1| Site_Treatment),
                      data = hydrology) 

flood_dur_anova <- tidy(anova(flood_dur_mod)) %>%
  mutate(model = "Flooding Duration")

flood_dur_anova

flood_dur_pred <- ggpredict(flood_dur_mod, terms = c("Time_Since_Restoration [all]"),
                         type = "fixed", interval = 'confidence') %>%
  rename(Overall_Treatment = x,
         Root_Flood_Duration = predicted,
         Timeline = group) %>%
  arrange(Overall_Treatment)

#Make Time Since Restoration a Numeric Factor
flood_dur_pred$Timeline = as.numeric(flood_dur_pred$Timeline)

# For visualization purposes, constrain the low band of the error ribbon to 0 if negative (impossible to be negative)
flood_dur_pred$conf.low[flood_dur_pred$conf.low < 0] <- 0

# For visualization purposes, constrain the upper band of error ribbon to 100% (impossible to be more than 100%)
flood_dur_pred$conf.high[flood_dur_pred$conf.high > 100] <- 100

glimpse(flood_dur_pred)



# Task 2: Analysis of High Tide Frequency of Root Zone (%)

HT_freq_mod <- lmer(Root_HT_Frequency ~ Time_Since_Restoration * Overall_Treatment + (1| Site_Treatment),
                      data = hydrology) 

HT_freq_anova <- tidy(anova(HT_freq_mod)) %>%
  mutate(model = "High Tide Frequency")

HT_freq_anova

HT_freq_pred <- ggpredict(HT_freq_mod, terms = c("Time_Since_Restoration [all]"),
                            type = "fixed", interval = 'confidence') %>%
  rename(Root_HT_Frequency = predicted,
         Timeline = x)

#Make Time Since Restoration a Numeric Factor
HT_freq_pred$Timeline = as.numeric(HT_freq_pred$Timeline)

# For visualization purposes, constrain the low band of the error ribbon to 0 if negative (impossible to be negative)
HT_freq_pred$conf.low[HT_freq_pred$conf.low < 0] <- 0

# For visualization purposes, constrain the upper band of error ribbon to 100% (impossible to be more than 100%)
HT_freq_pred$conf.high[HT_freq_pred$conf.high > 100] <- 100

glimpse(HT_freq_pred)




# Task 3: Analysis of Drainage Depth of Root Zone (cm)

drain_depth_mod <- lmer(Root_Drainage_Depth_cm ~ Time_Since_Restoration * Overall_Treatment + (1| Site_Treatment),
                    data = hydrology) 

drain_depth_anova <- broom.mixed::tidy(anova(drain_depth_mod)) %>%
  mutate(model = "Drainage Depth")

drain_depth_anova

drain_depth_pred <- ggpredict(drain_depth_mod, terms = c("Time_Since_Restoration [all]"),
                          type = "fixed", interval = 'confidence') %>%
  rename(Root_Drainage_Depth_cm = predicted,
         Timeline = x)

#Make Time Since Restoration a Numeric Factor
drain_depth_pred$Timeline = as.numeric(drain_depth_pred$Timeline)

# For visualization purposes, constrain the low band of the error ribbon to 0 if negative (impossible to be negative)
drain_depth_pred$conf.low[drain_depth_pred$conf.low < 0] <- 0

# For visualization purposes, constrain the upper band of error ribbon to 100% (impossible to be more than 100%)
drain_depth_pred$conf.high[drain_depth_pred$conf.high > 100] <- 100

glimpse(drain_depth_pred)





# Task 4: Analysis of Drainage Amplitude of Root Zone (cm)

drain_amp_mod <- lmer(Drainage_Amplitude_cm ~ Time_Since_Restoration * Overall_Treatment + (1| Site_Treatment),
                        data = hydrology) 

drain_amp_anova <- broom.mixed::tidy(anova(drain_amp_mod)) %>%
  mutate(model = "Drainage Amplitude")

drain_amp_anova

drain_amp_pred <- ggpredict(drain_amp_mod, terms = c("Time_Since_Restoration [all]"),
                              type = "fixed", interval = 'confidence') %>%
  rename(Drainage_Amplitude_cm = predicted,
         Timeline = x)

#Make Time Since Restoration a Numeric Factor
drain_amp_pred$Timeline = as.numeric(drain_amp_pred$Timeline)

# For visualization purposes, constrain the low band of the error ribbon to 0 if negative (impossible to be negative)
drain_amp_pred$conf.low[drain_amp_pred$conf.low < 0] <- 0

# For visualization purposes, constrain the upper band of error ribbon to 100% (impossible to be more than 100%)
drain_amp_pred$conf.high[drain_amp_pred$conf.high > 100] <- 100

glimpse(drain_amp_pred)



# Task 5: Merge the Mixed Linear Model ANOVA tables together

mixed_time_series_anova <- flood_dur_anova %>%
  rbind(HT_freq_anova, drain_depth_anova, drain_amp_anova) %>%
  select(model, term, sumsq, meansq, NumDF, DenDF, statistic, p.value)


write.csv(mixed_time_series_anova,
          "Output Stats\\Linear Mixed Time Series ANOVAs.csv")



# Chapter 5: Visualize the Mixed Linear Modeling -------------------------------

# Task 1: Graph the results of the ANCOVA analyses without significant interactions

# Create a plug-n-n play ggplot with facet_wrap

mixed_time_series <- ggplot(data = hydrology,
                            aes(x = Time_Since_Restoration, 
                                y = Drainage_Amplitude_cm,
                                fill = Overall_Treatment)) + 
  geom_point(size = 5.5,
             shape = 21,
             colour = "black") +
  labs(x = "Restoration Age (yrs)", 
       y = "Root Zone Drainage Amplitude (cm)") + 
  scale_x_continuous(limits = c(0, 14), 
                     breaks = seq(0, 14, 2)) + 
  scale_y_continuous(limits = c(-1, 26), 
                     breaks = seq(0, 25, 5)) +
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  scale_colour_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 27, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 27, colour = "black"),
    axis.text = element_text(size = 27, colour = "black")) + 
  facet_wrap(~Overall_Treatment, 
             nrow = 3, ncol = 1,
             scales = "free")

mixed_time_series


ggsave(mixed_time_series, 
       height = 14, width = 9, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Drainage Amplitude Time Series Model - Panel.jpg")


# Chapter 5: Visualize the Mixed Linear Modeling -------------------------------

# Task 1: Graph the results of the ANCOVA analyses with significant interactions

# Create a plug-n-n play ggplot with facet_wrap

mixed_time_series <- ggplot(data = filter(hydrology_metadata, Overall_Treatment == "Runnel"), 
                            aes(x = Year, 
                                y = Root_Flood_Duration,
                                fill = Tidal_Range)) + 
  geom_vline(xintercept = 0, 
             linetype = "dashed",
             size = 1.5) + 
  geom_point(size = 5.5,
             shape = 21,
             colour = "black",
             position = position_jitter(0.20)) +
  labs(x = "Monitoring Year", 
       y = "Root Zone Flooding Duration (%)") + 
  #scale_x_continuous(limits = c(5, 10), 
   #                  breaks = seq(5, 10, 1)) + 
  scale_y_continuous(limits = c(0, 102), 
                     breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c("orange", "darkgreen")) + 
  theme_bw() +
  theme(
    legend.position = c(0.90, 0.80),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
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
       height = 10, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\RUNNEL Root Flooding Duration - Monitoring Season.jpg")








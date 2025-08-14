# Project: Atlantic Coast Joint Venture - Assessment of Runnel Effectiveness
# Analysis: Vegetation Analysis
# Script: Script  - Time Series Regression

# Author: Grant McKown (james.mckown@unh.edu)

# Purpose: Conduct non-metric dimensional ordination on vegetation community

# Chapter 1: Package Library ---------------------------------------------------


#Data Organization & Formatting
library(tidyverse)
library(stringr)

#Statistics and Data Analysis
library(vegan)

#Data Visualization
library(ggplot2)
library(viridis)
library(patchwork)



# Chapter 1: Import the Vegetation Data Summarized by Site - Treatment

# The dataset is reduced to only contain the means of the main components of the 
# vegetation community: abiotic community, Spartina alternilfora, high marsh
# graminoids, and other species

# Task 1: the vegetation plot dataset will be formatted properly for ordination and
# to be merged with the tidal and flooding datasets
veg <- read.csv("Formatted Datasets\\Formatted Vegetation Plot Dataframe.csv") %>%
  # Remove extraneous column from saving process
  dplyr::select(-X) %>%
  # Double-check that only Impact Plots are included in the dataset
  filter(Impacted == "Yes") %>%
  filter(Overall_Treatment == "Runnel") %>%
  # Create new identifier and classifier columns for merging other dataframes
  mutate(Site_SubSite = ifelse(Sub_Site != "", 
                               paste(Site, Sub_Site, sep = " - "),
                               Site),
         Site_Treatment = paste(Site_SubSite, Overall_Treatment, sep = " - ")) %>%
  mutate(Age_Classification = ifelse(Overall_Treatment == "Runnel" & Time_Since_Restoration <= 4,
                                     "Early Restoration", 
                                     ifelse(Overall_Treatment == "Runnel" & Time_Since_Restoration > 4,
                                            "Older Restoration",
                                            ifelse(Overall_Treatment == "Reference", "Reference", "Pre-Restoration")))) %>%
  # Remove vegetation categories that are already calculated in abiotic and high marsh graminoids
  dplyr::select(-MUD:-ASCO, -SPPAT, -DISPI, -JUGER, -FERUB, -PUMAR, -AGSTO, -UNK, -Thatch_cm,
                -other, -algae_cover, -richness, -SADEP, -SABIG) %>%
  # Group by identifiers for merging and calculating means for each Site - Treatment and Year
  group_by(Site_Treatment, Overall_Treatment, Year, Time_Since_Restoration, 
           Tidal_Regime, Marsh_Condition, Age_Classification) %>%
  # Calculate means of each vegetation cover class (including most species)
  summarise(across(SPALT:salicornia,
                   ~mean(., na.rm = TRUE))) %>%
  mutate(across(SPALT:salicornia, 
                ~round(., 2))) %>%
  ungroup()

glimpse(veg)

# Task 2: Remove all vegetation species that are not found in 5% of all site-treatments

veg_remove <- veg %>%
  dplyr::select(SPALT:TYLAT) %>%
  # Calculates which columns have the number of site - treatments less than 
  # 5% of the site-treatments in the dataset
  dplyr::select(where(
    ~ sum(. > 0, na.rm = TRUE) < (0.05 * length(.)))) %>%
  # Extract the names of columns that should be removed from the dataset
  colnames(.)

glimpse(veg_remove)

# Remove the vegetation columns from the overall vegetation dataframe
veg_reduced <- veg %>%
  dplyr::select(!contains(veg_remove))

glimpse(veg_reduced)


# Task 3: Import the tidal datum dataset

tides <- read.csv("Input Data\\ACJV Site Treatment Metadata.csv") %>%
  mutate(Site_SubSite = ifelse(Subsite != "", 
                               paste(Site, Subsite, sep = " - "),
                               Site),
         Site_Treatment = paste(Site_SubSite, Overall_Treatment, sep = " - "))

glimpse(tides)

# Task 4: Import the flooding metrics dataset

flooding <- read.csv("input Data\\ACJV Master Hydrology Flooding Metrics.csv") %>%
  mutate(Site_SubSite = ifelse(Sub_Site != "", 
                               paste(Site, Sub_Site, sep = " - "),
                               Site),
         Site_Treatment = paste(Site_SubSite, Overall_Treatment, sep = " - ")) %>%
  filter(Include_Analysis == "Yes")

glimpse(flooding)


# Task 4: Merge the Tidal Datum and Flooding Metrics dataset to the Vegetation Dataset

# First, the tidal datum dataset will be merged based on the Site_Treatment characteristic,
# since the 2021.2022 tidal and elevation datums will be used for 2021/2022 and 2024

# Second, flooding statistics will be merged based on the Site_Treatment and Year characteristics. 

veg_compiled <- veg_reduced %>%
  merge(dplyr::select(tides, c(Site_Treatment, Runnel_Marsh_Precondition, MHT_NAVD88m:Spring_Tide_Difference_Relative)), 
        by = c("Site_Treatment")) %>%
  merge(dplyr::select(flooding,
               c(Site_Treatment, Year, Platform_Flood_Duration:Drainage_Amplitude_m)),
        by = c("Site_Treatment", "Year")) %>%
  filter(!is.na(Drainage_Amplitude_m)) %>%
  filter(!is.na(MHT_Difference_Relative))

glimpse(veg_compiled)

write.csv(veg_compiled, 
          "Formatted Datasets\\NMDS Runnel Vegetation Dataset - Plot Based.csv")


# Chapter 2: Non-metric dimensional analysis

# Task 1: Create the vegetation and site characteristic datasets

# Vegetation dataset for the NMDS
veg_dataset <- veg_compiled %>%
  dplyr::select(SPALT:salicornia) %>%
  dplyr::select(-Canopy_Height_cm, -live_cover)


glimpse(veg_dataset)

# Site characteristic dataset for the NMDS

site_dataset <- veg_compiled %>%
  dplyr::select(-SPALT:-salicornia)


glimpse(site_dataset)

# Task 2: Create Bray-Curtis Distance Matrix

veg_dist <- vegdist(veg_dataset, method = "bray")


# Task 3: Conduct NMDS Ordination on the dataset

set.seed(1994)


veg_nmds <- metaMDS(veg_dataset, distance = "bray",
                    try = 20, k = 3)

veg_nmds

# Task 4: Review the stressplot of the NMDS

stressplot(veg_nmds)




# Chapter 3: Visualize the NMDS ordination of the vegetation community

# The NMDS will be visualized in ggplot, which requires the NMDS locations be 
# extracted and combined with the site characteristic data. 

#Task 1: Create the NMDS Veg Points Dataset
veg_nmds_points <- veg_nmds$points %>%
  cbind(site_dataset) %>%
  mutate(Yearr = as.character(Year))

# Task 2: Create the Mean Locations of the Veg Points for Overall Treatment

veg_nmds_points_overall <- veg_nmds_points %>%
  group_by(Runnel_Marsh_Precondition, Time_Since_Restoration) %>%
  summarise(across(MDS1:MDS3, 
                   ~mean(.)))


# Task 2: Graph the Basic NMDS


nmds_plot_basic = ggplot(veg_nmds_points, 
                        aes(x = MDS1, y = MDS2)) + 
  geom_point(aes(fill = Runnel_Marsh_Precondition), 
             size = 6, alpha = 1, 
             stroke = 1.5, shape = 21) + 
 # geom_point(
  #  aes(x = MDS2, y = MDS3, 
   # fill = Runnel_Marsh_Precondition), 
    #data = veg_nmds_points_overall, 
     #        size = 10, alpha = 1, 
      #       stroke = 1.5, shape = 23) + 
  #geom_label(aes(x = MDS1, y = MDS2,
   #              label = Time_Since_Restoration),
    #         data = veg_nmds_points_overall, 
    #         hjust = 1.5) + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  # scale_colour_manual(values = c("orange", "darkgreen", "cornflowerblue")) +
  labs(x = "Axis 1", y = "Axis 2") + 
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black"),
        axis.text.y = element_text(colour = "black", size = 18, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 18), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = c(0.15, 0.80),
        axis.title.x = element_text(face = "bold", size = 20, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", 
                                                                        fill = NA, size = 1.2))

nmds_plot_basic




ggsave(nmds_plot_basic, 
       height = 10, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Veg NMDS - Marsh Condition - Axis 1 & 2 - Salicornia.jpg")



# Chapter 3: Visualize the NMDS ordination and correlations of the vegetation community

# Task 1: Calculation of Species Scores (regression of species to ordination) and save locations for ggplot2

#We can calculate the species scores with the envfit() function of the vegan package
#Essentially it conducts a regression (r^2 and p-value) for each species to the ordination

nmds.species <- envfit(veg_nmds, veg_dataset, permuations = 999, na.rm = T)

nmds.species.r <- data.frame(r = nmds.species$vectors$r, p = nmds.species$vectors$pvals)

nmds.species <- data.frame(veg_nmds$species)

colnames(nmds.species) <- c("MDS1", "MDS2", "MDS3")

nmds.species <- cbind(nmds.species, nmds.species.r)

glimpse(nmds.species)

#In my NMDS, I only want to overlay species scores that have a somewhat decent regression (r^2 > 0.20)
# and are significant (p < 0.05)

nmds.species <- filter(nmds.species, r >= 0.20, p <= 0.05)

nmds.species.pc <- cor(veg_dataset, veg_nmds$points, use = "complete.obs", method = "pearson")

glimpse(nmds.species.pc)




# Task 2: Graph the NMDS with the Species Scores


nmds_plot_species = ggplot(veg_nmds_points, 
                         aes(x = MDS1, y = MDS2)) + 
  geom_point(aes(fill = Overall_Treatment), 
            size = 6, alpha = 1, 
           stroke = 1.5, shape = 21) + 
  geom_segment(data = nmds.species,
               aes(x = 0, y = 0, 
                   xend = MDS1, yend = MDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  geom_label(data = nmds.species,
             aes(x = MDS1, y = MDS2), 
             colour = "black", 
             fontface = "bold", 
             label = row.names(nmds.species), size = 5) + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  # scale_colour_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  labs(x = "Axis 1", y = "Axis 2") + 
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black"),
        axis.text.y = element_text(colour = "black", size = 18, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 18), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = c(0.15, 0.80),
        axis.title.x = element_text(face = "bold", size = 20, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", 
                                                                        fill = NA, size = 1.2))

nmds_plot_species

ggsave(nmds_plot_species, 
       height = 10, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Veg NMDS - Overall Treatment & Species - Axis 1 & 2 - salicornia.jpg")




# Chapter 4: Visualize the Ordination with correlated site characteristics 

# The site charactersitics correlated against the NMDS will be from hydrology, tidal flooding, 
# and general site characteristics

# Site Characteristics: Year, Time Since Restoration, Tidal Regime, Marsh Pre-Condition

# Flooding Characteristics: Marsh Platform Flooding, Root Zone Drainage Depth, Drainage Amplitude

# Tidal Characteristics: Relative MHT, Relative MHHT


env_dataset <- site_dataset %>%
  mutate(Year = factor(Year)) %>%
  dplyr::select(Time_Since_Restoration, Year, Tidal_Regime, Runnel_Marsh_Precondition, 
         Platform_HT_Frequency, Root_Drainage_Depth_m, Drainage_Amplitude_m,
         MHT_Difference_Relative, MHHT_Difference_Relative)


env_corr <- envfit(veg_nmds, env_dataset, permutations = 999)


env_r <- env_corr$vectors$r %>%
  data.frame(.) %>%
  rownames_to_column(., var = "Metric") %>%
  rename(r = ".")

glimpse(env_r)

env_p <- env_corr$vectors$pvals %>%
  data.frame() %>%
  rownames_to_column(., var = "Metric") %>%
  rename(p_value = ".")

glimpse(env_p)

factor_r <- env_corr$factors$r %>%
data.frame(.) %>%
  rownames_to_column(., var = "Metric") %>%
  rename(r = ".")

glimpse(factor_r)

factor_p <- env_corr$factors$pvals %>%
  data.frame() %>%
  rownames_to_column(., var = "Metric") %>%
  rename(p_value = ".")

glimpse(factor_p)

env_scores <- scores(env_corr, display = "vectors", tidy = FALSE) %>%
  data.frame(.) %>%
  rownames_to_column(., var = "Metric") %>%
  merge(env_r, by = "Metric") %>%
  merge(env_p, by = "Metric") %>%
  filter(p_value < 0.05)

glimpse(env_scores)

factor_scores <- scores(env_corr, display = "factors") %>%
  data.frame(.) %>%
  rownames_to_column(., var = "Metric") %>%
  filter(str_detect(Metric, "Runnel"))

factor_scores$Condition <- c("Panne", "Pool")


glimpse(factor_scores)




# Task 2: Graph the NMDS with the Species Scores and Environmental - Factor Scores


nmds_plot_envfit = ggplot(veg_nmds_points, 
                           aes(x = MDS1, y = MDS2)) + 
  geom_point(aes(fill = Runnel_Marsh_Precondition), 
             size = 6, alpha = 1, 
             stroke = 1.5, shape = 21) + 
  geom_segment(data = nmds.species,
               aes(x = 0, y = 0, 
                   xend = MDS1, yend = MDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  geom_label(data = nmds.species,
             aes(x = MDS1, y = MDS2), 
             colour = "black", 
             fontface = "bold", 
             label = row.names(nmds.species), size = 5) + 
  geom_segment(data = env_scores,
               aes(x = 0, y = 0, 
                   xend = NMDS1, yend = NMDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  geom_label(data = env_scores,
             aes(x = NMDS1, y = NMDS2, 
                 label = Metric), 
             colour = "black", 
             fontface = "bold", size = 5) + 
  #geom_label(data = factor_scores, 
   #          aes(x = NMDS1,
    #             y = NMDS2,
     #            label = Condition), 
      #           colour = "black", 
       #          fontface = "bold", size = 3.5) + 
  scale_fill_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  # scale_colour_manual(values = c("orange", "darkgreen", "cornflowerblue")) + 
  labs(x = "Axis 1", y = "Axis 2") + 
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black"),
        axis.text.y = element_text(colour = "black", size = 18, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 18), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = c(0.15, 0.70),
        axis.title.x = element_text(face = "bold", size = 20, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", 
                                                                        fill = NA, size = 1.2))

nmds_plot_envfit

ggsave(nmds_plot_envfit, 
       height = 10, width = 14, 
       dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Output Figures\\Veg NMDS - Species and Site Characteristics - Axis 1 & 2 - salicornia.jpg")



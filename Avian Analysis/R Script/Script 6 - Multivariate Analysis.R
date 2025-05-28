#Project: Avian Community Analysis of SHARP Point Counts of ACJV Project
#Script: Part V - Multivariate Analysis of Avian Communities
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 12th, 2024
#Date Last Edited: March 22nd, 2024


# Purpose: Conduct Mixed Linear Two-WAY ANOVAs on the avian community metrics, Graph the results


# Chapter 1: Set up Code

rm(list = ls())

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(vegan)
library(pairwiseAdonis)
library(broom)
library(stringr)

#Graphing Packages
library(ggplot2)
library(patchwork)
library(ggrepel)
library(viridis)


set.seed(1994)

#Chapter 2: Import the SHARP 0 - 50 m Survey Averaged Dataset

birds_compiled <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird 50m Dataset - Surveys Averaged.csv") %>%
  select(-X) %>%
  filter(Site != 'Moody Marsh') %>%
  filter(Site != 'Broad Cove'| Treatment != 'No Action') %>%
  mutate(State = ifelse(str_starts("Broad Cove", Site), "RI", State)) %>%
  mutate(Region = ifelse(State == "RI", "Narragansett Bay", 
                         ifelse (State == "MA", "North Shore Mass", "North Shore Mass"))) %>%
  select(PointID, Site, State, Region, Treatment:saltysparrow, AGWT:YEWA)



# Chapter 3: Create the dataframes for the Non-metric Dimensional Analysis

# For the NMDS, we need to create 3 dataframes for the analysis:

# 1) Species - Abundance Matrix
# 2) Site Covariates Matrix


# First, we need to get our bird_compiled dataset into a wide dataframe, 
# Remove any rare species, remove any sites without any bird observations,
# Merge the count of feeding guilds and mean wetland score for each

birds_nmds_dataset <- birds_compiled %>%
  #Remove miscellaneous or incomplete datasets
  select(-c(Aerial:Wading), -runnel_age) %>%
  # Removes all columns that do no have observations in at least 5% of site visits (all visit metadata is retained)
  select_if(colSums(. != 0) > (nrow(.) * 0.05)) %>%
  #Remove the Unknown Sparrow category
  select(-UNK_SP, -SALS, -STSP) %>%
  # Removes any site visits that do not have a single bird observation (may need to change beginning column # for bird species)
  filter(rowSums(.[13:length(.)]) > 0)


# Create species - abundance matrix

species <- select(birds_nmds_dataset, BARS:YEWA, saltysparrow)

plots <- select(birds_nmds_dataset, PointID:totalbirds)

# Now, we have three refined datasets that have removed the rare species and
# all sites that do not have any bird observations

# Chapter 5: Run the NMDS, Graph Stress Plot, and Run Ordination Analysis

nmds = metaMDS(species, distance = "bray", trymax = 20, k = 3)

nmds

stressplot(nmds)



# Format for Graphing purposes


#Calculate the locations of the each site on the NMDS ordination (for future ggplot graph)
nmds.points <- as.data.frame(scores(nmds$points)) %>%
  mutate(Treatment = plots$Treatment,
         Site = plots$Site,
         Region = plots$Region,
         PlotID = plots$PlotID,
         Year = as.factor(plots$Year))


#Calculation of Species Scores (regression of species to ordination) and save locations for ggplot2
#We can calculate the species scores with the envfit() function of the vegan package
#Essentially it conducts a regression (r^2 and p-value) for each species to the ordination
#Be sure to record the Species Scores in Excel for future reporting purposes! 

nmds.species.env <- envfit(nmds, species, permuations = 999, na.rm = TRUE)

# Creation of dataframe with NMDS point locations for each individual species
# Species are then filtered by being significant (p < 0.05) and explained at least 10% of the varians (r > 0.10)

nmds.species <- data.frame(r = nmds.species.env$vectors$r, p = nmds.species.env$vectors$pvals) %>%
  cbind(nmds.species.env$vector$arrows) %>%
  mutate(metric = rownames(.)) %>%
  filter(p <= 0.05 & r > 0.15)


# Creation of dataframe with NMDS point locations for each individual species
# Species are then filtered by being significant (p < 0.05) and explained at least 10% of the varians (r > 0.10)

# Calculation of Site Characteristics Scores (regression of site characteristics to ordination)

nmds.site.env <- envfit(nmds, plots, permuations = 999, na.rm = TRUE)

nmds.site <- data.frame(r  = nmds.site.env$vector$r, 
                        p = nmds.site.env$vectors$pvals) %>%
  cbind(nmds.site.env$vector$arrows) %>%
  mutate(metric = rownames(.)) %>%
  filter(p <= 0.05 & r > 0.15)




# Chapter 4: Graph the NMDS

# The ggplot() is created to be easily manipulated for the creation of different symbologies,
# different groupings, and different arrows/texts

Bird_NMDS = ggplot(data = nmds.points,
                   aes(x = MDS1, y = MDS2)) + 
  geom_point(aes(colour = Year, shape = Year),
             size = 6, alpha = 1, stroke = 1.5) + 
  stat_ellipse(aes(color = Year),
               linewidth = 1.5) + 
  #geom_text_repel(aes(label = Site_Date)) +
   geom_text_repel(data = nmds.species,
           aes(x = NMDS1, y = NMDS2), colour = "black", 
          fontface = "bold", label = row.names(nmds.species), size = 5) + 
  geom_segment(data = nmds.species,
              aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
             size =1, alpha = 0.5, colour = "grey30") +
  geom_text_repel(data = nmds.site,
                  aes(x = NMDS1, y = NMDS2), colour = "black", 
                  fontface = "bold", label = row.names(nmds.site), size = 5) + 
  geom_segment(data = nmds.site,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black"),
        axis.text.y = element_text(colour = "black", size = 18, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 18), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = c(0.10, 0.15),
        axis.title.x = element_text(face = "bold", size = 20, colour = "black"), 
        legend.title = element_text(size = 18, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "Axis 1", y = "Axis 2")

Bird_NMDS


ggsave(Bird_NMDS,
       filename = "Avian Analysis\\Figures\\NMDS - Year.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 10, width = 14)



#Chapter 5: Graphing centroids of NMDS

# Step 1 - Calculate Centroids based on selected groupings

nmds.points.mean <- as.data.frame(scores(nmds$points)) %>%
  mutate(Treatment = plots$Treatment,
         Site = plots$Site,
         Region = plots$Region,
         PlotID = plots$PlotID,
         Year = as.factor(plots$Year)) %>%
  group_by(Region) %>%
  summarise(across(MDS1:MDS2, ~mean(.))) %>%
  ungroup()


Bird_NMDS_Mean = ggplot(data = nmds.points.mean,
                   aes(x = MDS1, y = MDS2)) + 
  geom_point(aes(colour = Region, shape = Region),
             size = 6, alpha = 1, stroke = 1.5) + 
  #geom_text_repel(aes(label = Site_Date)) +
  geom_text(data = nmds.species,
            aes(x = NMDS1, y = NMDS2), colour = "black", 
            fontface = "bold", label = row.names(nmds.species), size = 5) + 
  geom_segment(data = nmds.species,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  geom_text_repel(data = nmds.site,
                  aes(x = NMDS1 + 0.05, y = NMDS2), colour = "black", 
                  fontface = "bold", label = row.names(nmds.site), size = 5) + 
  geom_segment(data = nmds.site,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black"),
        axis.text.y = element_text(colour = "black", size = 18, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 18), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = c(0.05, 0.15),
        axis.title.x = element_text(face = "bold", size = 20, colour = "black"), 
        legend.title = element_text(size = 18, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "Axis 1", y = "Axis 2")

Bird_NMDS_Mean


ggsave(Bird_NMDS_Mean,
       filename = "Avian Analysis\\Figures\\Mean NMDS - Region.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 10, width = 14)






#Chapter 6: PERMANOVA comparisons for each individual factor

# The second step in the multivariate analysis is the comparison of the avian community
# between each grouping (Year, Treatment, and Region). PERMANOVAs will be used to conducted
# both the initial and post-hoc multiple comparisons. 

# PERMANOVA is selected due to the non-parametric structure of the data set. 

# Groupings will be analyzed separately and individually from other factors, since
# we are not interested in exploring more complex modelling of the avian community

# Post-hoc multiple comparisons are conducted by the pairwise.adonis2() function
# from the pairwiseadonis package -- simply a wrapper that conducts bonferonni - corrected
# PERMANOVA comparisons between two levels in each factor. 

#Analysis 1: PERMANOVA of the Different Years

perm_year <- adonis2(species ~ Year,  
                          method = "bray",
                          data = plots)
perm_year

#MRPP was significant between the three classes, so pairwise comparisons!

posthoc_year <- pairwise.adonis2(species ~ Year,
                 data = plots,
                 method = "bray",
                 p.adjust.m = 'bonferonni')

posthoc_year

#Analysis 2: Treatment

perm_treatment <- adonis2(species ~ Treatment,  
                  method = "bray",
                  data = plots)
perm_treatment

#Not significant, moving on...


#Analysis 3: Region

perm_region <- adonis2(species ~ Region,  
                          method = "bray",
                          data = plots)
perm_region

perm_region <- tidy(perm_region)


permanova_tables <- rbind(tidy(perm_year), tidy(perm_treatment), tidy(perm_region))


write.csv(permanova_tables,
          "Avian Analysis\\Output Stats\\PERMANOVA Tables.csv")

#Chapter 6: Similarity of Percentages (SIMPER) Analysis

#Similarity of Percentages is performed for Year and Region

#Analysis 1: SIMPER conducted between the two Regions

region_simper <- simper(species, plots$Region)

summary(region_simper)


#Analysis 2: SIMPER conducted between the three years

year_simper <- simper(species, plots$Year)

summary(year_simper)


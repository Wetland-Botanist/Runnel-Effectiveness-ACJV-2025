#Project: Avian Community Analysis of SHARP Point Counts of ACJV Project
#Script: Part II - Addiition of Feeding Habits and Wetland Scores to SHARP Birding Dataset
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 14th, 2024
#Date Last Edited: March 21st, 2024

#Purpose:


# Chapter 1: Set up Code

#Library & Packages
rm(list = ls())

#Stats & Data Organization Packages
library(tidyr)
library(dplyr)


#Chapter 2: Import Bird Dataset & Determine Species List for Dataset

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird 50 m Distance.csv")

bird_species <- birds %>%
  gather(key = AlphaCode, value = "Count", totalbirds:YEWA) %>%
  group_by(PointID) %>%
  filter(Count != 0) %>%
  ungroup() %>%
  distinct(AlphaCode)

#Dataset is saved for background work on compiilng wetland scores and feeding habits
write.csv(bird_species,
          "Avian Analysis\\Formatted Datasets\\Bird Species List at 50m band.csv")


#Chapter 3: Import the Bird Species List with Wetland Score and Feeding Guild

#After the bird species list was generated, I compiled the feeding guilds and wetland scores for each bird species


# FEEDING GUILD:
# Feeding guild preference was originally created by Graaf et al. 1985 that characterized the feeding habits
# of each individual bird species in North America. Shriver 2015 (Master's Thesis) subsetted and simplified the
# feeding guild preference to a handful of guilds for wetland species in Louisiana. I have subsetted all species
# within the 50 m distance band to Shriver 2015's guilds. Species that were not in Shriver were located into a guild
# based on observations, research, and similarly related species. An additional Upland Forager and Gleaner type was
# created to bin upland species. 

# WETLAND SCORE:
#Wetland habitat dependency score of each bird species in Pennsylvania was compiled by Croonquist et al. 1996. Almost
# all species from the bird observations were assigned a score in Croonquist et al. 1996. Several species were manually
# assigned a score by Grant McKown by similarly related species. Wetland scores are 0, 1, 3, and 5 with 0 being no 
# wetland dependence by and 5 being fully dependent for habitat needs. 

bird_species <- read.csv("Avian Analysis\\Input Data\\Bird_Species_50m.csv")

birds_scores <- birds %>%
  dplyr::select(-totalbirds, -saltysparrow) %>%
  gather(key = "AlphaCode", value = "Count", AGWT:YEWA) %>%
  merge(., 
        dplyr::select(bird_species, AlphaCode, Feeding.Guild.Code, Wetland.Score),
        by = "AlphaCode") %>%
  rename(wetland_score = Wetland.Score,
         feeding_guild = Feeding.Guild.Code) %>%
  dplyr::select(PointID, Site, State, RegionNum, Treatment, VisitNum, runnel_age, Year, Point_X:Site_Date, 
         DistBand, AlphaCode, Count, feeding_guild, wetland_score)


feeding_scores <- bird_species %>%
  select(Feeding.Guild, Common.Name, AlphaCode) %>%
  spread(key = Feeding.Guild, value = Common.Name) %>%
  select(-AlphaCode)

write.csv(feeding_scores,
          "Avian ANalysis\\Formatted Datasets\\Feeding Classifications Table.csv")




# Chapter 4: Calculate the Mean Wetland Weighted Score, Feeding Habit Count

# Calculate the total number of bird observations per site visit for each feeding guild,
# Calculate the total number of panne and pool dependent species 

bird_feeding_visit <- birds_scores %>%
  group_by(Site_Date, feeding_guild) %>%
  #Counts the number of bird individuals in each feeding habit
  summarise(Count = sum(Count)) %>%
  ungroup() %>%
  #Transforms the dataset from long to wide
  spread(feeding_guild, Count) %>%
  #Calculate the 'Panne Dependent' portion of the avian community,
  # Then calculates the total number of birds in the avian community
  mutate(Panne_Dependent = Dabbler + Mudflat + Wading + Piscivore,
         Total = rowSums(dplyr::select(., Aerial:Wading), na.rm = FALSE)) %>%
  #Calculates the percentage of the avian community for each habit, rounds to 1 decimal place
  mutate(across(Aerial:Panne_Dependent, ~(./Total)*100),
         across(Aerial:Panne_Dependent, ~round(., 1))) %>%
  select(-Total)
  

glimpse(bird_feeding_visit)


# Calculate the mean wetland score (non-weighted) for each site

bird_wetland_score <- birds_scores %>%
  group_by(Site_Date) %>%
  filter(Count != 0) %>%
#Calculate  Bird Count * Wetland Weighting Score for each bird species
  mutate(count_wetland_score = Count * wetland_score) %>%
#Sum across all bird species for each survey to calculate numerator and denominator of weighted average
  summarise(sum_count_wetland_score = sum(count_wetland_score),
            sum_wetland_score = sum(Count)) %>%
  ungroup() %>%
  #Calculate weighted average, round to 2 decimal places
  mutate(weighted_wetland_score = sum_count_wetland_score / sum_wetland_score,
         weighted_wetland_score = round(weighted_wetland_score, 2)) %>%
  select(Site_Date, weighted_wetland_score)

glimpse(bird_wetland_score)


# Chapter 5: Merge the Feeding Habits and Wetland Scores with the primary 50 m distance dataset

birds_compiled <- birds %>%
  merge(bird_feeding_visit, by = "Site_Date") %>%
  merge(bird_wetland_score, by = "Site_Date")

glimpse(birds_compiled)


# Dataset is saved for posterity
write.csv(birds_compiled,
          "Avian Analysis\\Formatted Datasets\\SHARP Bird 50m Dataset - Wetland and Feeding Scores - Final.csv")



#Chapter 6: Calculate the average of the two surveys in each monitoring season for each SHARP Point

#We are going to drop the survey specific characteristics such as wind speed, rain, personnel, etc. 

glimpse(birds_compiled)

birds_subsetted <- birds_compiled %>%
  dplyr::select(PointID, Site, State, Treatment, Year, runnel_age, 
                richness:YEWA, Aerial:Panne_Dependent, weighted_wetland_score) %>%
  group_by(PointID, Site, State, Treatment, Year, runnel_age) %>%
    summarise(across(richness:weighted_wetland_score,
                     ~round(mean(., na.rm = TRUE), 2))) %>%
    ungroup() %>%
  select(PointID, Site, State, Treatment, Year, runnel_age,
         weighted_wetland_score, Aerial:Panne_Dependent, 
         richness, shannon, totalbirds, saltysparrow, AGWT:YEWA)

glimpse(birds_subsetted)

#Save the final SHARP 50 m dataset in the "Formatted Datasets" Folder
# Dataset will be used in Scripts 3 - 7 for descriptive statistics, univariate analysis
# multivariate analysis, and occupational modeling

write.csv(birds_subsetted,
          "Avian Analysis\\Formatted Datasets\\SHARP Bird 50m Dataset - Surveys Averaged.csv")




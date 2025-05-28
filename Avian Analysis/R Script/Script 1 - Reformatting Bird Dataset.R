#Project: Avian Community Analysis of SHARP Point Counts of ACJV Project
#Script: Part I - Combining Bird Datasets & Formatting for Analysis
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 12th, 2024
#Date Last Edited: March 21st, 2024


# Purpose: Combine the separate SHARP datasets over 2021 - 2023 for exploratory, multivariate, and modeling analysis

# Chapter 1: Set up Code

#Library & Packages
rm(list = ls())

#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(vegan)


#Chapter 2: Load all of the bird datasets

# Bird datasets include:
# (1) Maine and Massachusetts 2021
# (2) Rhode Island 2021
# (3) All States 2022
# (4) All Stats 2023

# Bird datasets were not standardized upon data entry requiring needed data reformatting in Excel before R including:
# (1) Column renaming 
# (2) Miscellaneous columns removal, especially when they weren't used across years
# (3) Creation of new columns including overall 'Treatment' category


# Load Maine and Massachusetts 2021 Dataset (no R formatting needed)
bird_me_ma_21 <- read.csv("Avian Analysis\\Input Data\\Maine_Mass_2021.csv")
  

# Load Rhode Island 2021 Dataset (R formatting needed)

# For some reason, upon data entry, several birds at the same SHARP Point and Distance Band had multiple entries
# dplyr's spread() function can not work if a row have the same entries besides the value (# of birds observed),
# so it had to be remedied after loading. 
# Bird observations were added together for same SHARP Point, Visit, and Distance Band. Then only one entry was kept

bird_ri_21 <- read.csv("Avian Analysis\\Input Data\\RhodeIsland_2021.csv") %>%
  filter(Site != "") %>%
  group_by(PointID, VisitNum, AlphaCode, DistBand) %>%
  mutate(across(Min.1:Min.12, ~max(.))) %>%
  mutate(TotalCountN = sum(TotalCountN)) %>%
  ungroup() %>%
  distinct(PointID, VisitNum, AlphaCode, DistBand, .keep_all = TRUE)

# All states 2022 bird data (No R formatting needed)
bird_2022 <- read.csv("Avian Analysis\\Input Data\\Birds_All_2022.csv") %>%
  filter(Site != "") %>%
  group_by(PointID, VisitNum, AlphaCode, DistBand) %>%
  mutate(across(Min.1:Min.11, ~max(.))) %>%
  mutate(TotalCountN = sum(TotalCountN)) %>%
  ungroup() %>%
  distinct(PointID, VisitNum, AlphaCode, DistBand, .keep_all = TRUE)
  
# All states 2023 bird data (No R formatting needed)
bird_2023 <- read.csv("Avian Analysis\\Input Data\\Birds_All_2023.csv") %>%
  filter(Site != "") %>%
  filter(Site != "Canonchet" | Treatment != "RUN" | SurveyDate != "7/7/2023") %>%
  group_by(PointID, VisitNum, AlphaCode, DistBand) %>%
  mutate(across(Min.1:Min.12, ~max(.))) %>%
  mutate(TotalCountN = sum(TotalCountN)) %>%
  ungroup() %>%
  distinct(PointID, VisitNum, AlphaCode, DistBand, .keep_all = TRUE)
  



#Chapter 3: Create and reformat the entire bird dataset for Trustees

# See in-depth annotations for R formatting of entire bird dataset

birds_all <- bind_rows(bird_me_ma_21, bird_ri_21, bird_2022, bird_2023) %>%
  #Create a unique Point ID and Survey Date code for each survey
  mutate(Site_Date = paste(PointID, SurveyDate, " ")) %>%
  #Remove the minute by minute counts
  select(-c(Min.1:Min.11, Min.12, Comments, Outside)) %>%
  #Make each bird species a column, so the number of rows are reduced to each site visit and distance band
  spread(key = AlphaCode, value = TotalCountN) %>%
  #Remove all of the NAs from the species column
  mutate(across(AGWT:YEWA, ~ifelse(is.na(.), 0, .))) %>%
  mutate(
    #Calculate the total number of birds, "salty sparrows", and total sparrows  observed 
    totalbirds = rowSums(select(., AGWT:YEWA)),
    #Calculates the total number of endemic sparrows to salt marshes
         saltysparrow = SALS + NESP + STSP,
    #Calculates species richness using rowSums
         richness = rowSums(dplyr::select(., AGWT:YEWA) > 0, na.rm = FALSE),
    #Calculates Shannon Diversity using the vegan package, rounds to 2 decimal points
         shannon = round(diversity(dplyr::select(., AGWT:YEWA), index = "shannon"), 2),
    #Calculates runnel age for all Runnel Treatment ShARP Points
         runnel_age = ifelse(Treatment == "RUN", Year - Runnel_Year0, NA)) %>%
  #Remove the Ipswich Runnel West SHARP Point, since the runnel was never created
  filter(PointID != "Ipswich RUN West" & 
         PointID != "Ipswich REF West" &
         PointID != "Ipswich NAC West") %>%
  #Rename the Treatments
  mutate(Treatment = ifelse(Treatment == "RUN", "Runnel",
                            ifelse(Treatment == "NAC", "No Action",
                                   "Reference"))) %>%
  #Keep only necessary columns for further analysis and arrange for easier viewing
  select(RegionNum, State, PointID, Site, Treatment, runnel_age, Point_X, Point_Y,
          VisitNum:Site_Date, richness, shannon, totalbirds, saltysparrow, AGWT:YEWA) %>%
  arrange(PointID, Year, VisitNum)

# Formatted bird dataset is sent to the "Formatted Datasets" folder for use in next step code
# Dataset is saved for posterity
write.csv(birds_all, 
          "Avian Analysis\\Formatted Datasets\\SHARP Bird Dataset with Missing Distance Band.csv")



# Chapter 4: Complete the dataset with missing Distance Bands for each site visit

birds_complete <- birds_all %>%
  group_by(PointID, Year, VisitNum) %>%
  complete(DistBand = c("0-50m", "51-100m", "> 100m")) %>%
  mutate(across(totalbirds:YEWA, ~ifelse(is.na(.), 0, .)),
         across(RegionNum:Site_Date, ~ifelse(is.na(.), 
                                            paste(unique(.)[which(!is.na(unique(.)))]), .))) %>%
  ungroup()

write.csv(birds_complete,
          "Avian Analysis\\Formatted Datasets\\SHARP Bird Dataset with Completed Distance Band.csv")


#Chapter 4: Reduce and reformat the entire dataset for only distance bands of 0 - 50 m

birds_50 <- birds_complete %>%
  filter(DistBand == "0-50m")

# Formatted bird dataset is sent to the "Formatted Datasets" folder for use in next step code
# This is the dataset that will be used in Part II Script - Addition of Wetland and Feeding Habit Scores
write.csv(birds_50,
          "Avian Analysis\\Formatted Datasets\\SHARP Bird 50 m Distance.csv")



#Chapter 5: Metadata analysis of the bird surveys within 50 m distance bands

#Chapter 3: Summation of the Metadata of Bird Data
# The data was collected heterogeneously (i.e., possibly not all points collected each year, number of site visits, etc.)
# I need to wrap my mind around the structure of the data before exploratory analysis

birds_meta <- birds_50 %>%
  group_by(PointID) %>%
  summarise(
    #The code pastes the Site and then the seasons each SHARP Point was monitored  
    Site = paste(unique(Site)),
    Seasons = paste(unique(Year)[which(!is.na(unique(Year)))], collapse = ", "),
    #The complicated code essentially pastes the list of unique site visit numbers fore each year
    Visit_21 = paste(unique(VisitNum[Year == "2021"])[which(!is.na(unique(VisitNum[Year == "2021"])))], 
                     collapse = ", "),
    Visit_22 = paste(unique(VisitNum[Year == "2022"])[which(!is.na(unique(VisitNum[Year == "2022"])))], 
                     collapse = ", "),
    Visit_23 = paste(unique(VisitNum[Year == "2023"])[which(!is.na(unique(VisitNum[Year == "2023"])))], 
                     collapse = ", "),
    # The complicated code pastes the list of unique monitoring dates for each year         
    Dates_21 = paste(unique(SurveyDate[Year == "2021"])[which(!is.na(unique(SurveyDate[Year == "2021"])))], 
                     collapse = ", "),
    Dates_22 = paste(unique(SurveyDate[Year == "2022"])[which(!is.na(unique(SurveyDate[Year == "2022"])))], 
                     collapse = ", "),
    Dates_23 = paste(unique(SurveyDate[Year == "2023"])[which(!is.na(unique(SurveyDate[Year == "2023"])))], 
                     collapse = ", "),
    # The complicated code pastes the list of observers for each year (not separated by monitoring visit)        
    Observers_21 = paste(unique(Observer[Year == "2021"])[which(!is.na(unique(Observer[Year == "2021"])))], 
                         collapse = ", "),
    Observers_22 = paste(unique(Observer[Year == "2022"])[which(!is.na(unique(Observer[Year == "2022"])))], 
                         collapse = ", "),
    Observers_23 = paste(unique(Observer[Year == "2023"])[which(!is.na(unique(Observer[Year == "2023"])))], 
                         collapse = ", ")) %>%
  ungroup()

# Dataset is saved for posterity and background information on the project
write.csv(birds_meta,
          "Avian Analysis\\Formatted Datasets\\Metadata of SHARP Point Visits.csv")

  









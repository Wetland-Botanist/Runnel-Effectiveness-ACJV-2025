#Project: Avian Community Analysis of SHARP Point Counts of ACJV Project
#Script: Descriptive Statistics of Avian Community Metrics
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 13th, 2024
#Date Last Edited: March 21st, 2024


# Purpose: Calculate mean +/- standard error for avian community metrics for Treatment + Year, Graph the statistics

# Chapter 1: Set up Code

rm(list = ls())

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(stringr)

#Graphing Packages
library(ggplot2)
library(patchwork)
library(ggrepel)
library(viridis)
library(pals)


#Chapter 2: Import necessary datasets

#Import the Bird 50 m distance band dataset with feeding habit and wetland scores, averaged across surveys each season

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird 50m Dataset - Surveys Averaged.csv") %>%
  select(-X) %>%
  filter(Site != "Moody Marsh") %>%
  filter(Site != 'Broad Cove'| Treatment != 'No Action')

colnames(birds)


#Chapter 3: Calculate Descriptive Statistics for Treatment - Year across all metrics

treatment_stats <- birds %>%
  group_by(Treatment, Year) %>%
  #Calculate the mean and standard error using the across() function for all metrics and species
  summarise(across(weighted_wetland_score:YEWA,
                   list(
                     m = ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE)/sqrt(n()),
                   ))) %>%
  ungroup() %>%
  #Round the mean and standard error for all columns to 2 decimal points
  mutate(across(weighted_wetland_score_m:YEWA_se,
         ~round(., 2))) %>%
  #Remove the descriptive statistics of the bird species (only keeping avian community metrics)
  select(Treatment, Year,
         weighted_wetland_score_m : saltysparrow_se)


write.csv(treatment_stats,
          "Avian Analysis\\Output Stats\\Descriptive Stats Treatment - Year.csv")  



#Chapter 4: Graph the descriptive statistics of the Feeding Habit

# For the graphs, in each chapter, I will need to transform the dataset from Wide to Long and subset
# according to the graph's parameters

# In the graph, the panne dependent groupings will be removed and replaced with the overarching
# 'Panne Dependent' classification


# First, transform the descriptive stats dataset from wide to long and subset to just the feeding habitat categories

feeding_mean <- treatment_stats %>%
  select(Treatment, Year, Aerial_m:Panne_Dependent_se) %>%
  select(-Wading_m, -Piscivore_m, -Mudflat_m, -Dabbler_m, 
         -ends_with('_se')) %>%
  gather(key = 'feeding_habit', value = 'feeding_mean', ends_with('_m')) %>%
  mutate(feeding_habit = str_remove(feeding_habit, "_m")) %>%
  arrange(Treatment, Year)
  
  
feeding_se <- treatment_stats %>%
  select(Treatment, Year, Aerial_m:Panne_Dependent_se) %>%
  select(-Wading_se, -Piscivore_se, -Mudflat_se, -Dabbler_se, 
         -ends_with('_m')) %>%
  gather(key = feeding_habit, value = feeding_se, ends_with('_se')) %>%
  mutate(feeding_habit = str_remove(feeding_habit, "_se")) %>%
  arrange(Treatment, Year)

feeding_stats <- feeding_mean %>%
  merge(feeding_se, by = c('Treatment', 'Year', 'feeding_habit')) %>%
  mutate(feeding_habit = ifelse(feeding_habit == "Panne_Dependent",
                                "Panne & Pool Dependent", feeding_habit),
         Year = as.factor(Year))

rm(feeding_mean, feeding_se)


#Graph the Feeding Habit Composition of the Avian Community
feeding_habit_graph <- ggplot(feeding_stats,
                              aes(x = Year, y = feeding_mean, fill = feeding_habit)) + 
  geom_bar(stat = 'identity', width = 0.55, color = "black", position = "fill") + 
  scale_fill_manual(values = as.vector(kelly(9))) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0,0)) + 
  labs(y = "Percent of Community", 
       x = "") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"),
    strip.text = element_text(size = 20, colour = "black"),
    strip.background = element_blank()) + 
  facet_wrap(~Treatment,
             nrow = 3)

feeding_habit_graph

ggsave(feeding_habit_graph,
       filename = "Avian Analysis\\Figures\\Feeding Habit Community Composition.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 12, width = 14)



# Chapter 5: Graph the Remaining Avian Community Metrics

#stats_mean <- treatment_stats %>%
#  select(Treatment, Year, ends_with("_m"), -c(Aerial_m:Wading_se)) %>%
#  gather(key = metric, value = mean, ends_with("_m")) %>%
#  mutate(metric = str_remove(metric, "_m")) %>%
#  arrange(Treatment, Year)

#stats_se <-  treatment_stats %>%
#  select(Treatment, Year, ends_with("_se"), -c(Aerial_m:Wading_se)) %>%
#  gather(key = metric, value = se, ends_with("_se")) %>%
#  mutate(metric = str_remove(metric, "_se")) %>%
#  arrange(Treatment, Year)

descrip_stats <- treatment_stats %>%
  select(-c(Aerial_m:Wading_se)) %>%
  mutate(Year = as.factor(Year),
         Panne_Dependent_m = Panne_Dependent_m / 100,
         Panne_Dependent_se = Panne_Dependent_se / 100)

glimpse(descrip_stats)


#Graph 1: Total bird abundance


abundance_graph <- ggplot(descrip_stats,
                               aes(x = Year, y = totalbirds_m, group = Treatment)) +
  geom_bar(aes(fill = Treatment),
           stat = 'identity', position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = totalbirds_m + totalbirds_se, 
                    ymin = totalbirds_m - totalbirds_se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 15),
                     breaks = seq(0, 15, 3),
                     expand = c(0,0)) +
  labs(y = "Bird Abundance",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.position = "mone",
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"))

abundance_graph


#Graph 2: Marsh Sparrow Abundance

sparrow_graph <- ggplot(descrip_stats,
                          aes(x = Year, y = saltysparrow_m, group = Treatment)) +
  geom_bar(aes(fill = Treatment),
           stat = 'identity', position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = saltysparrow_m + saltysparrow_se, 
                    ymin = saltysparrow_m - saltysparrow_se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 6),
                     breaks = seq(0, 6, 1),
                     expand = c(0,0)) +
  labs(y = "Marsh Sparrow Abundance",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.position = c(0.125, 0.80),
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"))

sparrow_graph


#Graph 3: Panne and Pool Dependent Species


panne_graph <- ggplot(descrip_stats,
                        aes(x = Year, y = Panne_Dependent_m, group = Treatment)) +
  geom_bar(aes(fill = Treatment),
           stat = 'identity', position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = Panne_Dependent_m + Panne_Dependent_se, 
                    ymin = Panne_Dependent_m - Panne_Dependent_se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0,0),
                     limits = c(0, 0.60)) + 

  labs(y = "Pool and Panne Species Abundance (%)",
       x = element_blank()) +
  
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"))

panne_graph


#Graph 4: Weighted Wetland Score


wetland_graph <- ggplot(descrip_stats,
                      aes(x = Year, y = weighted_wetland_score_m, group = Treatment)) +
  geom_bar(aes(fill = Treatment),
           stat = 'identity', position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = weighted_wetland_score_m + weighted_wetland_score_se, 
                    ymin = weighted_wetland_score_m - weighted_wetland_score_se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 5),
                     breaks = seq(0, 5, 1),
                     expand = c(0,0)) +
  labs(y = "Weighted Wetland Score",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"))

wetland_graph

  
#Graph 5: Species Richness


richness_graph <- ggplot(descrip_stats,
                        aes(x = Year, y = richness_m, group = Treatment)) +
  geom_bar(aes(fill = Treatment),
           stat = 'identity', position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = richness_m + richness_se, 
                    ymin = richness_m - richness_se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 5),
                     breaks = seq(0, 5, 1),
                     expand = c(0,0)) +
  labs(y = "Species Richness",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"))

richness_graph



#Graph 6: Shannon Diversity


shannon_graph <- ggplot(descrip_stats,
                         aes(x = Year, y = shannon_m, group = Treatment)) +
  geom_bar(aes(fill = Treatment),
           stat = 'identity', position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = shannon_m + shannon_se, 
                    ymin = shannon_m - shannon_se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 2),
                     breaks = seq(0, 2, 0.50),
                     expand = c(0,0)) +
  labs(y = "Shannon Diversity",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"))

shannon_graph


descrip_graph <- (abundance_graph + sparrow_graph) / (panne_graph + wetland_graph) / (richness_graph + shannon_graph)

descrip_graph


ggsave(descrip_graph,
       filename = "Avian Analysis\\Figures\\Descriptive Stats Bar Charts.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 12, width = 16)
  
  
































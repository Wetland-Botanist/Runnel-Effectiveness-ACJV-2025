#Title:  Exploratory Analysis of SHARP Point Count Surveys (Community Composition)
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 13th, 2024
#Date Last Edited: February 13th, 2024


# Purpose: Describe and calculate the composition of the avian communities based on feeding guilds (Shriver 2015)

# Chapter 1: Set up Code

rm(list = ls())

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(vegan)
library(broom)

#Graphing Packages
library(ggplot2)
library(patchwork)
library(ggrepel)
library(viridis)
library(pals)

#Chapter 2: Import the relevant datasets

#Dataset 1 - Bird community dataset within 50 m distance band, created in "Bird Reformatting" R Code

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird Dataset 50m Wetland and Feeding Scores.csv") %>%
  mutate(Year = as.factor(Year))


# Chapter 3: Calculate Descriptive Statistics for Wetland Ranking and Feeding Guild

# First I need to sum the number of counts for each feeding guild and wetland score separately


#Step 1 - Calculate the sum of each feeding group per site visit
birds_feeding <- birds %>%
  select(-AlphaCode) %>%
  spread(key = feeding_guild, value = Count) %>%
  mutate(across(AERIAL:WADING, ~ifelse(is.na(.), 0, .))) %>%
  #Calculate all species that are habitat dependent on mudflats, pannes, or pools
  #Calculate all birds observed
  mutate(Panne_Dependent = DABBLER + MUDFLAT + WADING + PISCIVORE,
         totalbirds = rowSums(select(., AERIAL:WADING))) %>%
  gather(key = feeding_guild, value = Count, AERIAL:totalbirds) %>%
  group_by(PointID, Site, State, Treatment, VisitNum, Year, Point_X, runnel_age, feeding_guild) %>%
  summarise(Count = sum(Count)) %>%
  ungroup()

#Calculate the mean count of each feeding guilt for each SHARP Point per season
birds_feeding_stats <- birds_feeding %>%
  group_by(Treatment, Year, feeding_guild) %>%
  summarise(Count.m = mean(Count, na.rm = TRUE),
            Count.se = mean(Count)/sqrt(n())) %>%
  mutate(Count.m = round(Count.m, 2),
         Count.se = round(Count.se, 2)) %>%
  ungroup()

write.csv(birds_feeding_stats,
          "Avian Analysis\\Output Stats\\Feeding Guild By Sharp Point Descriptive Stats.csv")


#Step 2 - Calculate the weighted wetland score of each site visit
birds_wetland <- birds %>%
  select(-AlphaCode) %>%
  filter(Count != 0) %>%
  mutate(Weighted_WetlandScore = wetland_score * Count) %>%
  group_by(PointID, Site, State, Treatment, Year, Point_X, runnel_age) %>%
  summarise(Score = sum(Weighted_WetlandScore)/sum(Count)) %>%
  ungroup() %>%
  mutate(Score = round(Score, 2))

#Calculate the mean weighted wetland score for each SHARP Point for each site visit
birds_wetland_stats <- birds_wetland %>%
  group_by(Treatment, Year) %>%
  summarise(Score.m = mean(Score, na.rm = TRUE),
            Score.se = sd(Score)/sqrt(n())) %>%
  mutate(Score.m = round(Score.m, 2),
         Score.se = round(Score.se, 2)) %>%
  ungroup()

write.csv(birds_wetland_stats,
          "Avian Analysis\\Output Stats\\Weighted Wetland Score By Sharp Point Descriptive Stats.csv")


#Chapter 4: Graph the descriptive statistics of the Feeding Guild and Wetland Score


#Graph 1 - Community description of feeding guilds per treatment (with stacked bar charts)
birds_feeding_graph <- ggplot(filter(birds_feeding_stats, feeding_guild != "Panne_Dependent",
                                     feeding_guild != "totalbirds"),
                              aes(x = Year, y = Count.m, fill = feeding_guild)) + 
  geom_bar(stat = 'identity', width = 0.55, color = "black", position = "fill") + 
  scale_fill_manual(values = as.vector(kelly(9))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Count of Birds per Feeding Guild", 
       x = "") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_blank()) + 
  facet_wrap(~Treatment,
             nrow = 3)

birds_feeding_graph


#Export the community composition graph to jpeg
ggsave(birds_feeding_graph,
       filename = "Avian Analysis\\Figures\\Birds Feeding Guilds Community Composition.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 10, width = 16)



#Graph 2 - Change in Marsh Foragers and Panne-Dependent Species overtime per treatment (bar charts)

birds_percent <- birds_feeding %>%
  group_by(PointID, Site, State, VisitNum, Year) %>%
  mutate(total = sum(Count[feeding_guild == "totalbirds"]),
         percent_community = Count / total) %>%
  ungroup() %>%
  group_by(Treatment, Year, feeding_guild) %>%
  summarise(percent_community.m = mean(percent_community, na.rm = TRUE) * 100,
            percent_community.se = sd(percent_community, na.rm = TRUE)/sqrt(n()) * 100) %>%
  mutate(percent_community.m = round(percent_community.m, 2),
         percent_community.se = round(percent_community.se, 2)) %>%
  ungroup()
  



birds_panne_graph <- ggplot(filter(birds_percent,
                                     feeding_guild == "Panne_Dependent" | feeding_guild == "MARSH"),
                              aes(x = Year, 
                                  y = percent_community.m, 
                                  group = Treatment)) + 
  geom_bar(aes(fill = Treatment),
           stat = 'identity', 
           position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = percent_community.m + percent_community.se, 
                    ymin = percent_community.m - percent_community.se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) +
  labs(y = "% of Bird Community", 
       x = "") + 
  theme_bw() +
  theme(
    legend.position = c(0.15, 0.40),
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_blank()) + 
  facet_wrap(~feeding_guild,
             nrow = 3)

birds_panne_graph


#Graph 3 - Change in weighted wetland score over time per treatment (bar charts)

birds_wetland_graph <- ggplot(data = birds_wetland_stats,
                             aes(x = Year, y = Score.m, group = Treatment)) +
  geom_bar(aes(fill = Treatment),
           stat = 'identity', position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = Score.m + Score.se, 
                    ymin = Score.m - Score.se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 5),
                     breaks = seq(0, 5, 1),
                     expand = c(0,0)) +
  labs(y = "Weighted Wetland Score of Bird Community",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"),
    strip.text = element_text(size = 20, colour = "black"),
    strip.background = element_blank())

birds_wetland_graph


#Combine the bar  graphs and export the graph to jpeg

birds_bargraph <- birds_panne_graph + birds_wetland_graph

birds_bargraph


ggsave(birds_bargraph,
       filename = "Avian Analysis\\Figures\\Birds Feeding Guilds and Wetland Score Bar Graphs.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 10, width = 16)




# Regressions of Wetland Score and Panne Dependent Species Over Runnel Age



# Graph 4: Linear Regression of Mean Site Wetland Score per Season over Runnel Age

birds_wetland_runnel <- birds_wetland %>%
  filter(Treatment == "Runnel") %>%
  group_by(PointID, State, Site, runnel_age) %>%
  summarise(Score.m = mean(Score, na.rm = TRUE),
            Score.se = mean(Score)/sqrt(n())) %>%
  mutate(Score.m = round(Score.m, 2),
         Score.se = round(Score.se, 2)) %>%
  ungroup()

write.csv(birds_wetland_runnel,
          "Avian Analysis\\Output Stats\\Weighted Wetland Score by Runnel Age Descriptive Stats.csv")


runnel_wetland_graph <- ggplot(birds_wetland_runnel,
                                  aes(x = runnel_age, y = Score.m)) +
  geom_point(aes(colour = State),
             size = 6) + 
  geom_text_repel(aes(x = runnel_age - 0.25,
                      y = Score.m+ 0.25,
                      label = Site)) + 
  geom_smooth(aes(),
              method = "lm", colour = "black") + 
  scale_y_continuous(limits = c(0, 6),
                     breaks = seq(0, 6, 1),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-2, 13),
                     breaks = seq(-2, 13, 2),
                     expand = c(0,0)) + 
  labs(y = "Mean Weighted Wetland Score",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.80),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

runnel_wetland_graph


# Graph 5: Linear Regression of panne dependent species per Season over Runnel Age

#Calculate the mean count of each feeding guilt for each SHARP Point per season
birds_feeding_runnel <- birds_feeding %>%
  group_by(PointID, Site, State, VisitNum, Year, Treatment) %>%
  mutate(total = sum(Count[feeding_guild == "totalbirds"]),
         percent_community = Count / total) %>%
  ungroup() %>%
  filter(Treatment == "Runnel") %>%
  group_by(Site, State, runnel_age, feeding_guild) %>%
  summarise(percent_community.m = mean(percent_community, na.rm = TRUE) * 100,
            percent_community.se = sd(percent_community)/sqrt(n()) * 100) %>%
  mutate(percent_community.m = round(percent_community.m, 2),
         percent_community.se = round(percent_community.se, 2)) %>%
  ungroup() %>%
  filter(feeding_guild == "Panne_Dependent" | feeding_guild == "MARSH" |
           feeding_guild == "SCAVENGERS" | feeding_guild == "AERIAL" |
           feeding_guild =="UPLAND")


write.csv(birds_feeding_runnel,
          "Avian Analysis\\Output Stats\\Feeding Guild Per Runnel Age Descriptive Stats.csv")


runnel_feeding_graph <- ggplot(filter(birds_feeding_runnel, feeding_guild == "Panne_Dependent"),
                                  aes(x = runnel_age, 
                                      y = percent_community.m)) +
  geom_point(aes(colour = State),
             size = 6) + 
  geom_text_repel(aes(x = runnel_age - 0.25,
                      y = percent_community.m + 0.25,
                      label = Site)) + 
  geom_smooth(aes(),
              method = "lm", colour = "black") + 
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-2, 13),
                     breaks = seq(-2, 13, 2),
                     expand = c(0,0)) + 
  labs(y = "% Community of Panne Dependent Species",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.80),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

runnel_feeding_graph


#Combine the regression graphs and export the graph to jpeg

runnel_regression <- runnel_wetland_graph / runnel_feeding_graph

runnel_regression


ggsave(runnel_regression,
       filename = "Avian Analysis\\Figures\\Birds Runnel Regressions Graphs.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 14, width = 16)



#Chapter 



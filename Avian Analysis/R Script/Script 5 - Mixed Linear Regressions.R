#Project: Avian Community Analysis of SHARP Point Counts of ACJV Project
#Script: Part V - Mixed Linear Regressions of Runnel Age
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
library(broom)
library(lme4)
library(afex)
library(ggeffects)
library(stringr)
library(MuMIn)

#Graphing Packages
library(ggplot2)
library(patchwork)
library(ggrepel)
library(ggResidpanel)




#Chapter 2: Load the 0 - 50 m SHARP Point Count dataset (surveys averaged per monitoring season)
# Dataset is formatted to remove feeding habits and individual species
# Only SHARP points of 'runnel' treatment are retained for the regressions

#Upon exploratory analysis of the data over runnel age, it seemed that there might be a different trajectory
# for sites in the North Shore of Massachusetts (north of Cape Cod) and in the Narragansett Bay (south of Cape Cod)

#When importing the bird dataset, we will do the following:
# Remove the Moody Marsh Site (not tied to either Mass or Narragansett Bay),
# Change the state of Broad Cove to 'Rhode Island' since it is in the Narragansett Bay Estuary system
# Add a 'Region' classifier that divides the sites between North Shore Mass and Narragansett Bay

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird 50m Dataset - Surveys Averaged.csv") %>%
  select(-X, -c(Aerial:Wading), -c(AGWT:YEWA)) %>%
  filter(!is.na(runnel_age), Site != "Moody Marsh") %>%
  filter(Site != 'Broad Cove' | Treatment != 'No Action') %>%
  mutate(State = ifelse(str_starts("Broad Cove", Site), "RI", State)) %>%
  mutate(Region = ifelse(State == "RI", "Narragansett Bay", 
                         ifelse (State == "MA", "North Shore Mass", "Above Cape Cod")))



#Chapter 3: Mixed Linear Regressions of Avian Community Metrics over Runnel Age

#Analysis Notes:
# Metrics to be tested: Salt Marsh Sparrows, Total Bird Abundance, Wetland Score, Feeding Habit, Richness, Shannon Diversity

#The fixed effects for the model are: Runnel Age, Region, and their interaction
# The random effects for the model are: Nested design of Site and SHARP Point
# If square-root transformations are conducted and significant regression is found, 
  # the predicted points of the regression and confidence intervals will be back transformed

# After each analysis, the regression is graphed to see the immediate result



#Analysis 1: Total Bird Abundance

#Note - square-root transformation needed to improve QQ Plot and normality

total <- lmer(sqrt(totalbirds) ~ runnel_age * Region + (1|PointID),
               data = birds)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(total)

#Returning the ANOVA table for the model
total_anova <- tidy(anova(total)) %>%
  mutate(metric = 'totalbirds')

total_anova

#Returning the Fixed and Random Effects table of the model
total_tidy <- broom.mixed::tidy(total) %>%
  mutate(metric = 'totalbirds')

total_tidy

r.squaredLR(total)

#Returning the Summary of the Model to copy + paste the Variance Explained by Random Effects

summary(total)

# Graph the linear regression (feel free to remove hashtag to see individual site labels)

total_graph <- ggplot(birds,
                      aes(x = runnel_age, y = totalbirds)) +
  geom_point(aes(fill = Region),
             pch = 21, 
             size = 6) + 
 # geom_text_repel(aes(
  #  y = totalbirds + 0.15,
   # label = Site)) + 
  scale_y_continuous(limits = c(-0.5, 30),
                     breaks = seq(0, 30, 5),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-0.5, 10.5),
                     breaks = seq(0, 10.5, 2),
                     expand = c(0,0)) + 
  labs(y = "Total Bird Abundance",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.20, 0.85),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

total_graph



#Analysis 2: Salt Marsh Sparrows

#For the marsh sparrow metric, we further subsetted the dataset to only include sites
# where  marsh sparrows were observed at least once over two years of monitoring

sparrows <- birds %>%
  group_by(PointID) %>%
  mutate(sparrow_count = sum(saltysparrow)) %>%
  ungroup() %>%
  filter(sparrow_count > 0) %>%
  select(-sparrow_count)

#Marsh sparrow model

salty <- lmer(sqrt(saltysparrow) ~ runnel_age * Region + (1|PointID),
              data = sparrows)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(salty)



#Returning the ANOVA table for the model
salty_anova <- tidy(anova(salty)) %>%
  mutate(metric = 'saltysparrow')

salty_anova

#Returning the Fixed and Random Effects table of the model
salty_tidy <- broom.mixed::tidy(salty) %>%
  mutate(metric = 'saltysparrow')

salty_tidy

r.squaredLR(salty)

#Returning the Summary of the Model to copy + paste the Variance Explained by Random Effects

summary(salty)


#Predicted values of the Marsh Sparrow Model

#ggpredict() generates the predicted values of salt marsh abundance over each runnel age
predicted_sparrow <- ggpredict(salty, term = c("runnel_age[all]"),
                               #back.transform set to false, so I will manually backtransform the predicted values
                               back.transform = FALSE, 
                               interval = 'confidence',
                               type = 'fixed') %>%
  rename(runnel_age = x) %>%
  #Confidence interval can't be below zero for back transformation purposes and impossible to have less than zero sparrows
  mutate(conf.low = ifelse(conf.low < 0, 0, conf.low),
        conf.high = ifelse(conf.high < 0, 0, conf.high)) %>%
  #Back transformation by squaring the predicted values and confidence interval values
  mutate(across(c(predicted, conf.low, conf.high),
               ~.^2))

#Graph the regression

sparrow_graph <- ggplot() + 
 # geom_ribbon(data = predicted_sparrow,
  #            aes(x = runnel_age, ymin = conf.low, ymax = conf.high),
   #           size = 1, linetype = 'dashed',
    #          colour = 'black', fill = 'lightgrey', alpha = 0.50) + 
 # geom_line(data = predicted_sparrow,
  #          aes(x = runnel_age, y = predicted),
   #         linewidth = 1.5) +
  geom_point(data = sparrows,
             aes(y = saltysparrow, x = runnel_age, fill = Region),
             pch = 21, size = 6) + 
 # geom_text_repel(data = sparrows,
  #                aes(y = saltysparrow + 0.15, x = runnel_age, label = Site)) + 
  scale_y_continuous(limits = c(-0.5, 15),
                     breaks = seq(0, 15, 3),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-0.5, 10.5),
                     breaks = seq(0, 10.5, 2),
                     expand = c(0,0)) + 
  labs(y = "Marsh Sparrow Abundance",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

sparrow_graph




#Analysis 3: Panne and Pool species

#Analysis Notes - square root transformation to improve anova assumptions of normality of data

panne <- lmer(sqrt(Panne_Dependent) ~ runnel_age * Region + (1|PointID),
              data = birds)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(panne)


#Returning the ANOVA table for the model
panne_anova <- tidy(anova(panne)) %>%
  mutate(metric = 'panne_dependent')

panne_anova

#No significant regression found, so no need to predict the values of the regression

#Returning the Fixed and Random Effects table of the model
panne_tidy <- broom.mixed::tidy(panne) %>%
  mutate(metric = 'panne_dependent')

panne_tidy

#Returning the Summary of the Model to copy + paste the Variance Explained by Random Effects

summary(panne)


#No significant regression found, so no need to use ggpredict()

#Graph the community composiiton of panne and pool species

panne_graph <- ggplot(birds,
                aes(x = runnel_age, y = Panne_Dependent)) +
  geom_point(aes(fill = Region),
             pch = 21,
             size = 6) + 
  #geom_text_repel(aes(
   # y = Panne_Dependent + 0.15,
    #label = Site)) + 
  
  scale_y_continuous(limits = c(-1, 100),
                     breaks = seq(0, 100, 25),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-01, 11),
                     breaks = seq(0, 11, 2),
                     expand = c(0,0)) + 
  labs(y = "Panne and Pool Species (%)",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

panne_graph


#Analysis 4: Weighed Wetland Score

#Analysis notes - no transformation or outliers removed

wetland <- lmer(weighted_wetland_score ~ runnel_age * Region + (1|PointID),
                data = birds)


#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(wetland)


#Returning the ANOVA table for the model
wetland_anova <- tidy(anova(wetland)) %>%
  mutate(metric = 'weighted_wetland_score')

wetland_anova

#Returning the Fixed and Random Effects table of the model
wetland_tidy <- broom.mixed::tidy(wetland) %>%
  mutate(metric = 'weighted_wetland_score')

wetland_tidy

r.squaredLR(wetland)


#Returning the Summary of the Model to copy + paste the Variance Explained by Random Effects

summary(wetland)

#Predicted values of the Marsh Sparrow Model

#Predicts the wetland score over runnel age for each Region
predicted_wetland <- ggpredict(wetland, term = c("runnel_age[all]" , "Region[all]"),
                               back.transform = FALSE, 
                               interval = 'confidence',
                               type = 'fixed') %>%
  rename(runnel_age = x,
         Region = group)  %>%
  mutate(conf.low = ifelse(conf.low < 0, 0, conf.low),
                           conf.high = ifelse(conf.high < 0, 0, conf.high))

#Graph the wetland score regression

wetland_graph <- ggplot() +
  geom_ribbon(data = predicted_wetland,
              aes(x = runnel_age, ymin = conf.low, ymax = conf.high,
                  colour = Region, fill = Region),
              size = 1, linetype = 'dashed', alpha = 0.50) + 
  geom_line(data = predicted_wetland,
            aes(x = runnel_age, y = predicted,
                colour = Region),
            linewidth = 1.5) +
  geom_point(data = birds,
             aes(x = runnel_age, y = weighted_wetland_score,
                 fill = Region),
             pch = 21, size = 6) + 
 # geom_text_repel(data = birds,
  #                aes(x = runnel_age, y = weighted_wetland_score,
  #          label = Site) + 
  scale_y_continuous(limits = c(-0.5, 6),
                     breaks = seq(0, 6, 1),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-0.5, 11),
                     breaks = seq(0, 11, 2),
                     expand = c(0,0)) + 
  labs(y = "Weighted Wetland Habitat Score",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

wetland_graph






#Analysis 5: Shannon Diversity

#Analysis notes - no need for transformation, outlier removal

shannon <- lmer(shannon ~ runnel_age * Region + (1|PointID),
                data = birds)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(shannon)


#Returning the ANOVA table for the model
shannon_anova <- tidy(anova(shannon)) %>%
  mutate(metric = 'shannon')

shannon_anova

#Returning the Fixed and Random Effects table of the model
shannon_tidy <- broom.mixed::tidy(shannon) %>%
  mutate(metric = 'shannon')

shannon_tidy

#Returning the Summary of the Model to copy + paste the Variance Explained by Random Effects

summary(shannon)


#No significant regression found, so no need to predict the values of the regression


shannon_graph <- ggplot(birds,
                        aes(x = runnel_age, y = shannon)) +
  geom_point(aes(fill = Region),
             size = 6,
             pch = 21) + 
 # geom_text_repel(aes(y = shannon, label = Site)) + 
  scale_y_continuous(limits = c(0, 3),
                     breaks = seq(0, 3, 0.5),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-1, 11),
                     breaks = seq(0, 11, 2),
                     expand = c(0,0)) + 
  labs(y = "Shannon Diversity",
       x = "Runnel Age") +
  theme_bw() +
  theme(
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

shannon_graph




#Analysis 6: Species Richness

#Analysis notes - no need for transformation, outlier removal

species_richness <- lmer(richness ~ runnel_age * Region + (1|PointID),
                         data = birds)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(species_richness)

boxplot.stats(birds$richness)$out

#Returning the ANOVA table for the model
species_richness_anova <- tidy(anova(species_richness)) %>%
  mutate(metric = 'richness')

species_richness_anova

#Returning the Fixed and Random Effects table of the model
species_richness_tidy <- broom.mixed::tidy(species_richness) %>%
  mutate(metric = 'richness')

species_richness_tidy


#Returning the Summary of the Model to copy + paste the Variance Explained by Random Effects

summary(species_richness)

#No significant regression found, so no need to predict the values of the regression

richness_graph <- ggplot(birds,
                  aes(x = runnel_age, y = richness)) +
  geom_point(aes(fill = Region),
             size = 6,
             pch = 21) + 
  #geom_text_repel(aes(y = richness, label = Site)) + 
  scale_y_continuous(limits = c(0, 9),
                     breaks = seq(0, 9, 3),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-1, 11),
                     breaks = seq(-2, 10, 2),
                     expand = c(0,0)) + 
  labs(y = "Species Richness",
       x = "Runnel Age") +
  theme_bw() +
  theme(
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

richness_graph



#Lastly, I want to combine all of the regressions into one single, large graph


regression_graph = (total_graph + sparrow_graph) / (panne_graph + wetland_graph) / (richness_graph + shannon_graph)

regression_graph

#Save the Graph 

ggsave(regression_graph,
       filename = "Avian Analysis\\Figures\\Mixed Linear Regressions.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 12, width = 16)


#Chapter 5: Combine the ANOVA and Model tables and export

# Combine the ANOVA Tables

anova_tables <- rbind(total_anova, salty_anova, panne_anova, wetland_anova, species_richness_anova, shannon_anova)

write.csv(anova_tables,
          "Avian Analysis\\Output Stats\\Mixed Regression Tables Compiled.csv")

#Combine the model tidy tables 

tidy_tables <- rbind(total_tidy, salty_tidy, panne_tidy, wetland_tidy, species_richness_tidy, shannon_tidy)

write.csv(tidy_tables,
          "Avian Analysis\\Output Stats\\Mixed Regression Model Tables Compiled.csv")


#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 4 - Graphing Tidal Water Elevations

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire




# Script 4 (this code), graphs the 30-day continuous water level elevations from the 'Formatted WLR 
# dataset' with horizontal reference lines of the marsh platform and rootzone elevation. The code
# is highly editable by the user to format the graph to their purposes. The code for the graph was
# designed for automation. 


#Chapter 1: Library of packages for necessary work

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(ggplot2)
library(scales)

#Chapter 2: Import Datasets

#To graph, we will need to import all three datasets:
# 1) Formatted water level elevation dataset
# 2) Individual water level recorder and elevation dataset
# 3) Sparrow island elevation dataset

#Import formatted water level elevation dataset

Logger_Name = "NAC"

Creek_Logger = "Creek"

Site = "Essex South"

Year = "2024"


#First input is the formatted water level elevations from the Reformat R Code
# The code removes the pesky "X" column created to label row names, then 
# formats the Date.Time column to a POSIXct format

wlr_format <- read.csv(paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  rename(WLR = Logger_Name,
        Creek = Creek_Logger) %>%
  filter(!is.na(Creek_Logger),
         !is.na(WLR),
         !is.na(Date.Time))

glimpse(wlr_format)


#Second input is the marsh platform and root zone elevations

elevs <- read.csv("Input Data\\EssexSouth_MarshElevations_2024.csv") %>%
  filter(WLR == Logger_Name)

glimpse(elevs)

#Optional input is a separate creek when one is not present on site

#external_creek <- read.csv("Boston Creek WLR", 
 #                                          collapse = "")) %>%
  #mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M'))


#Chapter 2: Graph the tidal water elevations

options(scipen = 999)

Tidegraph <- ggplot(data = wlr_format, 
                    aes(x = Date.Time)) + 
  #Creek level elevation line
  geom_line(aes(y = Creek),
                linewidth = 1.25, colour = "blue") + 
  #Groundwater level elevation line
  geom_line(aes(y = WLR),
            linewidth = 1.25, colour = "orange") + 
  #Marsh platform elevation reference line
  geom_hline(yintercept = elevs$Marsh_Elevation, linetype = "dashed", 
             color = "black", linewidth = 1.25) +
  #Marsh root zone elevation reference line
  geom_hline(yintercept = elevs$Rootzone_Elevation, linetype = "dashed", 
             color = "green", linewidth = 1.25) +
  #X and Y Axis Labels
  labs(x = "", y = "Water Elevation (NAVD88 m)") +
  #Scale the x-axis by the minimum and maximum date and times
  scale_x_datetime(limits = c(wlr_format$Date.Time[which.min(wlr_format$Date.Time)] - days(1), 
                              wlr_format$Date.Time[which.max(wlr_format$Date.Time)] + days(1) ),
                   expand = c(0, 0),
                   breaks = "3 days",
                   date_labels = "%m-%d") +
  #Scale the y-axis by the minimum and maximum tidal water elevations
  scale_y_continuous(limits = c(0.8, 2.0),
                     breaks = seq(0.8, 2.0, 0.20), 
                     labels = scales::label_number(accuracy = 0.10)) + 
  #All the fun text resizing and coloring
  theme_bw() +
  theme(
    #legend.position = c(0.125, 0.95),
    #legend.text = element_text(size = 20),
    #legend.title = element_blank(),
    #legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    axis.title.y = element_text(size = 20, colour = "black"))

Tidegraph



#Save the graph to the 'Figures' Folder of the project

ggsave(Tidegraph,
       filename = paste("Figures\\", Site, Logger_Name, Year, " Water Level Elevations Graph.jpg", sep = " "), 
       height = 9, width = 14, 
       units = "in", dpi = 300)


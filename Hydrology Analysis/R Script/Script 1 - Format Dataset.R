#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 1 - Format the Water Level Recorder Dataset


#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire


# Script 1 (this code), of the 'Simplified Water Level Analysis' project formats the continuous water
# level series input dataset (user provided) for analysis and graphing in Scripts 2 - 4. The 
# water level series is formatted by:
#   (1) Reduces the study period to a 30-day lunar cycle, anchored in the middle of the study period


#Chapter 1: Library of packages for necessary work

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)


#Chapter 2: Import the water level elevation time series dataset

wlr <- read.csv("Input Data\\Essex South 2024 WLRs.csv") %>%
  filter(!is.na(Date.Time),
         Date.Time != "")

glimpse(wlr)


# The logger name is recorded in the CSV and Image exports for all formatted datasets, 
# tidal regime statistics, flooding statistics, and water level elevation graphs in
# the whole R project. 'Logger_Name' has no impact on the functionality of the code. 

Site <- "Essex South"


#Chapter 3: Format the Continuous Water Level Elevation dataset

# Convert the 'Date.Time' column to a POSIT data type to make data calculations in 'lubridate' 
# package easier to reduce the entire dataset to 30-day lunar tidal cycle

#Step 1: Conversion of Date.Time

wlr_format <- wlr %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M'))

glimpse(wlr_format)

Year <- min(unique(year(wlr_format$Date.Time)))
  
#Step 2: Reduce the dataset to the 30 day lunar tidal cycle

#For research methods for the lab at University of New Hampshire, we normally deploy the WLRs for 
# roughly 30 - 40 days. We usually are not able to collect the WLRs perfectly at the end of the 
# lunar tidal cycle. We decided to subset the WLR data set to roughly a spring tidal cycle (~30 days). 
# We calculate the exact center date in the monitoring period and +/- 15 days. 

lunar_cycle <- data.frame(matrix(nrow = 1, ncol = 8)) %>%
  setNames(c("Site_Name", 'Year', 'Deployment_Duration', 'Deployment_Days', 'Analysis_Duration', 'Analysis_Days',
             'Start_Date', 'End_Date')) %>%
  mutate( Site_Name = Site,
          WLR = colnames(wlr_format)[2],
          Year = year(wlr_format$Date.Time[1]),
    
    #Deployment time is calculated as the total duration of the water level elevation dataset
    #Deployment time is calculated with the as.duration() function that converts the entire dataset to the number of seconds
    #Deployment time is then divided by the number of days to calculate the deployment time in days
        
          Deployment_Days = as.duration(min(wlr_format$Date.Time) %--% max(wlr_format$Date.Time))/ddays(1),
          Deployment_Duration = paste(min(wlr_format$Date.Time), max(wlr_format$Date.Time), sep = " - "),
    
    #Middle, Start, and End dates are calculated with the lubridate() package with quick mathematics of dates
    #Currently, start and end dates are calculated as +/- 15 days from the middle date
         Middle_Date = days(round(Deployment_Days/ 2, 0)) + wlr_format$Date.Time[1],
         
         Start_Date = Middle_Date - days(15),
         End_Date = Middle_Date + days(15),
    
          Analysis_Days = as.duration(Start_Date %--% End_Date) / ddays(1),
          Analysis_Duration = paste(Start_Date, End_Date, sep = " - "))

glimpse(lunar_cycle)

write.csv(lunar_cycle,
          paste("Output Stats\\", Site, Year, "WLR Metadata.csv", 
                collapse = ""))


#Step 3: Next filter the 'wlr' dataset within the parameters of the start and end dates

wlr_subset <- wlr_format %>%
  filter(Date.Time > lunar_cycle$Start_Date,
         Date.Time < lunar_cycle$End_Date)

#Step 4: Export the formatted continuous water level recorder dataset 

write.csv(wlr_subset,
          paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                collapse = ""))



#Continue onto the Creek Hydrology Analysis code to describe the tidal regime for each creek WLR


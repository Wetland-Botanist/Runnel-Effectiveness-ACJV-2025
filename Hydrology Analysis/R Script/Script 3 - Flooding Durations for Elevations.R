#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 3 - Flooding frequency and duration for marsh platform and root zone elevations

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire



# In Script 3 (this code) of the 'Simplified Water Level Recorder Analysis', users provide
# elevations of the marsh platform and root zone (5 cm below ground) in the 'Marsh and Root Zone
# Elevations' input dataset. The code uses the Formatted Water Level Series and the Tidal Regime 
# Water Level Series (list of low and high tides), created in Script 2, to calculate the flooding 
# frequency and flooding duration for given marsh elevations. 

# In the end, the code compiles the flooding statistics and exports them into an easy-to-read CSV file.


#Chapter 1: Library of packages for necessary work------------------------------

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(purrr)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(VulnToolkit)




#Chapter 2: Import the needed datasets-----------------------------------------

Logger_Name <- "NAC"

Creek_Logger <- "Creek"

Site <- "Essex South"

Year <- "2024"

# The needed datasets are: (1) Formatted continuous water level elevations series,
#                          (2) Tidal regime series (list of low and high tides), and
#                          (3) Marsh Platform and Root zone Elevations (user provided)

# The first input is the formatted continuous water level elevation series

wlr_format <- read.csv(paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  dplyr::select(-X) %>%
  rename(WLR = Logger_Name) %>%
  select(Date.Time, WLR) %>%
  filter(!is.na(Creek_Logger),
         !is.na(WLR),
         !is.na(Date.Time))

glimpse(wlr_format)

#The second input is the tidal hydrology (low and high tides) of the creek water level recorders

creek_tides <- read.csv(paste("Formatted Datasets\\", Site, Year, "Tidal Hydrology Dataset.csv", 
                        collapse = "")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S'))

glimpse(creek_tides)



#The third input is a dataframe that contains the elevations of the marsh platform and root zone

elevs <- read.csv("Input Data\\EssexSouth_MarshElevations_2024.csv") %>%
  filter(WLR == Logger_Name)

glimpse(elevs)



#Chapter 3: Calculate the Flooding Duration of the Marsh Platform and Rootzone Elevations--------------------

#First, assess the flooding duration of the marsh platform and root zone elevation

#To accomplish using the fld.dur() function, which returns the flooding duration (as a percent of monitoring time)

#Additionally, we will create two new columns that "pulls" the marsh platform and root zone elevations from
#the 'elevs' dataframe.

wlr_flood <- wlr_format %>%
  summarise(marsh_flood = fld.dur(z = elevs$Marsh_Elevation,
                                  level = WLR) * 100,
            root_flood = fld.dur(z = elevs$Rootzone_Elevation,
                                 level = WLR) * 100,
            
            marsh_elev = elevs$Marsh_Elevation,
            
            
            root_elev = elevs$Rootzone_Elevation) %>%
  ungroup() %>%
  mutate(across(marsh_flood:root_flood, ~round(., 1)))

glimpse(wlr_flood)

# Reformat the flooding duration for easier input into the overall statistics table later on (Chapter 4)

wlr_flood_format <- wlr_flood %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = Flood_Duration_Percent, marsh_flood, root_flood) %>%
  mutate(zone = ifelse(zone == "marsh_flood", "Marsh Platform", "Root Zone")) %>%
  arrange(zone)






# Chapter 4: Assess the high tide flooding frequency of the marsh platform and root zone elevations----------------------------

#Step 3: We calculate the high tide flooding frequency (%) with the fld.frq() function
# The function requires two inputs:   
# (1) z = tidal hydrology (water elevations) from the creek tides dataset
# (2) ht = list of high tides

# We will pull the ht (list of high tides) from the wlr_tides dataset we created earlier in the code

wlr_freq <- creek_tides %>%
  filter(tide == "H") %>%
  select(-Date.Time) %>%
  summarise(marsh_freq = fld.frq(z = elevs$Marsh_Elevation, ht = level) *100,
            
            root_freq = fld.frq(z = elevs$Rootzone_Elevation, ht = level) * 100,
            
            marsh_elev = elevs$Marsh_Elevation,
            
            root_elev = elevs$Rootzone_Elevation) %>%
  
  ungroup() %>%
#Flooding frequency columns are in a 'list' format, reformat to double and round to 2 decimal points
  mutate(across(c(marsh_freq, root_freq), ~as.numeric(.)),
         across(c(marsh_freq, root_freq), ~round(., 1)))

glimpse(wlr_freq)

# Reformat the high tide flooding frequency for easier input into the overall statistics table later on (Chapter 9)
wlr_freq_format <- wlr_freq %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = HT_Frequency_Percent, marsh_freq, root_freq) %>%
  mutate(zone = ifelse(zone == "marsh_freq", "Marsh Platform", "Root Zone")) %>%
  arrange(zone)

glimpse(wlr_freq_format)




#Chapter 5: Calculate the average depth of low tide------------------------------

#Task 1: Divide the entire monitoring period into 12.5 hour windows to determine low tides

tides_creek_seq_low <- creek_tides %>%
  #Only focusing on low tides to create tidal buffer window
  filter(tide == "L") %>%
  #Calculate the time 6.25 hr before and after each low tide to capture the full tidal window
  mutate(seq_low = Date.Time - minutes(380),
         seq_high = Date.Time + minutes(380)) %>%
  group_by(Date.Time) %>%
  #Create a 12.5 hour buffer (10 min intervals) before and after each low (encompassing the full tidal window),
  #Use the map function to create sequence independently for each WLR, tide type
  reframe(tide_seq = seq(from = seq_low, to = seq_high, by = '10 mins')) %>%
  ungroup() %>%
  #Rename columns for merging 
  rename(Date_Group = "Date.Time",
         Date.Time = "tide_seq")

glimpse(tides_creek_seq_low)

#Task 2: Calculate the minimum tide in each 12.5 hour tidal window

gw_tides_low <- wlr_format %>%
  # Merge the Associated Creek column of the elevations dataset to the water elevations dataset
  #Merge the 1 hour high/low tide sequences with the groundwater elevations dataset
  # based on the Associated_Creek and Date-Time columns
  merge(tides_creek_seq_low, by = c("Date.Time")) %>%
  group_by(Date_Group) %>%
  #Calculate the high tide as the maximum water elevation in the time buffer,
  mutate(tide_elev = min(WLR)) %>%
  ungroup() %>%
#Remove the tidal elevation of the select time to reduce confusion
  select(-WLR) %>%
arrange(Date.Time) %>%
  #Remove duplicates from the dataset
  distinct(Date_Group, .keep_all = TRUE)

glimpse(gw_tides_low)


#Task 3: Calculate the average low tide and then subtract from the marsh platform and root zone elevations

drainage_depth <- gw_tides_low %>%
  summarise(Marsh_Drainage = elevs$Marsh_Elevation - mean(tide_elev),
            RootZone_Drainage = elevs$Rootzone_Elevation - mean(tide_elev)) %>%
  mutate(across(Marsh_Drainage:RootZone_Drainage,
                ~round(., 3)))

glimpse(drainage_depth)



# Chapter 7: Calculate drainage amplitude -------------------------------------------

# Drainage amplitude is an elevation blind metric to evaluate drainage, especially during neap tidal cycles

# The thought is that with drainage enhancement with runnels, the "drainage amplitude" will increase as 
# the "lower low tide" elevation decreases into the root zone compared to staying stagnant on the surface as a pool

# Drainage amplitude is calculated as the difference between Mean Low Tide and the minimum water elevation of the dataset

# For groundwater WLRs, where the tradition definitions do not hold water (*drum and snare*), they are calculated as:
  # LLT = lowest water elevation in a given day


# Task 2: Calculate Lower Low Tide

mlt <- gw_tides_low %>%
  summarise(llt = mean(tide_elev, na.rm = TRUE))
  
mlt
  
#Task 3: Calculate Drainage Amplitude

drainage_amplitude <- round(
  mlt[1,1] - min(wlr_format$WLR), 
                   3)

glimpse(drainage_amplitude)

#Chapter 6: Combine flooding statistics into an exportable CSV file--------------

gw_stats <- data.frame(matrix(nrow = 2,
                               ncol = 8)) %>%
  SetNames(c("WLR", "Season", "Zone", "Elevation", "Flood_Duration_percent",
             "HT_Frequency", "Drainage_Depth_m", "Drainage_Amplitude_m")) %>%
  mutate(
    WLR = Logger_Name,
    Season = Year,
    Zone = c("Marsh Platform", "Root Zone"),
    Elevation = ifelse(Zone == "Marsh Platform", elevs$Marsh_Elevation, elevs$Rootzone_Elevation),
    Flood_Duration_percent = wlr_flood_format$Flood_Duration_Percent,
    HT_Frequency = wlr_freq_format$HT_Frequency_Percent,
    Drainage_Depth_m = ifelse(Zone == "Marsh Platform", drainage_depth$Marsh_Drainage, drainage_depth$RootZone_Drainage),
    Drainage_Amplitude_m = drainage_amplitude[1,1])

glimpse(gw_stats)


write.csv(gw_stats,
          paste("Output Stats\\", Site, Logger_Name, Year, "Flooding Stats.csv", 
                collapse = ""))



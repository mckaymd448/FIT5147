## FIT5147 – Data Exploration and Visualisation

## Assignment Three: Data Exploration and Visualisation Final Project

##Visualising changing environmental factors over the 20th and 21st century.

##Michael McKay
##Student ID: 32270208

#Load required libraries.
library(ggmap)
library(tidyverse)
library(tmaptools)
library(sf)
library(visdat)
library(ggplot2)

## Section 3: Data Wranging.
# 3.1: Atmospheric Greenhouse gas levels.

## Load the data for atmospheric concentrations of greenhouse gases.
# Set directory when raw data can be found.
greenhouse_filepath <- "data/2016-soe-atmosphere-annual-mean-conc-of-ghgs-cape-grim-antarctic-firn-ice-cores-1500-2015.csv"
greenhouse_data <- read_csv(greenhouse_filepath)

# Determine structure of the greenhouse_data set.
str(greenhouse_data)

# Look for missing data in the greenhouse_data set.
vis_miss(greenhouse_data)

#Rename headers of greenhouse data.
names(greenhouse_data)[2] <- "ant_CO2_CH4"
names(greenhouse_data)[3] <- "ant_CH4"
names(greenhouse_data)[4] <- "ant_NO2"
names(greenhouse_data)[5] <- "ant_SGG"
names(greenhouse_data)[6] <- "CG_CO2"
names(greenhouse_data)[7] <- "CG_CH4"
names(greenhouse_data)[8] <- "CG_NO2"
names(greenhouse_data)[9] <- "CG_SGG"

# Merge the CO2, CH4, NO2 and SGG columns into single continuous field.
merged_greenhouse_data <- greenhouse_data %>%
  rename(year = Year) %>%
  filter(year>1856) %>%
  mutate(ant_CO2 = ant_CO2_CH4 - (ant_CH4/1000)) %>%
  mutate(CO2 = if_else(is.na(ant_CO2), CG_CO2, ant_CO2)) %>%
  mutate(CH4 = (if_else(is.na(ant_CH4), CG_CH4, ant_CH4))/1000) %>%
  mutate(NO2 = (if_else(is.na(ant_NO2), CG_NO2, ant_NO2))/1000) %>%
  mutate(SGG = (if_else(is.na(ant_SGG), CG_SGG, ant_SGG))/1000) %>%
  select(year, CO2, CH4, NO2, SGG) %>%
  filter(!is.na(CO2))

# 3.2: High Quality Daily Precipitation (HQDP):

## Import data for daily precipitation.

# Set directory when raw data can be found.
hqdp_data_pathway <- "data/HQ_daily_prcp_txt/fact/"

#List files in the data pathyway set above.
folders<- list.files(path = hqdp_data_pathway)

# Loop for each file within the folder.
for(i in c(1:length(folders))) 
{
  # Put together pathway for file in current iteration of loop.
  file_pathway <- paste(hqdp_data_pathway,folders[i], sep = "")
  
  # Extract information from the header of the file.
  temp_header <- read_delim(file_pathway,delim = " ", col_names = FALSE, trim_ws = TRUE, n_max = 1)
  
  # Extract information from body of file.
  temp_data <- read_delim(file_pathway,delim = " ", col_names = FALSE, trim_ws = TRUE, skip = 1)
  
  # Summarise data in file.
  temp_data <- temp_data %>%
    rename(date = X1, precipitation = X2) %>% # Rename headers to something more meaninful.
    filter(precipitation != '99999.9') %>% # Remove all missing values.
    mutate(year = substr(date, 1, 4)) %>% # Extract year from the date.
    select(year, precipitation) %>% # Select only year and precipitation.  Filter out original date string.
    group_by(year) %>% # Calculate count, min, mean and max precipitation for each year in the current file.
    summarise(count = length(precipitation), mean = mean(precipitation), min = min(precipitation), max = max(precipitation)) %>%
    filter(count > 365 * 0.8) # Filter out any years with less than 80% of daily data available.
  
  # Add a new column containing the location ID, extracted from the header.
  temp_data <- cbind(temp_data,temp_header[2])
  
  # Rename the new column to 'location'.
  temp_data <- temp_data %>%
    rename(location = X2)

  # Create a new data set if this is the first iteration of the loop.  Append if it's not.
  if(i==1){
    hqdp_data <- temp_data
  } else {
    hqdp_data <- rbind(hqdp_data,temp_data)
  }
} 

# Import information regarding each location.
info_pathway <- "data/HQ_daily_prcp_txt/HQDR_stations.txt"
location_import <- read_delim(info_pathway,delim = " ", col_names = FALSE)

# Rename headers to something more meaningful.
location_import <- location_import %>%
  rename(location = X1, latitude = X2, longitude = X3, X4 = X4, location_name = X5)

# Encode the longitude and latitude columns into a 'geography' column.
location_geography <- st_as_sf(location_import, coords = c("longitude", "latitude"), crs = 4326)

# Look up information for each station using 'rev_geocode_OSM'.  Please note this step can take a while to execute.
location_addy <- rev_geocode_OSM(location_geography)

# Append new information from 'rev_geocode_OSM' lookup to the file imported above.
location_info <- cbind(location_import, location_addy)

# Combine territory and state columns into one.  Remove unecessary columns from dataset.
HQDP_location_info <-  location_info %>%
  mutate(state = if_else(is.na(territory), state, territory)) %>%
  select(location, latitude, longitude, location_name, country, state)

# Combine location information dataset with dataset containing precipitation information.
hqdp_merged_data <- merge(HQDP_location_info,hqdp_data,by="location")

# Create output files to turn into charts.
rain_fall_output_dataset <- merge(merged_greenhouse_data,hqdp_merged_data,by='year')

# 3.3: ACORN-SAT Daily:

## Load the data for ACORN surface temperature data.

#Set the file pathway for Max temperature data.
satMax_filepath <- "data/ACORN_sat/acorn_sat_v2.2.0_daily_tmax/"

#List files in the data pathyway set above.
files <- list.files(path = satMax_filepath)

# Loop for each file within the folder.
for(i in c(1:length(files))) 
{
  # Put together pathway for file in current iteration of loop.
  file_pathway <- paste(satMax_filepath,files[i], sep = "")
  temp_data <- read_delim(file_pathway,delim = ",", col_names = TRUE, trim_ws = TRUE)
  
  # Site number and site name are located within the second row (counting header) in columns 3 and 4. 
  temp_site_number <- temp_data[1,3]
  
  # Summarise information in imported file.
  temp_data <- temp_data %>%
    rename(max = `maximum temperature (degC)`) %>% # Simplify the column name for max temperature.
    filter(!is.na(max)) %>% # Remove empty observations.
    mutate(year = substr(date, 1, 4)) %>% # Extract year from the date column.
    select(year, max) %>% # Select year and max temp.  Filter out original date column.
    group_by(year) %>% # Determine count and mean of maximum temperatures.
    summarise(count_max = length(max), max = mean(max)) %>%
    filter(count_max > 365 * 0.8) # Remove any years where less than 80% of data is available.
  
  # Create new column for the site number extracted from header of current file.
  temp_data <- cbind(temp_data,temp_site_number)
  
  # Rename the column name for new column.
  temp_data <- temp_data %>%
    rename(site_number = `site number`)
  
  # Create a new dataset if this is the first iteration of the loop.  Append if it's not.
  if(i==1){
    acorn_max_data <- temp_data
  } else {
    acorn_max_data <- rbind(acorn_max_data,temp_data)
  }
}

#Set the file pathway for Min temperature data.
satMin_filepath <- "data/ACORN_sat/acorn_sat_v2.2.0_daily_tmin/"

#List files in the data pathyway set above.
files <- list.files(path = satMin_filepath)

# Loop for each file within the folder.
for(i in c(1:length(files))) 
{
  # Put together pathway for file in current iteration of loop.
  file_pathway <- paste(satMin_filepath,files[i], sep = "")
  temp_data <- read_delim(file_pathway,delim = ",", col_names = TRUE, trim_ws = TRUE)
  
  # Site number and site name are located within the second row (counting header) in columns 3 and 4. 
  temp_site_number <- temp_data[1,3]
  
  # Summarise information in imported file.
  temp_data <- temp_data %>%
    rename(min = `minimum temperature (degC)`) %>% # Simplify the column name for min temperature.
    filter(!is.na(min)) %>% # Remove empty observations.
    mutate(year = substr(date, 1, 4)) %>% # Extract year from the date column.
    select(year, min) %>% # Select year and max temp.  Filter out original date column.
    group_by(year) %>% # Determine count and mean of maximum temperatures.
    summarise(count_min = length(min), min = mean(min)) %>%
    filter(count_min > 365 * 0.8) # Remove any years where less than 80% of data is available.
  
  # Create new column for the site number extracted from header of current file.
  temp_data <- cbind(temp_data,temp_site_number)
  
  # Rename the column name for new column.
  temp_data <- temp_data %>%
    rename(site_number = `site number`)
  
  # Create a new dataset if this is the first iteration of the loop.  Append if it's not.
  if(i==1){
    acorn_min_data <- temp_data
  } else {
    acorn_min_data <- rbind(acorn_min_data,temp_data)
  }
}

#Combine data into a single dataframe containing min and max information.
acorn_min_max_data <- merge(acorn_max_data,acorn_min_data,by=c('site_number','year'))

#Create a dataframe containing means for the acorn_min_max_data set. 
acorn_mean_data <- as.data.frame((acorn_min_max_data$min+acorn_min_max_data$max)/2)
names(acorn_mean_data)[1] <- "mean"

#Connect to the acorn_min_max_data set.
acorn_min_max_data <- cbind(acorn_min_max_data,acorn_mean_data)

# Load information regarding stations in the above dataframe.
# Import information regarding each location.
info_pathway <- "data/ACORN_sat/acorn_sat_v2_stations.txt"
location_import <- read_delim(info_pathway,delim = " ", col_names = TRUE, trim_ws = TRUE)

# Rename headers to something more meaningful.
location_import <- location_import %>%
  rename(site_number = 'stnnum', latitude = 'lat.', longitude = 'long.', elev = 'elev.', location_name = 'name')

# Encode the longitude and latitude columns into a 'geography' column.
location_geography <- st_as_sf(location_import, coords = c("longitude", "latitude"), crs = 4326)

# Look up information for each station using 'rev_geocode_OSM'.  Please note this step can take a while to execute.
location_addy <- rev_geocode_OSM(location_geography)

# Append new information from 'rev_geocode_OSM' lookup to the file imported above.
location_info <- cbind(location_import, location_addy)

# Combine territory and state columns into one.  Remove unecessary columns from dataset.
acorn_location_info <-  location_info %>%
  mutate(state = if_else(is.na(territory), state, territory)) %>%
  select(site_number, latitude, longitude, location_name, country, state)

# Create dataframe containing information regarding location, min and max by date.
acorn_min_max_data <- merge(acorn_location_info,acorn_min_max_data,by="site_number")

# Create output files to turn into charts.
temp_output_dataset <- merge(merged_greenhouse_data,acorn_min_max_data,by='year')

# Export datasets for EDA and visualisation.
write.csv(temp_output_dataset,'data/output/temperature_data.csv')
write.csv(rain_fall_output_dataset,'data/output/rainfall_data.csv')
write.csv(merged_greenhouse_data,'data/output/greenhouse_data.csv')

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

## Section 4: Exploratory Data Analysis:

# Load the datasets outputted from the file 'mckay_32270208_wrangling.R'.  That file needs to be ran first.
temp_data <- read_csv("data/output/temperature_data.csv")
rainfall_data <- read_csv("data/output/rainfall_data.csv")
greenhouse_data <- read_csv("data/output/greenhouse_data.csv")

## Section 4.1: EDA for the Greenhouse Data set.

# Create a plot to see how the data looks for all attributes except CO2.
plot_greenhouse_data <- greenhouse_data %>%
  select(-X1) %>%
  pivot_longer(cols = -c("year"), names_to = "attribute", values_to = "value") %>%
  filter(attribute!="CO2")

CO2_scatter <- ggplot(plot_greenhouse_data) +
  geom_line(aes(x = year, y = value, group = attribute, colour = attribute)) +
  labs(title = "Concentration of contaminant vs Year") +
  xlab("Year") +
  ylab("Concentration / ppm")

CO2_scatter

# Create a plot to see how the data looks for CO2.
plot_greenhouse_data <- greenhouse_data %>%
  select(-X1) %>%
  pivot_longer(cols = -c("year"), names_to = "attribute", values_to = "value") %>%
  filter(attribute=="CO2")

not_CO2_scatter <- ggplot(plot_greenhouse_data) +
  geom_line(aes(x = year, y = value, group = attribute, colour = attribute)) +
  labs(title = "Concentration of contaminant vs Year") +
  xlab("Year") +
  ylab("Concentration / ppm")

not_CO2_scatter

## Section 4.2: EDA for the HQDP dataset.

# Vis miss analysis of hqdp_merged_data.
vis_miss(rainfall_data)

# Plot a histogram and boxplot for the min, mean and max values.
hist_data <- rainfall_data %>%
  select(state, min, mean, max) %>%
  pivot_longer(col=-c("state"), names_to = "aggregate", values_to = "values")

rainfall_hist <- ggplot(hist_data, aes(x=values)) +
  geom_histogram() +
  facet_wrap(aggregate ~ .)

rainfall_boxplot <- ggplot(hist_data, aes(y=values)) +
  geom_boxplot() + 
  facet_wrap(aggregate ~ .)

rainfall_hist
rainfall_boxplot

# Work out mean, and standard deviation of the aggregated values.
stats_hdqp_data <- hist_data %>%
  select(aggregate, values) %>%
  group_by(aggregate) %>%
  summarise(mean = mean(values), stdev = sd(values))

# Correlation matrix for the CO2 level, maximum and the year for rainfall dataset.
corr_data <- rainfall_data %>%
  select(year, CO2, max) %>%
  group_by(year, CO2) %>%
  summarise(max = mean(max))

cor(corr_data)  
plot(corr_data)

## Section 4.3: EDA for the ACORN-SAT data set.

# Create a data frame to plot a histogram and a box plot for the temperature data.
temp_hist_data <- temp_data %>%
  select(min, max, mean) %>%
  pivot_longer(everything(), names_to = "aggregate", values_to = "values")

temp_hist <- ggplot(temp_hist_data, aes(x=values)) +
  geom_histogram() +
  facet_wrap(aggregate ~ .)

temp_boxplot <- ggplot(temp_hist_data, aes(y=values)) +
  geom_boxplot() + 
  facet_wrap(aggregate ~ .)

temp_hist
temp_boxplot

# Correlation matrix for the CO2 level, maximum and the year for the temperature dataset.
corr_data <- temp_data %>%
  select(year, CO2, min, mean, max) %>%
  group_by(year, CO2) %>%
  summarise(min = mean(min), mean = mean(mean), max = mean(max))

cor(corr_data)  
plot(corr_data)

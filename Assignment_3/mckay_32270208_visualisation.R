## FIT5147 – Data Exploration and Visualisation

## Assignment Three: Data Exploration and Visualisation Final Project

##Visualising changing environmental factors over the 20th and 21st century.

##Michael McKay
##Student ID: 32270208

# Load necessary libraries.  These will need to be installed to use visualisation.  Please uncomment lines below to install packages. 
#install.packages(shiny)
#install.packages(tidyverse)
#install.packages(leaflet)
#install.packages(ggplot2)

library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

    # Sidebar with a slider input for the number of bins
    sidebarLayout(
      sidebarPanel(
        leafletOutput("map"),
        selectInput("state",
                    "State:",
                    c("All Australia",
                      "Australian Capital Territory",
                      "New South Wales",
                      "Northern Territory",
                      "Queensland",
                      "South Australia",
                      "Tasmania",
                      "Victoria",
                      "Western Australia")),
        sliderInput("year",
                    "Year Range",
                    min = 1850,
                    max = 2022,
                    value = c(1800,2022),
                    step = 1),
        selectInput("attribute",
                    "Attribute:",
                    c("Rainfall",
                      "Temperature")),
        selectInput("agg_func",
                    "Aggregate:",
                    c("Mean",
                      "Minimum",
                      "Maximum"))
        ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot",
                   height = 800)
      )
    )
  ))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

  # Load the datasets.
  temp_data <- read_csv("data/output/temperature_data.csv")
  rainfall_data <- read_csv("data/output/rainfall_data.csv")
  
  # Render the line / scatter plot for displaying data.
  output$distPlot <- renderPlot({
    
    # Turn first and second input of slider input bar into their own variables.
    year_start <- input$year[1]
    year_end <- input$year[2]
    
    if (input$attribute=="Rainfall") {
      
      # If "Rainfall" has been selected, use this dataset.  Define the name of left Y-axis.
      filtData <- rainfall_data
      yLeftTitle = "Rainfall / mm"
      
    } else {
      
      # Else, assume "Temperature" has been selected, and use that dataset.  Define the name of left Y-axis.
      filtData <- temp_data
      yLeftTitle = "Temperature / deg C"
      
    }
    
    # Determine the minimum and maximum years in the selected dataset and update the slider bar for year input based on it.
    minYear <- min(filtData$year)
    maxYear <- max(filtData$year)
    
    updateSliderInput(inputId = "year",
                      min=minYear,
                      max=maxYear)
    
    # Filter dataset based on which aggregate function has been selected in the inputSelect box for aggregate.
    if (input$agg_func=="Minimum") {
      
      filtData <- filtData %>%
        select(state, year, CO2, min) %>%
        filter(!is.na(CO2))
      
    } else if (input$agg_func=="Mean") {
      
      filtData <- filtData %>%
        select(state, year, CO2, mean) %>%
        filter(!is.na(CO2))
      
    } else {
      
      filtData <- filtData %>%
        select(state, year, CO2, max) %>%
        filter(!is.na(CO2))
      
    }
    
    # Update Left axis title with aggregate information and attribute information.
    yLeftTitle <- paste(input$agg_func,yLeftTitle,sep=" ")
    
    filtData <- filtData[filtData$year >= input$year[1] & filtData$year <= input$year[2],]
    
    # Filter data by state if anything other than "All Australia" has been selected.
    if (input$state!="All Australia") {
      
      filtData <- filtData %>%
        filter(state==input$state)
      
    }
    
    # Pivot data into a longer form, which will make it easier to plot on the scatter/line plot.
    filtData <- filtData %>%
      select(-state) %>%
      pivot_longer(-c("year","CO2"), names_to = "agg_func", values_to = "values") %>%
      group_by(year, agg_func) %>%
      summarise(mean = mean(values), CO2 = mean(CO2))
    
    # Determine a 'scaleFactor', so the CO2 and the temperature/rainfall data will overlap even if scale is very different.
    max_mean <- max(filtData$mean)
    max_CO2 <- max(filtData$CO2)
    
    scaleFactor <- max_mean / max_CO2
    
    # Create the line/scatterplot.
    ggplot(filtData) + 
      geom_point(aes(x = year, y = mean), color='blue') +
      geom_smooth(aes(x = year, y = mean), method = lm, formula = y ~ x) +
      geom_line(aes(x = year, y = CO2 * scaleFactor), color='red', size=1.5) +
      scale_y_continuous(name=yLeftTitle, sec.axis=sec_axis(~./scaleFactor, name="CO2 / ppm")) +
      labs(title = paste("Plot of",input$attribute," and CO2 concentration (ppm) vs. Year", sep=" ")) +
      xlab("Year") +
      theme(
        axis.title.y.left=element_text(color="blue", size=16),
        axis.text.y.left=element_text(color="blue", size=16),
        axis.title.y.right=element_text(color="red", size=16),
        axis.text.y.right=element_text(color="red", size=16),
        axis.title.x.bottom=element_text(color="black", size=16),
        axis.text.x.bottom=element_text(color="black", size=16),
        plot.title = element_text(size=18)
      )
    
  })
  
  # Render the map to display where the stations are located.
  output$map <- renderLeaflet({
    
    # Descide which dataset to use depending on which attribute has been selected.
    if (input$attribute=="Rainfall") {
      
      coords <- rainfall_data %>%
        select(longitude, latitude, state) %>%
        distinct()
      
    } else {
      
      coords <- temp_data %>%
        select(longitude, latitude, state) %>%
        distinct()
      
    }
    
    # Get a distinct list of states from the coords dataframe.
    selInputChoices <- coords %>%
      select(state) %>%
      arrange(state) %>%
      distinct()
    
    # Add "All Australia" as an option.
    selInputChoices <- rbind("All Australia",selInputChoices)
    
    # Update the select input box with a list of options.
    updateSelectInput(inputId = "state", choices = selInputChoices, selected=input$state)
    
    # Filter based on state if anything other then 'All Australia' has been selected.
    if (input$state!="All Australia") {
      
      coords <- coords %>%
        filter(state==input$state)
    
      } 
    
    # Create the leaflet map.
    leaflet(coords) %>%
      addTiles() %>%
      addControl("Locations of stations", position = "topright", className="map-title") %>%
      fitBounds(~min(longitude),
                ~min(latitude),
                ~max(longitude),
                ~max(latitude)) %>%
      addMarkers(~longitude,
                 ~latitude,
                 clusterOptions = markerClusterOptions())
    
  })
})

# Run App.
shinyApp(ui, server)
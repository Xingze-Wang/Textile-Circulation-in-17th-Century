# @Middlebury College - 2021
# Interactive textile explorer
# Originally authored by Ev Berger-Wolf, Camryn Kluetmeier, Jason Rickenbacker, and Nicholas Sliter
# Under the instruction of Prof. Carrie Anderson at Middlebury College
# Code maintained and extended by Nicholas Sliter
# Code and Dataset maintained and extended by Xingze Wang
# Code maintained and extended by Sanjana Roy

#Project initial
library(DT)
library(shiny)
library(shinythemes)
library(readxl)
library(rgdal)
library(tidyverse)
library(stringr)
library(debkeepr)
library(leaflet)
library(viridis)
library(jsonlite)
library(plotly)
library(gapminder)
library(shinyWidgets)

#source to function file
source('functions.R')


#define constants:
#reference with CONSTANTS["NAME"]
CONSTANTS <- c(
  "SHINY_THEME" = "sandstone",
  "COLOR_THEME" = "magma",
  "COLORS" = toString(c("white","yellow","red","blue","purple","green","black", "brown", "grey", "silver", "gold"))
)


#### CREATE ZOOM LOCATIONS #### 

latLongZoom.original <- data.frame("Area" = c("World", "Europe", "Africa",
                                              "Middle East", "Pacfic Islands", "Asia"),
                                   "Lat" = c(30, 49.8, -6, 27, 0, 32),
                                   "Long" = c(53, 15.47, 30, 72.5, 116, 115),
                                   "Magnify" = c(2, 4.25, 2.5, 4, 4, 3.25))

latLongZoom <- latLongZoom.original

#### READING IN DATA#### 

#Read in the data
#Read in textile data
joined.data.original <- read_csv("textileData.csv")
#Making a second copy
joined.data.original1 <- read_csv("textileData.csv")
#Make copies of original data
joined.data <- joined.data.original


# Read in map data
map.data.original <- readOGR("filteredCountries.GeoJSON")
# Making copy of map data
map.data <- map.data.original


#Creating a modifier choice vector
modVec <- c("Textile Name" = "textile_name",
            #"Color" = "colorGroup",
            "Color" = "colorList",
            "Pattern" = "textile_pattern_arch",
            "Process" = "textile_process_arch",
            "Fiber Type" = "textile_fiber_arch",
            "Geography" = "textile_geography_arch",
            "Quality" = "textile_quality_arch")

#### CREATING THE UI #### 

ui <- fluidPage(theme = shinytheme("sandstone"),
                titlePanel("Interactive Textile Circulation Explorer"),
                
                #### SIDEBAR PANEL ####
                
                sidebarPanel(#All inputs will go in this sidebarPanel
                  #h4("Explore different facets of the data by selecting inputs below:"),
                  
                  #Selecting dataset
                  radioButtons(inputId = "dataSet",
                               label = "Select one or more company of interest",
                               choices = c("West India Company (WIC)", "East India Company (VOC)", "Both"),
                               selected = "West India Company (WIC)"),
                  
                  #Selecting Quantity or Value data to show on map
                  radioButtons(inputId = "dataType",
                               label = "Select one data type of interest",
                               choices = c("Quantity", "Value"),
                               selected = "Quantity"),
                  
                  #Selecting Origin or Destination
                  radioButtons(inputId = "regionChoice",
                               label = "Select one",
                               choices = c("Origin", "Destination"),
                               selected = "Destination"),
                  
                  #Selection to zoom to a region
                  selectizeInput(inputId = "zoomTo",
                                 label = "Zoom to:",
                                 choices = levels(factor(latLongZoom$Area)),
                                 selected = "World"),
                  
                  # selectizeInput(inputId = "textileName",
                  #                label = "Choose textile(s) of interest",
                  #                choices = levels(factor(joined.data$textile_name)),
                  #                multiple = TRUE),
                  # # uiOutput(outputId = 'filtered_inputs'),
                  # selectizeInput(inputId = "colors",
                  #                label = "Choose color(s) of interest",
                  #                #choices = levels(factor(joined.data$colorGroup)),
                  #                #choices = levels(factor(joined.data$colorList)),
                  #                
                  #                choices = {
                  #                  strsplit(CONSTANTS['COLORS'], ", ")[[1]]
                  #                  
                  #                  },
                  #               multiple = TRUE),
                  # selectizeInput(inputId = "patterns",
                  #                label = "Choose pattern(s) of interest",
                  #                choices = levels(factor(joined.data$textile_pattern_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "process",
                  #                label = "Choose process(es) of interest",
                  #                choices = levels(factor(joined.data$textile_process_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "fibers",
                  #                label = "Choose fiber(s) of interest",
                  #                choices = levels(factor(joined.data$textile_fiber_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "geography",
                  #                label = "Choose geography of interest",
                  #                choices = levels(factor(joined.data$textile_geography_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "qualities",
                  #                label = "Choose quality(s) of interest",
                  #                choices = levels(factor(joined.data$textile_quality_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "year",
                  #                label = "Year:",
                  #                choices = levels(factor(c(joined.data$orig_yr,joined.data$dest_yr))),
                  #                multiple = TRUE),
                  
                  
                  # For rendering UI later
                  uiOutput(outputId = "TextileName"),
                  uiOutput(outputId = "Origins"),
                  uiOutput(outputId = "Destinations"),
                  uiOutput(outputId = "Colors"),
                  uiOutput(outputId = "Pattern"),
                  uiOutput(outputId = "Process"),
                  uiOutput(outputId = "Fibers"),
                  uiOutput(outputId = "InferredQualities"),
                  uiOutput(outputId = "Geography"),
                  uiOutput(outputId = "Qualities"),
                  uiOutput(outputId = "Year"),
                  
                  
                  
                  actionButton(inputId = "updateBtn",
                               label = "Click to update map and tables!"),
                  br(), br(),
                  br(), br(), 

                                  
                  #Input selections for the pie chart and bar chart
                  selectInput(inputId = "pieChart",
                              label = "Select a modifier for the pie chart:",
                              choices = modVec,
                              selected = "textile_name"),
                  checkboxInput(inputId = "omitNAs",
                                label = "Omit NAs in charts"),
                  selectInput(inputId = "barChart",
                              label = "Select a modifier for the bar chart:",
                              choices = modVec,
                              selected = "textile_name"),
                  checkboxInput(inputId = "facet",
                                label = "Facet by modifier"),
                  actionButton(inputId = 'graph_updateBtn',
                               label = 'Click to update graphs!')
                ),
                
             
                #### MAIN PANEL ####
                
                mainPanel(
                  tabsetPanel(#All of the outputs go here (introduction, map/graphs, data tables, sources)
                    # tabPanel(title= "Introduction",
                    #          h2("Dutch Textile Trade from 1710 to 1715", align = "center"),
                    #          img(src = "HARC_textiles.png", height = 350, width = 550, style="display: block; margin-left: auto; margin-right: auto;"),
                    #          br(),br(),
                    #          p("Through the seventeenth and early eighteenth centuries, the Dutch Republic – what we would today call the Netherlands – dominated global trade. The Dutch East India Company (VOC for short), chartered in 1602, commanded the Indian Ocean, while the Dutch West India Company (WIC for short), chartered in 1621, sought to gain control over trade in Western Africa and the Americas. The companies traded goods ranging from gold, ivory, and enslaved peoples to sugar, spices, and especially textiles. In fact, of the many types of commodities included on Dutch East and West India Company cargo lists, textiles – of every color and variety – were by far the most numerous. This app is the first in a three part series – which bring together archival, visual, and material data collected by Professors Marsely Kehoe and Carrie Anderson – and will enable scholars in a range of disciplines to make meaningful connections between these data types and thus contribute more broadly to our understanding of historic textiles. Some of the questions our apps aim to answer are:"),
                    #          em("What kinds of and how many textiles were exported/imported by the Dutch East and West Indies Company and where? How did patterns in textile circulation change over time? Which textile types were most popular and in which geographical regions? Which colors? Which patterns? What did these textiles look like? How were they represented in images and what social values did they carry?"),
                    #          br(), br(),
                    #          p("The app that we designed is an interactive map focused on the trade of textiles from 1710 to 1715. The Map Explorer allows the user to choose a company and data type of interest, while filtering by textile modifiers, and displays an interactive world map with a complementary pie chart and bar chart when a specific country is selected. The Table Explorer displays the compiled and cleaned dataset."),
                    #          p("The information presented within this app is messy historical data transcribed from invoices and ledgers that is currently part of a larger ongoing research project investigating interconnected patterns of textile trade in the VOC and WIC. Many of the textile names and types are now obsolete and at present have been cleaned and grouped to the best of our ability using secondary source materials. Historically, the Dutch used the tripartite format of Holland guilders as their currency. Using the debkeepr package developed by Dr. Jesse Sadler, a historian of early modern Europe from Virginia Tech, the currency values are converted in a decimal format for ease of visualization. Uncertainty still remains between differences between Dutch and Indian guilders and unit discrepancies. For the WIC dataset, one piece is equal to one ell (~ 27 inches), but for the VOC dataset this relationship varies."),
                    # ),
                    
                    tabPanel(title = "Data Visualization and Interactive Map",
                             leafletOutput(outputId = "countriesMap"),
                             plotOutput(outputId = "pieChart"),
                             plotOutput(outputId = "barChart") #outputId = 
                    ),
                    tabPanel(title = "Data - Catalog",
                             checkboxInput(inputId = "rownames2",
                                           label = "Show Rownames"),
                             dataTableOutput('update_inputs'),
                             downloadButton("downloadData", "Download Table") #download button
                    )
                    )
                
                  )
                )


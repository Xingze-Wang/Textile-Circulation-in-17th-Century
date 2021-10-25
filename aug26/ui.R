# @Middlebury College - 2021
# Interactive textile explorer
# Originally authored by Ev Berger-Wolf, Camryn Kluetmeier, Jason Rickenbacker, and Nicholas Sliter
# Under the instruction of Prof. Carrie Anderson at Middlebury College
# Code maintained and extended by Nicholas Sliter

## Xingze Wang's summer changes included
## Demo Version



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


#> strsplit((CONSTANTS["COLORS"]), ",")[[1]]


# Create zoom locations
latLongZoom.original <- data.frame("Area" = c("World", "Europe", "Africa",
                                              "Middle East", "Pacfic Islands", "Asia"),
                                   "Lat" = c(30, 49.8, -6, 27, 0, 32),
                                   "Long" = c(53, 15.47, 30, 72.5, 116, 115),
                                   "Magnify" = c(2, 4.25, 2.5, 4, 4, 3.25))

latLongZoom <- latLongZoom.original

#Read in the data
joined.data.original <- read_csv("joined.csv")

joined.data.original1 <- read_csv("joined.csv")

secondarydefinitions = read_excel("TestFactSheetDefinitions.xlsx")


#Data Cleaning - changed all the NA into 0 and "N/A"
joined.data.original1[["orig_yr"]][is.na(joined.data.original1["orig_yr"])]=0
joined.data.original1[["dest_yr"]][is.na(joined.data.original1["dest_yr"])]=0
joined.data.original1[["textile_quantity"]][is.na(joined.data.original1["textile_quantity"])]=0
joined.data.original1[["quant_ells"]][is.na(joined.data.original1["quant_ells"])]=0
joined.data.original1[["units_ells"]][is.na(joined.data.original1["units_ells"])]=0
joined.data.original1[["value_per_piece"]][is.na(joined.data.original1["value_per_piece"])]=0
joined.data.original1[is.na(joined.data.original1)]="N/A"
joined.data.original1$X1 = NULL

#changing textile names
joined.data.original1$textile_name[joined.data.original1$textile_name=="Kannekijns"]<-"Kannekyns"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Carroots"]<-"Corroots"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Pattamaroepoe"]<-"Pattamaraphoe"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Nicanees"]<-"Nickanees"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Deken"]<-"Dekens"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Tannyzijde"]<-"Tannazijde"
joined.data.original <- joined.data.original1

map.data.original <- readOGR("filteredCountries.GeoJSON")

#make copies of original data
joined.data <- joined.data.original

#convert JSON col to nonJSON
#joined.data <- joined.data.original %>% mutate(colorList = vec_unflatten(colorList))


#Fix Facet Wrapping Issue (deal with this after presentation)
#joined.data$textile_quality_inferred <- factor(joined.data$textile_quality_inferred,
#                                              levels = c("Inexpensive", "Mid-Range", "Expensive"))
map.data <- map.data.original

#Creating a modifier choice vector
modVec <- c("Textile Name" = "textile_name",
            #"Color" = "colorGroup",
            "Color" = "colorList",
            "Pattern" = "textile_pattern_arch",
            "Process" = "textile_process_arch",
            "Fiber Type" = "textile_fiber_arch",
            "Value Range" = "textile_quality_inferred",
            "Geography" = "textile_geography_arch",
            "Quality" = "textile_quality_arch")


#Creating the UI
ui <- fluidPage(theme = shinytheme("sandstone"),
                titlePanel("Interactive Textile Circulation Explorer"),
                sidebarPanel(#All inputs will go in this sidebarPanel
                  #h4("Explore different facets of the data by selecting inputs below:"),
                  radioButtons(inputId = "dataSet",
                               label = "Select one or more company of interest",
                               choices = c("West India Company (WIC)", "East India Company (VOC)", "Both"),
                               selected = "West India Company (WIC)"),
                  radioButtons(inputId = "dataType",
                               label = "Select one data type of interest",
                               choices = c("Quantity", "Value"),
                               selected = "Quantity"),
                  radioButtons(inputId = "regionChoice",
                               label = "Select one",
                               choices = c("Origin", "Destination"),
                               selected = "Origin"),
                  
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
                  # selectizeInput(inputId = "inferredQualities",
                  #                label = "Choose value range(s) of interest",
                  #                choices = levels(factor(joined.data$textile_quality_inferred)),
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
                  actionButton(inputId = "updateBtn",
                               label = "Click to update map and tables!"),
                  br(), br(),
                  br(), br(), #The inputs for the pie chart and bar chart
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
                    ),
                    tabPanel(title = "Secondary Sources",
                             checkboxInput(inputId = "rownames1",
                                           label = "Show Rownames"),
                             dataTableOutput('factsheet'),
                             downloadButton("downloadData1", "Download Table")
                    )
                    
                    ##unwanted functions
                    
                    # tabPanel(title = "comparison",
                    #          dataTableOutput('effects'))
                    # tabPanel(title = "Suggested Reading",
                    #          h3("Data Sources (compiled by Marsely Kehoe and Carrie Anderson):"),
                    #          h5("Nationaal Archief, West India Company Archive, nrs. 1290-1296."),
                    #          h5("Huygens ING. Bookkeep-General Batavia/Boekhouder-Generaal Batavia."),
                    #          a("https://bgb.huygens.knaw.nl"),
                    #          h3("Suggested Reading:"),
                    #          h5("Alpern, Stanley B. “What Africans Got for Their Slaves: A Master List of European Goods,” History in Africa, 22 (1995): 5-43."),
                    #          h5("Bruijn, J. R., F. S. Gaastra, and I. Schöffer, with assistance from A. C. J. Vermeulen. Dutch-Asiatic Shipping in the 17th and 18th Centuries. The Hague: Martinus Nijhoff, 1987. Hexham, Henry. Het groot woorden-boeck: gestelt in 't Nederduytsch, end in 't Englisch. Rotterdam: Amount Leers, 1648."),
                    #          h5("Chaudhuri, K. N. The Trading World of Asia and the East India Company, 1660-1760. Cambridge: Cambridge University Press, 1978)."),
                    #          h5("Crill, Rosemary, ed. Textiles from India: The Global Trade. Papers presented at a conference on the Indian textile trade, Kolkata, 12-14 Oct. 2003. Calcutta: Seagull Books, 2006."),
                    #          h5("Crill, Rosemary, ed. The Fabric of India. London: Victoria and Albert Publishing, 2015."),
                    #          h5("Fotheringham, Avalon. The Indian Textiles Sourcebook: Patterns and Techniques. London: Thames and Hudson and Victoria and Albert Museum: 2019."),
                    #          h5("Gillow, John and Nicholas Barnard. Traditional Indian Textiles. London: Thames and Hudson, 1991."),
                    #          h5("Hartkamp-Jonxis, Ebeltje, ed. Sits: Oost-West Relatie in Textiel. Zwolle: Uitgeverij Waanders, 1987. (Hartkamp-Jonxis 1987, p. )"),
                    #          h5("Hexham, Henry. Het groot woorden-boeck: gestelt in 't Nederduytsch, end in 't Engelsch. Rotterdam: Arnout Leers, 1648. "),
                    #          h5("Instituut voor Nederlandse Geschiedenis, VOC-Glossarium. Verklaringen van termen, verzameld uit de Rijks geschiedkundige publicatiën die betrekking hebben op the Verenigde Oost-Indische Compagnie. The Hague: Institute voor Nederlandse Geschiedenis, 2000. "),
                    #          h5("Irwin, John and P. R. Schwartz, Studies in Indo-European Textile History (Ahmedabad India: Calico Museum of Textile, 1966)."),
                    #          h5("Jain, Rahul. Rapture: the art of Indian textiles. New Delhi: Niyogi Books, 2011."),
                    #          h5("Jones, Adam, ed. West Africa in the mid-seventeenth century: an anonymous Dutch manuscript. African Studies Association Press, 1995."),
                    #          h5("Oliver, Liza. Art, Trade, and Imperialism in Early Modern French India. Amsterdam: Amsterdam University Press, 2019."),
                    #          h5("Peck, Amelia, ed. Interwoven Globe: The Worldwide Textile Trade, 1500-1800 (New York: Metropolitan Museum of Art, 2013). exh. cat."),
                    #          h5("Ratelband, K., ed. Vijf Dagregisters van het kasteel São Jorge da Mina (Elmina) aan de Goudkust (1645-1647). The Hague: Martinus Nijhoff, 1953. (Ratelband 1953, p. cix)"),
                    #          h5("Riello, Giorgio and Prasannan Parthasarathi, eds. The Spinning World: A Global History of Cotton Textiles, 1200-1850. Oxford University Press, 2009."),
                    #          h5("Sangar, Satya Prakash. Indian Textiles in the Seventeenth Century. New Delhi: Reliance Publishing House, 1998."),
                    #          h5('Van Groesen, Michiel. "Global Trade." In H. Helmers & G. Janssen, eds., The Cambridge Companion to the Dutch Golden Age. Cambrudge Companions to Culture, 166-186. Cambridge: Cambridge University Press, 2018.'),
                    #          h5("Whitechapel Art Gallery. Woven air: the muslin & kantha tradition of Bangladesh. London: Whitechapel Art Gallery, 1988.")
                    # )
                  )
                )
)



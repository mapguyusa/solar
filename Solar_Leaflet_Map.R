#-------------------------------
# Interactive Map of Solar-Pollinator States
#   Using Leaflet
#   August 9, 2018
#-------------------------------
#
#Call the libraries
#install.packages('RCurl')
#install.packages('geojson')
#install.packages('geojsonio')
library(RCurl)
library(dplyr)
library(rgdal)
library(leaflet)
library(maps)
library(geojson)
library(geojsonio)
#
#Delete the Global Environment
rm(list=ls())
#
#Set the Working Directory
setwd("P:/PROJECTS/Pollinators/R")
#
#---------------------------------------------------------
###1 Import the Solar States Summary CSV File
#---------------------------------------------------------
#There should be 48 obs, 4 variables
#
#Read straight from GITHUB
SolarStates <- read.csv("https://raw.githubusercontent.com/mapguyusa/solar/master/SolarPollinatorStates_NEW.csv")
#
#Read from local machine
#SolarStates <- read.csv("P:/PROJECTS/Pollinators/R/SolarPollinatorStates_NEW.csv")
#---------------------------------------------------------
#
sapply (SolarStates, typeof)
#
# Round the Ag area and MW to integer and Thousand Separator (e.g., 1,000)
SolarStates$MW_Label <- format(as.integer(round((SolarStates$Solar_MW), 0)), big.mark = ",")
SolarStates$AG_Label <- format(as.integer(round((SolarStates$TP_AG_AC), 0)), big.mark = ",")
##
#---------------------------------------------------------
# Import the states spatial polygons using geojsonio
# transfrom .json file into a spatial polygons data frame
#---------------------------------------------------------
states <- geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp")
#plot(states)
# check the class of the object
class(states)
names(states)
#
#--------------------------------------------------------
# Subset the polygons down to the lower 48
#      AND APPEND THE DATASETS
#--------------------------------------------------------
States48 <- subset(states, !(name %in% c('Alaska', 'District of Columbia', 'Hawaii', 'Puerto Rico')))
#
# Append the Total Solar MW (MW) and Total Pollinator Ag (AG)
States48$MW <- SolarStates$Solar_MW[match(States48$name, SolarStates$STATE)]
States48$AG <- SolarStates$TP_AG_AC[match(States48$name, SolarStates$STATE)]
States48$MW_Label <- SolarStates$MW_Label[match(States48$name, SolarStates$STATE)]
States48$AG_Label <- SolarStates$AG_Label[match(States48$name, SolarStates$STATE)]
#
#View the head of the data attribute table
head(States48@data)
#
#check variable types
#is.numeric(States48$MW) #should be true
#is.numeric(States48$AG) #should be true
#
#convert string to double (if needed)
#States48$MW = as.double(States48$MW)
#States48$AG = as.double(States48$AG)
#
#-----------------------------------------------------
#
#  NOW MAP IT OUT!
#
#-----------------------------------------------------
#create bins
bins.AG <- c(0, 500, 1000, 5000, 10000, 50000, 100000, 250000)
bins.MW <- c(0, 10, 100, 500, 1000, 5000, 10000, 15000)
#
#color palettes
pal.AG <- colorBin("Greens", domain=States48$AG, bins=bins.AG)
pal.MW <- colorBin("Reds", domain=States48$MW, bins=bins.MW)
#
#Label Information
labels <- sprintf(
  "<strong>%s</strong><br/>%s MW <font size = 1>Total Utility-Scale Solar Development<SUP>1</SUP></font><br/>%s Acres <font size = 1> Pollinator-dependent agriculture Within 1.5 km of Solar Facilities<SUP>2</SUP></font>",
  States48$name, States48$MW_Label, States48$AG_Label
) %>% lapply(htmltools::HTML)
#
#Run the Leaflet
leaflet(States48)%>%
  setView(-87, 35, 4)%>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light", accessToken = 'pk.eyJ1IjoibHdhbHN0b24iLCJhIjoiY2poN3p4eDhoMDFlZTJ3cW1udDRwZ3A2aSJ9.P2dL7O_R3ZqOyUpRfLpk_w', minZoom=5, maxZoom=7)) %>%
  addPolygons(group = "Solar Development", fillColor = ~pal.MW(MW), weight = 2, opacity = 1, color = "gray", dashArray = "3", fillOpacity = 0.7,
              highlight = highlightOptions(weight=5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto"))%>%
  addPolygons(group = "Agriculture", fillColor = ~pal.AG(AG), weight = 2, opacity = 1, color = "gray", dashArray = "3", fillOpacity = 0.7,
              highlight = highlightOptions(weight=5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto")) %>%
  #Add Legend
  addLegend(group = "Agriculture", pal=pal.AG, values=~AG, opacity=0.7, title="<p>Amount of Pollinator-Dependent Agriculture</p><p>Within 1.5 km of Solar Facilities (acres)</p>", position = "bottomright")%>%
  addLegend(group = "Solar Development", pal=pal.MW, values=~MW, opacity=0.7, title="<p>Total Utility-Scale Solar Development Capacity(MW)<SUP>1</SUP></p>", position = "bottomleft")%>%
  #Add Layers Control
  addLayersControl(baseGroups = c("Agriculture", "Solar Development"), options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  








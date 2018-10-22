#-------------------------------
# Mapping EIS Solar Facilities
#
#  Using R Leaflet
#-------------------------------

#Call the libraries
#install.packages('RCurl')
#install.packages('geojson')
#install.packages('geojsonio')
library(dplyr)
library(rgdal)
library(leaflet)
library(geojson)
library(geojsonio)
#
#Delete the Global Environment
rm(list=ls())
#
#Set the Working Directory
setwd("P:/PROJECTS/Pollinators/R")
getwd()
#
#---------------------------------------------------------
###1 Import the 2017 EIS Solar Sites
#---------------------------------------------------------
#
#Read straight from GITHUB
EIA2017 <- read.csv("https://raw.githubusercontent.com/mapguyusa/solar/master/SolarSites_EIA_2017.csv", stringsAsFactors = FALSE)
head(EIA2017)
#
#Subset to Desired Fields Only
Keep <- c("UTILITY_NAME", "PLANT_NAME", "STATE", "COUNTY", "LAT", "LONG", "STATUS", "TOTAL_MW")
SolarSites <- EIA2017[Keep]
head(SolarSites)

sapply(SolarSites, typeof) #Make sure variables types are correct

#-------------------------------------------------
# LABELS
#-------------------------------------------------
labels <- sprintf(
  "<strong>%s<br />%s</strong><br/>%s MW",
  SolarSites$PLANT_NAME, SolarSites$UTILITY_NAME,SolarSites$TOTAL_MW)%>% lapply(htmltools::HTML)
#
#-------------------------------------------------
# Custom Sun Icon
#-------------------------------------------------
sunIcon <- makeIcon(iconUrl = "https://images.vexels.com/media/users/3/145133/isolated/preview/7677cf8c727c039ddc915af8ed3e8f2c--cone-de-feixes-de-rodelas-do-sol-by-vexels.png")
#
#-------------------------------------------------
# Raster Data for Agriculture
#-------------------------------------------------
library(raster)
library(sp)
#crs(r) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
r<-raster("P:/PROJECTS/Pollinators/GIS/USDA_CDL/2017/CDL_Midwest_2017_ag_240_wgs.tif")
plot(r)
pal.ag <- colorNumeric("green", values(r), na.color = "transparent")
#
#--------------
# Radius & Weight
#--------------
SolarSites$Radius <- as.integer(0)
SolarSites$Weight <- as.integer(0)
SolarSites$Radius <- ifelse(SolarSites$TOTAL_MW <= 1, 2, 
                            ifelse(SolarSites$TOTAL_MW > 1 & SolarSites$TOTAL_MW <= 10, 5, 10))
SolarSites$Weight <- ifelse(SolarSites$TOTAL_MW <= 1, 2, 
                            ifelse(SolarSites$TOTAL_MW > 1 & SolarSites$TOTAL_MW <= 10, 5, 10))                             
#-------------------------------------------------
# LEAFLET MAP  #1 Basic Map with Simple Markers
#-------------------------------------------------
eia.map <- leaflet(SolarSites)%>%
  setView(-99, 35, 4)%>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
  addMarkers(data=SolarSites, lng=~LONG, lat=~LAT)
eia.map

#-------------------------------------------------
# LEAFLET MAP  #2 (with Layer Controls & radius - NO AGRICULTURE)
#-------------------------------------------------
eia.map2 <- leaflet(SolarSites)%>%
  setView(-90, 40, 6)%>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo Map")%>%
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map")%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery")%>%
  #Add Markers and Labels
  addCircleMarkers(data=SolarSites, lng=~LONG, lat=~LAT, group="sites", color="#FF5733", fill=TRUE, radius = ~Radius,
             label = labels, labelOptions = labelOptions(noHide=F,"font-style" = "bold","font-size" = "16px"),
             weight = ~Weight, opacity=0.99)%>%
  #Add Layer Control
  addLayersControl(baseGroups=c("Topo Map", "Street Map", "World Imagery"),
                   options = layersControlOptions(collapsed=FALSE))
eia.map2

#-------------------------------------------------
# LEAFLET MAP  #3 (with Layer Controls and Agriculture)
#-------------------------------------------------
eia.map3 <- leaflet(SolarSites)%>%
  setView(-99, 35, 5)%>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo Map")%>%
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map")%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery")%>%
  #Add Markers and Labels
  addCircles(data=SolarSites, lng=~LONG, lat=~LAT, group="sites", color="#FF5733", fill=TRUE, radius = 4,
             label = labels, labelOptions = labelOptions(noHide=F,"font-style" = "bold","font-size" = "16px"),
             weight = 10, opacity=0.99)%>%
  #Add Ag Raster for Minnesota
  addRasterImage(r, colors=pal.ag, opacity=0.6, group = "Agriculture")%>%
  #Add Layer Control
  addLayersControl(baseGroups=c("Topo Map", "Street Map", "World Imagery"),
                   overlayGroup = "Agriculture </p> (midwest)",
                   options = layersControlOptions(collapsed=FALSE))
eia.map3


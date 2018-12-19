#-------------------------------
# Mapping EIA Solar Facilities
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
library(leaflet.extras)
library(htmltools)
library(mapview)
library(magrittr)
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
Keep <- c("UTILITY_NAME","STATE", "COUNTY", "LAT", "LONG", "STATUS", "TOTAL_MW")
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
#--------------
# Radius & Weight
#--------------
SolarSites$Radius <- as.integer(0)
SolarSites$Weight <- as.integer(0)
SolarSites$Radius <- ifelse(SolarSites$TOTAL_MW <= 1, 2, 
                            ifelse(SolarSites$TOTAL_MW > 1 & SolarSites$TOTAL_MW <= 10, 5, 10))
SolarSites$Weight <- ifelse(SolarSites$TOTAL_MW <= 1, 2, 
                            ifelse(SolarSites$TOTAL_MW > 1 & SolarSites$TOTAL_MW <= 10, 5, 10)) 

print(SolarSites)

Logo <- "https://raw.githubusercontent.com/mapguyusa/MyStuff/master/TMG.png"


#-------------------------------------------------
# LEAFLET MAP  #1 Basic Map with Simple Markers
#-------------------------------------------------
eia.map <- leaflet(SolarSites)%>%
  setView(-99, 35, 4)%>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
  addMarkers(data=SolarSites, lng=~LONG, lat=~LAT, icon = sunIcon)
eia.map
#
#------------------
# Leaflet #2 - HeatMap
#------------------
#
Solar.Heatmap <- leaflet(SolarSites)%>%
  setView(-99, 35, 4)%>%
  addProviderTiles(providers$Esri.WorldStreetMap)%>%
  addLogo(Logo, url="http://www.mapguyusa.com/p/portfolio.html", position = "topright", width=100, height=100)%>%
  addHeatmap(lng=~LONG, lat=~LAT, blur=8, intensity = ~TOTAL_MW, max=0.25, radius=5)
Solar.Heatmap

"Set the WD and load the Libraries:"
setwd("C:/Users/miqui/OneDrive/R Projects/Chicago Crime")
library(tidyverse)
library(leaflet)
library(stringr)
library(sf)
library(here)
library(widgetframe)
library(dplyr)
library(ggplot2)

options(digits = 3)
set.seed(1234)
theme_set(theme_minimal())

"Import the Chicago Crime Dataset:"
#https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
library(readxl)
crimes <- read_excel("Chicago Crimes 2017.xlsx", 
                     sheet = "Crimes_-_2017", col_types = c("numeric", 
                                                            "text", "date", "text", "text", "text", 
                                                            "text", "text", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "text", "numeric", "numeric", 
                                                            "numeric", "date", "numeric", "numeric", 
                                                            "text"))
View(crimes)
attach(crimes)

"Select only the necessary columns for graphing:"

names(crimes)

crimes <- crimes %>%
            select(ID, Date, Block,)


areas <- st_read(dsn = "C:/Users/miqui/OneDrive/R Projects/Chicago Crime/Boundaries.geojson") %>%
         mutate(community = str_to_title(community))


plot(areas)

"Get just the homicides:"
homicides <- crimes %>%
    filter(`Primary Type` == "HOMICIDE")


homicides %>%
  mutate(popup = str_c(Date,
                       Block,
                       str_c("Location type:", `Location Description`,
                             sep = " "),
                       sep = "<br/>")) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup = ~popup) %>%
  frameWidget()

areas_homicides <- areas %>%
    select(community, area_numbe) %>%
    mutate(area_numbe = as.numeric(as.character(area_numbe))) %>%
    left_join(homicides %>%
                count(`Community Area`),
              by = c("area_numbe" = "Community Area")) %>%
    mutate(n = ifelse(is.na(n), 0, n))






"Full Map:"

bins <- c(0, 10, 20, 30, 40, 50, Inf)
pal <- colorBin("YlOrRd", domain = areas_homicides$n, bins = bins)


areas_homicides %>%
  mutate(popup = str_c("<strong>", community, "</strong>",
                       "<br/>",
                       "Reported homicides in 2017: ", n) %>%
           map(htmltools::HTML)) %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(label = ~popup,
              fillColor = ~pal(n),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  frameWidget()



























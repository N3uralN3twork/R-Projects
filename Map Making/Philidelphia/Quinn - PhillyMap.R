setwd("C:/Users/miqui/OneDrive/R Projects/Map Making/Philidelphia")

"
Source: https://cengel.github.io/R-spatial/mapping.html
Shape files: https://www.census.gov/cgi-bin/geo/shapefiles/index.php
"

# Load in the necessary libraries:
library(dplyr)
library(ggplot2)
library(tigris)
library(sf)
library(leaflet)
library(RColorBrewer)
library(classInt)

# Read in the shape files:
PhilCrimes <- st_read("Data/PhillyCrimerate")

# 1st plot of Homicide rate using built-in `plot` function:
plot(PhilCrimes["homic_rate"],
     breaks = "quantile")


# 2nd plot using ggplot2:
ggplot(PhilCrimes) +
  geom_sf(aes(fill = homic_rate))

# 3rd plot using Leaflet:
"Reproject the current sf data:"
philly_WGS84 <- st_transform(PhilCrimes, 4326)

pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)

p_popup <- paste0("<strong>Homicide Density: </strong>", philly_WGS84$homic_rate)

# quantile breaks
breaks_qt <- classIntervals(PhilCrimes$homic_rate, n = 7, style = "quantile")

leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup,
    group = "philly") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addLegend("bottomright", 
            colors = brewer.pal(7, "YlOrRd"), 
            labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
            title = 'Philadelphia homicide density per sqkm') %>%
  addLayersControl(baseGroups = c("OSM", "Carto"), 
                   overlayGroups = c("philly"))


"Using the Tigris package:"
USStates <- tigris::states(class = "sf")



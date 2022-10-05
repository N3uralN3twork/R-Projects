setwd("C:/Users/miqui/OneDrive/R Projects/Map Making/US Counties Maps")

########################################
### Load in the necessary libraries: ###
########################################
library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(RColorBrewer)
library(classInt)

########################################
### Import the US County datasets:   ###
"Data Source: https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/"
########################################
library(readxl)
USDA <- read_excel("USDA - Median Household Income Data.xlsx"
                  , sheet = "Data")
View(USDA)

"
Tigris will return the shape files in the sf format, also known as simple features.

The simple features data structure can be manipulated and viewed as a regular data frame.
"

"Using the Tigris library:"
USStates <- tigris::states(class = "sf", year = 2021)      # ~15 MB
USCounties <- tigris::counties(class = "sf", year = 2021)  # ~130 MB


########################################
### Merge the two datasets:          ###
########################################
dfMerged <- geo_join(spatial_data = USCounties
                     , data_frame = USDA
                     , by_sp = "GEOID", by_df = "FIPS_code")

########################################
### Filter and Clean the merged DF:  ###
########################################
"Selecting the appropriate state: Ohio"
dfOhio <- dfMerged %>%
          filter(State == "OH") %>%
          select(STATEFP, COUNTYFP, COUNTYNS, GEOID, NAME, NAMELSAD, LSAD, CLASSFP, MTFCC, CSAFP
                , CBSAFP, METDIVFP, FUNCSTAT, ALAND, AWATER, INTPTLAT, INTPTLON, State, Area_name
                , Civilian_labor_force_2020, Employed_2020, Unemployed_2020, Unemployment_rate_2020
                , Civilian_labor_force_2021, Employed_2021, Unemployed_2021, Unemployment_rate_2021
                , Median_Household_Income_2020, Med_HH_Income_Percent_of_State_Total_2020
                , rank, geometry)


########################################
### Re-project the current sf data:  ###
########################################
dfOhioTrans <- st_transform(dfOhio, crs = 4326)

pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)

myPopup <- paste0("<strong>Median HH Income ($):</strong>", dfOhioTrans$Median_Household_Income_2020)
dfOhioTrans <- dfOhioTrans %>% 
                mutate(myPopup = str_c("<strong>County: </strong>", NAME,
                                     "<br/>",
                                     "<strong>State: </strong>", State,
                                     "<br/>",
                                     "<strong>Median HH Income ($):</strong>", dfOhioTrans$Median_Household_Income_2020
                                    ))

# quantile breaks
quantileBreaks <- classIntervals(dfOhioTrans$Median_Household_Income_2020
                                , n = 7, style = "quantile")

########################################
### Making the actual maps:          ###
########################################
leaflet(dfOhioTrans) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(Median_Household_Income_2020),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = ~myPopup,
    group = "Ohio",
    label = ~NAME) %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addLegend("bottomright",
            colors = brewer.pal(7, "YlOrRd"), 
            labels = paste0("$", format(quantileBreaks$brks[-1], digits = 2)),
            title = "Median Income ($)") %>%
  addLayersControl(baseGroups = c("OSM", "Carto"), 
                   overlayGroups = c("Ohio"))








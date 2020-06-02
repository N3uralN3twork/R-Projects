#### Set WD and Import the Datasets####
setwd("C:/Users/miqui/OneDrive/R Projects/ADAMS Board")
library(readxl)
library(dplyr)
library(lubridate)
library(leaflet)
library(ggmap)
library(stringr)
members <- read_excel("ADAMHSCCNeedsAssessmentData20200522.xlsx", 
                      sheet = "Members")
View(members)

PayerSpans <- read_excel("ADAMHSCCNeedsAssessmentData20200522.xlsx", 
                         sheet = "PayerSpans", col_types = c("numeric", 
                                                             "date", "date", "text", "text", "text", 
                                                             "numeric", "text"))
View(PayerSpans)

Services <- read_excel("ADAMHSCCNeedsAssessmentData20200522.xlsx", 
                       sheet = "Services", col_types = c("numeric", 
                                                         "text", "numeric", "text", "text", 
                                                         "text", "text", "text", "text", "numeric", 
                                                         "text", "numeric", "text", "date", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "text", "text", 
                                                         "text", "text", "text"))
View(Services)


#### Create a new table ####

# Only for 2019

# Variables needed:
" 1. Individual ID
  2. AmtPaidM
  3. NumServM
  4. AmtPaidB
  5. NumServB
"

"1. Filter datasets for just 2019 data:"

names(members)
names(PayerSpans)
names(Services)

# Turn into a date-time variable for easier year extraction
Services$ServiceDate <- as.Date(Services$ServiceDate)

# Extract the year from the service date

Services$Year <- year(Services$ServiceDate)

# Select only the data with Year == 2019

Services <- Services %>% filter(Year == 2019)


"2. Select columns we want in our new dataset:"

# Select the following columns:
  "A. IndividualID
   B. Payment
   C. PayerOfService
  "

Services <- Services %>% select(IndividualId, Payment, PayerOfService)

# Create the following variables: 
  "
  1. AmtPaidM
  2. NumServM
  3. AmtPaidB
  4. NumServB
  "

ServicesSummary <- Services %>%
  select(IndividualId, Payment, PayerOfService) %>%
  group_by(IndividualId) %>%
  summarise(AmtPaidM = sum(Payment[PayerOfService=="MEDICAID"]),
            NumServM = table(PayerOfService)[2],
            AmtPaidB = sum(Payment[PayerOfService=="BOARD"]),
            NumServB = table(PayerOfService)[1]) %>%
  mutate(Category = 
           case_when(AmtPaidM >= 1 & AmtPaidB >=1 ~ "Both",
                     AmtPaidM == 0 & AmtPaidB >=1 ~ "Board Only",
                     AmtPaidM >= 1 & AmtPaidB ==0 ~ "Medicaid Only",
                     AmtPaidM == 0 & AmtPaidB ==0 ~ "Neither"))




table(ServicesSummary$Category)

ServicesSummary %>%
  group_by(Category) %>%
  summarise(Medicaid = mean(AmtPaidM),
            Board = mean(AmtPaidB))

#write.xlsx(ServicesSummary, sheetName = "ADAMHS", file = "ADAMHS.xlsx", col.names = TRUE, row.names = FALSE)


"Create a Map of the Data:"
table(is.na(Address))
members <- members %>%
  filter(!is.na(Address))

# Select only columns we want:

members <- members %>%
  select(IndividualId, Address, ZipCode, Gender, Race, Ethnicity, MaritalStatus, Birthdate)

attach(members)


" Create a new variable with the full address:"
# You'll have to use the paste function

members <- members %>%
  mutate(FullAddress = paste(Address, ",", "OHIO", ",", ZipCode))


"GEOCODE the full address:"

register_google(key = "AIzaSyDF-reX5JzcsAF-yXjZlBpaeIxkgoubdH8")
#data <- geocode(location = members$FullAddress, output = "latlona", source = "google")


data <- cbind(test, data)

"Map the new data:"

data <- data %>% 
  mutate(popup = str_c("<strong>", Address, "</strong>",
                       "<br/>",
                       "ID: ", IndividualId,
                       "<br/>",
                       "Gender: ", Gender,
                       "<br/>",
                       "Race: ", Race,
                       "<br/>",
                       "Marital Status: ", MaritalStatus,
                       "<br/>",
                       "Birthdate: ", Birthdate))

leaflet(data = data) %>%
  addMarkers(popup = ~popup) %>%
  addProviderTiles("Wikimedia")






"Merge Demographics with the Services Summary:"


















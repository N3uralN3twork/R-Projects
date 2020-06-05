#### Set WD and Import the Datasets####
setwd("C:/Users/miqui/OneDrive/R Projects/ADAMS Board")
library(readxl)
library(readr)
library(xlsx)
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
  5. NumSUD
  6. NumMH
  "
ServicesSummary <- Services %>%
  select(IndividualId, Payment, PayerOfService, ProviderType) %>%
  group_by(IndividualId) %>%
  summarise(AmtPaidM = sum(Payment[PayerOfService=="MEDICAID"]),
            NumServM = sum(PayerOfService=="MEDICAID"),
            AmtPaidB = sum(Payment[PayerOfService=="BOARD"]),
            NumServB = sum(PayerOfService=="BOARD"),
            NumSUD = sum(ProviderType=="SUD"),
            NumMH = sum(ProviderType=="MH")) %>%
  mutate(Category = 
           case_when(NumServM > 0 & NumServB > 0 ~ "Both",
                     NumServM == 0 & NumServB >= 1 ~ "Board Only",
                     NumServM >= 1 & NumServB == 0 ~ "Medicaid Only",
                     AmtPaidM == 0 & AmtPaidB == 0 ~ "Neither")) %>%
  mutate(ProviderCategory = 
           case_when(NumSUD > 0 & NumMH > 0 ~ "Both",
                     NumSUD > 0 & NumMH == 0 ~ "SUD Only",
                     NumSUD == 0 & NumMH > 0 ~ "MH Only",
                     is.na(NumSUD) & is.na(NumMH) ~ "Neither"))

# Create a table of the categories:
table(ServicesSummary$Category)
table(ServicesSummary$ProviderCategory)

# Write to an Excel file:
ServicesSummary <- as.data.frame(ServicesSummary)
write.xlsx(ServicesSummary, sheetName = "ADAMHS", file = "ServicesSummary.xlsx", col.names = TRUE, row.names = FALSE)


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


names(members)
names(ServicesSummary)

merged <- merge(members, ServicesSummary, by="IndividualId")
#write.csv(merged, "MergedADAMS.csv", col.names = TRUE, row.names = FALSE)


"Cluster Analysis:"

# Read in the newly merged dataset
library(readr)
adams <- read_csv("MergedADAMS.csv", col_types = cols(Birthdate = col_date(format = "%m/%d/%Y")))
View(adams)
attach(adams)

# Replace NAs with 0

adams$NumServM[is.na(adams$NumServM)] <- 0
adams$ZipCode[is.na(adams$ZipCode)] <- 0

# Create an AGE variable, base date is May 1st 2020:
names(adams)
base_date <- as.Date("2020-05-01")
adams <- adams %>%
            mutate(Age = (base_date - Birthdate)/365)
adams$Age <- as.numeric(adams$Age)

# Create a TOTAL PAID variable:
adams <- adams %>%
            mutate(TotalPaid = AmtPaidB + AmtPaidM)

# Create a TOTAL NUM variable:
adams <- adams %>%
            mutate(TotalNum = NumServB + NumServM)
adams$ZipCode <- as.numeric(adams$ZipCode)
"Preprocessing:" 

# Select only the numeric columns
Numeric <- adams %>% 
              select_if(is.numeric) %>%
              select(-MasterIndividualId)

Category <- adams$Category
Numeric <- cbind(Numeric, Category)
write.csv(Numeric, "Numeric.csv", col.names = TRUE, row.names = FALSE)

"Perform Cluster Analysis:"
library(cluster)
library(factoextra)


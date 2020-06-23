"Sources: https://www.cpc.unc.edu/projects/addhealth/faqs/aboutdata/index.html#what-is-the-best"

"Read in the data and necessary libraries:"
setwd("C:/Users/miqui/OneDrive/R Projects/ADD Health")
library(dplyr)
library(lubridate)
library(readr)
wave1 <- read_delim("Wave1.tsv", "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
View(wave1)


library(readr)
wave2 <- read_delim("Wave2.tsv", "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
View(wave2)

library(readr)
wave3 <- read_delim("Wave3.tsv", "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
View(wave3)

"Turn into data-frames:"

wave1 <- as.data.frame(wave1)
wave2 <- as.data.frame(wave2)
wave3 <- as.data.frame(wave3)

table(unique(wave1$AID))
table(is.na(wave1$AID))

"Merge waves by AID:"

waves <- merge(wave1, wave2, by="AID")
waves <- merge(waves, wave3, by="AID")
variables <- as.data.frame(names(waves))

"Calculating the Age:"

waves <- waves %>% 
  mutate(IDate = mdy(paste(IMONTH, IDAY, IYEAR)))

waves <- waves %>%
  mutate(BDate = mdy(paste(H1GI1M, 15, H1GI1Y)))

waves <- waves %>%
  mutate(Age = as.numeric((IDate - BDate)/365.25))

summary(waves$Age)

"Calculating the Race:"
waves <- waves %>%
  mutate(Race = case_when(
    H1GI4 == 1 ~ "Hispanic",
    H1GI6B == 1 ~ "Black",
    H1GI6D == 1 ~ "Asian",
    H1GI6C == 1 ~ "NativeAmerican",
    H1GI6E == 1 ~ "Other",
    H1GI6A == 1 ~ "White"))

table(waves$Race)

table(PA12)
table(is.na(PA12))

"Parent's Highest Education:"
# 0 = missing/refused
# 1 = HS or less
# 2 = some college / trade school
# 3 = College or higher
# 4 = Other

waves <- waves %>%
  mutate(PEducation = case_when(
    is.na(PA12) ~ 0,
    PA12 == 96 ~ 0,
    PA12 %in% c(1,2,3, 4,5,10) ~ 1,
    PA12 %in% c(6,7,8) ~ 2,
    PA12 == 9 ~ 3))

table(waves$PEducation)



















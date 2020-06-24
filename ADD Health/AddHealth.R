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
wave3 <- read_delim("Wave3.tsv", "\t", 
                    escape_double = FALSE, trim_ws = TRUE)
View(wave3)

library(readr)
wave4 <- read_delim("Wave4.tsv", "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
View(wave4)

"Turn into data-frames:"

wave1 <- as.data.frame(wave1)
wave2 <- as.data.frame(wave2)
wave3 <- as.data.frame(wave3)
wave4 <- as.data.frame(wave4)

table(unique(wave1$AID))
table(is.na(wave1$AID))

"Merge waves by AID:"

waves <- merge(wave1, wave2, by="AID")
waves <- merge(waves, wave3, by="AID")
waves <- merge(waves, wave4, by="AID")

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

variables <- as.data.frame(names(waves))

"Select only the necessary variables:"
attach(waves)
Waves <- waves %>%
  select(AID, PA10, PA12, S1, S6B, S2, S4, IMONTH, IDAY, IYEAR,
         H1GI1Y, H1GI6B, H1GI11, H1GI14, H1GI21, H1IR12, H1FV3,
         H1GI4, H1JO11, H1DS5, H1DS2, H1TO53, H1DS12, H1DS13, H1DS9,
         H2GI10, H2IR12, H2FV1, H2DS10, H2DS11,
         H3DS8, H3OD4B, BIO_SEX3, H3HR24, H3HR25, H3ID32, H3ID30,
         H3ID29, H3CJ5, H3DS16, H3CJ108A, H3LM7,
         H4DS8, H4CJ9I, H4DS1, H4DS19, H4CJ25M, H4DS5, H4DS6, H4DS2,
         H4WP28, H4CJ20, H4ED2, H4CJ1, H4CJ17, H4CJ24M, H4LM11,
         PEducation, Race, Age)
attach(Waves)

"Descriptive Statistics:"
table(Waves$BIO_SEX3)
table(Race)
Waves %>%
  group_by(BIO_SEX3) %>%
  summarize(mean = mean(Age, na.rm = TRUE),
            sd = sd(Age, na.rm = TRUE),
            median = median(Age, nana.rm = TRUE),
            IQR = IQR(Age, na.rm = TRUE))
 






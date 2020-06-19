"Sources: https://www.cpc.unc.edu/projects/addhealth/faqs/aboutdata/index.html#what-is-the-best"

"Read in the data and necessary libraries:"
setwd("C:/Users/miqui/OneDrive/R Projects/ADD Health")
library(dplyr)
library(lubridate)
library(readr)
wave1 <- read_delim("C:/Users/miqui/Downloads/ICPSR_21600-V21/ICPSR_21600/DS0001/21600-0001-Data.tsv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)
View(wave1)

library(readr)
wave2 <- read_delim("C:/Users/miqui/Downloads/ICPSR_21600-Wave2/ICPSR_21600/DS0005/21600-0005-Data.tsv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)
View(wave2)

" Turn into data-frames:"

wave1 <- as.data.frame(wave1)
wave2 <- as.data.frame(wave2)

table(unique(wave1$AID))
table(is.na(wave1$AID))

"Merge waves by AID:"

waves <- merge(wave1, wave2, by="AID")


waves <- waves %>%
          select(AID, IMONTH, IDAY, IYEAR, H1GI1M, H1GI1Y,
                 H1GI4, H1GI6A, H1GI6B, H1GI6C, H1GI6D, H1GI6E)

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

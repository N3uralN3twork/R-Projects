"Read in the data and necessary libraries:"
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
          select(AID, IMONTH, IDAY, IYEAR, H1GI1M, H1GI1Y)

"Calculating the Age:"

waves <- waves %>% 
  mutate(IDate = mdy(paste(IMONTH, IDAY, IYEAR)))

waves <- waves %>%
  mutate(BDate = mdy(paste(H1GI1M, 15, H1GI1Y)))

waves <- waves %>%
  mutate(Age = as.numeric((IDate - BDate)/365.25))

summary(waves$Age)


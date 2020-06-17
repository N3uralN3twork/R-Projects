"Read in the data and necessary libraries:"
library(dplyr)
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

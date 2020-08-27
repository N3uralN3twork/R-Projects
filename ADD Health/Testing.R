library(datasets)
data("SF12")
library(MLCIRTwithin)
library(readr)
data("SF12_nomiss")

write_csv(path = "SF12.csv", SF12, col_names = TRUE)

write_csv(path = "SF12_nomissing.csv", SF12_nomiss, col_names = TRUE)
setwd("C:/Users/miqui/OneDrive/R Projects/MinitabReadWrite Project")
library(rio)
library(foreign)
install_formats()


import("AutoSpecs.mwx") # Doesn't work

read.mtp("AutoSpecs.MTW")
read.mtp("AutoSpecs.mwx")

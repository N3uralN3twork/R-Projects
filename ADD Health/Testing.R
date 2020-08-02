library(randomizeR)
library(randomizr)
library(dplyr)

# Printing out the codes:
for (i in seq(1, 30)){
  if (i < 10){
    print(paste("AAA", i, sep = "0"))
  } else {
    print(paste("AAA", i, sep = ""))
  }
}

# Assigning random treatment/control:
TLC <- data.frame(TorC = sample(t(rep(c("T", "C"), 15))))
test$ID <- paste(matt$ID, TLC$TorC, sep = "")



library(lavaan)

MODEL <-
  "
  STEALING =~ JDStealLess + JDStealMore + JDStealStore
  
  DRUGS =~ JDDriveHigh + JDSellDrugs
  "
fit <- cfa(MODEL, data=Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)

# They all load onto the same stealing/drug factors quite well.


library(lavaan)

MODEL <-
  "
  STEALING =~ JDStealLess + JDStealMore + JDStealStore
  
  DRUGS =~ JDDriveDrunk + JDDriveHigh + JDSellDrugs + JDIllegalDrugUse + JDCocaineUse + 
           JDIllegalDrugNeedle + JDAloneDrugUse + JDFightOnDrugs + JDWeaponOnDrugs
  "
fit <- cfa(MODEL, data=Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)

# They all load onto the same stealing/drug factors quite well.



library(lavaan)

# Adult Non-aggressive Delinquency
ANonAD <- 
  "
  ANonAD =~ AStealProperty + ADamageProperty + AStealLess + AStealMore
  "
fit4 <- cfa(ANonAD, data=Waves, std.lv=TRUE)
summary(fit4, fit.measures=TRUE, standardized=TRUE)

AAD <-
  "
  AAD =~ AShootStab + APhysicalFight + AKnifeGun + ASellDrugs
  "
fit4 <- cfa(AAD, data=Waves, std.lv=TRUE)
summary(fit4, fit.measures=TRUE, standardized=TRUE)


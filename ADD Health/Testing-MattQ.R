library(lavaan)

# Adult Aggressive/Non-Aggressive Delinquency
AD <-
  "
  AAD =~ AShootStab + APhysicalFight + AKnifeGun + ASellDrugs
  
  ANonAD =~ AStealProperty + ADamageProperty + AStealLess + AStealMore
  "
fit4 <- cfa(AD, data=Waves, std.lv=TRUE)
summary(fit4, fit.measures=TRUE, standardized=TRUE)


test <- 
  "
  AIncarceration ~ JIncarceration
  "
fit <- cfa(test, data=Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)














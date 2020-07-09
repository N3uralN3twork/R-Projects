library(lavaan)
names(Waves)

MODEL <- 
  "
  # Latent Variables
  
  SCHOOL =~ APMath + APScience + APHistory + APEnglish
  
  #SCHOOLATTACH =~ AtSClose + AtSHappy + AtSPartOf + AtSFairly
  
  #PARENTATTACH =~ AtPCloseMother + AtPCloseFather + AtPMotherCare + AtPFatherCare
  
  #DELINQPEERS =~ AtDPCigs + AtDPAlcohol + AtDPWeed
  
  AGGDELINQ =~ ADPhysicalFight + ADKnifeGun + ADShootStab + ADWeaponSchool
  
  NADDELINQ =~ NADLying + NADShoplift + NADStealLess + NADStealMore
  
  #TRAUMAFAMILY =~ Death + Divorce
  
  #TRAUMAPOVERTY =~ Unemployment
  
  TRAUMACRIME =~ Victim
  
  # Regressions
  
  AIncarceration ~ SCHOOL + AGGDELINQ + NADDELINQ + TRAUMACRIME + 
                   ESuspend + MSuspend + HSuspend + Gender.Coded + Black + Citizenship + FamilySize + HighGrade15 + SES + Unemployment + JIncarceration

  JIncarceration ~ SCHOOL + AGGDELINQ + NADDELINQ + TRAUMACRIME + 
                   ESuspend + MSuspend + HSuspend + Gender.Coded + Black + Citizenship + FamilySize + HighGrade15 + SES + Unemployment
  "

fit <- cfa(MODEL, data = Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)
lavInspect(fit)
exp(coef(fit))

table(Waves$AtDPCigs)
table(Waves$AtDPAlcohol)
table(Waves$AtDPWeed)

NAD <- Waves %>%
  select(NADLying, NADShoplift, NADStealLess, NADStealMore)

alpha(AggDelinq)
alpha(NAD)

princomp(AggDelinq, cor=TRUE)

# Compute Z-scores for NAD:
waves <- waves %>%
  mutate(ZScoreLying = (NADLying - mean(NADLying, na.rm = TRUE))/sd(NADLying, na.rm = TRUE)) %>%
  mutate(ZScore)
summary(waves$ZScore)

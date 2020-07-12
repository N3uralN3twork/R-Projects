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
  
  TRAUMAPOVERTY =~ Unemployment + Poverty
  
  TRAUMACRIME =~ Victim  + Touched
  
  TRAUMAOTHER =~ BasicNeeds + HomeAlone + HurtFeelings
  
  # Regressions
  
  AIncarceration ~ SCHOOL + AGGDELINQ + NADDELINQ + TRAUMACRIME  +
                   ESuspend + MSuspend + HSuspend + Gender.Coded + Black + Citizenship + FamilySize + HighGrade15 + SES + Unemployment + JIncarceration

  JIncarceration ~ SCHOOL + AGGDELINQ + NADDELINQ + TRAUMACRIME  +
                   ESuspend + MSuspend + HSuspend + Gender.Coded + Black + Citizenship + FamilySize + HighGrade15 + SES + Unemployment
  "

fit <- cfa(MODEL, data = Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)
exp(coef(fit))


# Cronbach's Alpha
NAD <- Waves %>%
  select(NADLying, NADShoplift, NADStealLess, NADStealMore)
alpha(NAD)







##################################
### Logistic Regression Models ###
##################################

"Being Black:"

AIBlack <- glm(
  AIncarceration ~ Black,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIBlack)
exp(coef(AIBlack))
"The odds of being incarcerated as an adult are 1.44 times
higher if you are black than if you are not."


"Full Model:"

AIFull <- glm(
  AIncarceration ~ ESuspend + MSuspend + HSuspend + Gender.Coded + Black + Citizenship + FamilySize + HighGrade15 + SES + Unemployment + JIncarceration,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIFull)





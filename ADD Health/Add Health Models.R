library(lavaan)
names(Waves)

# Rename Poverty to Can't pay bills
# Find Bullying
# Code Asian/Other
# Adult Crime = Aggressive and Non-aggressive Crimes


MODEL <- 
  "
  ### Latent Variables ###
  
  AGGDELINQ =~ ADPhysicalFight + ADKnifeGun + ADShootStab + ADWeaponSchool
  
  NONAGGDELINQ =~ NADLying + NADShoplift + NADStealLess + NADStealMore
  
  #TRAUMAFAMILY =~ Divorce
  
  TRAUMAPOVERTY =~ CantPayBills + Homeless
  
  #TRAUMACRIME =~ Victim + BioFatherJail + BioMotherJail + FigFatherJail + FigMotherJail
  
  TRAUMAMALTREATMENT =~ Touched + SlapHitKick + BasicNeeds
  
  ### Correlations ###
  
  AGGDELINQ ~~ NONAGGDELINQ
  
  TRAUMAPOVERTY ~~ TRAUMAMALTREATMENT
  
  ### Regressions ###
  
  AIncarceration ~ AGGDELINQ + NONAGGDELINQ + TRAUMAMALTREATMENT + TRAUMAPOVERTY + Victim + Divorce +
                   Gender.Coded + Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade + JIncarceration

  JIncarceration ~ AGGDELINQ + NONAGGDELINQ + TRAUMAMALTREATMENT + TRAUMAPOVERTY + Victim + Divorce +
                   Gender.Coded + Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade
                   
  AGGDELINQ ~ Gender.Coded + Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade
  
  NONAGGDELINQ ~ Gender.Coded + Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade
  "

fit <- cfa(MODEL, data=Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)
exp(coef(fit))


GENDER <- 
  "
  ### Latent Variables ###

  SCHOOL =~ APMath + APScience + APHistory + APEnglish
  
  #SCHOOLATTACH =~ AtSClose + AtSHappy + AtSPartOf + AtSFairly
  
  #PARENTATTACH =~ AtPCloseMother + AtPCloseFather + AtPMotherCare + AtPFatherCare
  
  DELINQPEERS =~ AtDPCigs + AtDPAlcohol + AtDPWeed
  
  AGGDELINQ =~ ADPhysicalFight + ADKnifeGun + ADShootStab + ADWeaponSchool
  
  NADDELINQ =~ NADLying + NADShoplift + NADStealLess + NADStealMore
  
  #TRAUMAFAMILY =~ Death + Divorce
  
  TRAUMAPOVERTY =~ Unemployment + Poverty + Homeless
  
  TRAUMACRIME =~ Victim  + Touched
  
  TRAUMAOTHER =~ BasicNeeds + HomeAlone + HurtFeelings
  
  ### Regressions ###
  
  AIncarceration ~ SCHOOL + AGGDELINQ + NADDELINQ + TRAUMACRIME  +
                   ESuspend + MSuspend + HSuspend  + Black + Citizenship + FamilySize + HighGrade15 + SES + Unemployment + JIncarceration

  JIncarceration ~ SCHOOL + AGGDELINQ + NADDELINQ + TRAUMACRIME  +
                   ESuspend + MSuspend + HSuspend  + Black + Citizenship + FamilySize + HighGrade15 + SES + Unemployment
  "

fit2 <- cfa(MODEL, data=waves, std.lv=TRUE, group="Gender")
summary(fit2, fit.measures=TRUE, standardized=TRUE)





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
  AIncarceration ~  as.factor(JIncarceration),
  data = waves,
  family = binomial(link = "logit"))

summary(AIFull)

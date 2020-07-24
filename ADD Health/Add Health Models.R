library(lavaan)
names(Waves)


FULL <- 
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

fit <- cfa(FULL, data=Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)
exp(coef(fit))


### By Gender ###
GENDER <- 
  "
  ### Latent Variables ###
  
  NONAGGDELINQ =~ NADLying + NADShoplift + NADStealLess + NADStealMore
  
  TRAUMAFAMILY =~ Divorce
  
  TRAUMAMALTREATMENT =~ Touched + SlapHitKick + BasicNeeds
  
  ### Regressions ###
  
  AIncarceration ~ NONAGGDELINQ + TRAUMAMALTREATMENT + 
                   Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade + JIncarceration

  JIncarceration ~ NONAGGDELINQ + TRAUMAMALTREATMENT + 
                   Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade
  "

fit2 <- cfa(GENDER, data=Waves, std.lv=TRUE, group="Gender")
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

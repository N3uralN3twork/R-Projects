library(lavaan)
names(Waves)

# CFI/TLI > 0.9 = Good
# RMSEA < 0.05 = Good
# Look @ the std.all column

FULL <- 
  "
  ### Latent Variables ###
  
  AGGDELINQ =~ ADPhysicalFight + ADKnifeGun + ADShootStab + ADWeaponSchool
  
  NONAGGDELINQ =~ NADLying + NADShoplift + NADStealLess + NADStealMore
  
  TRAUMAFAMILY =~ Death + Divorce
  
  TRAUMAPOVERTY =~ CantPayBills + Homeless
  
  #TRAUMACRIME =~ Victim + BioFatherJail + BioMotherJail + FigFatherJail + FigMotherJail
  
  TRAUMAMALTREATMENT =~ Touched + SlapHitKick + BasicNeeds
  
  #HIGHSCHOOL =~ HSuspend + HighDropout
  
  ### Correlations ###
  
  AGGDELINQ ~~ NONAGGDELINQ
  
  TRAUMAPOVERTY ~~ TRAUMAMALTREATMENT
  
  #TRAUMACRIME ~~ TRAUMAPOVERTY
  
  ### Regressions ###
  
  AIncarceration ~ AGGDELINQ + NONAGGDELINQ + TRAUMAMALTREATMENT + TRAUMAPOVERTY + TRAUMAFAMILY + 
                   Gender.Coded + Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade + JIncarceration

  JIncarceration ~ AGGDELINQ + NONAGGDELINQ + TRAUMAMALTREATMENT + TRAUMAPOVERTY + TRAUMAFAMILY + 
                   Gender.Coded + Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade
                   
  AGGDELINQ ~ Gender.Coded + Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade
  
  NONAGGDELINQ ~ Gender.Coded + Black + Hispanic + Asian + Citizenship + Age + SES + TwoParentHome + HighestGrade
  "

fit <- cfa(FULL, data=Waves, std.lv=TRUE, sampling.weights = )
summary(fit, fit.measures=TRUE, standardized=TRUE)
exp(coef(fit))

### Delinquency and Crime ###
JDCrime <- 
  "
  ### Latent Variables ###
  
  ALCOHOL =~ JDDriveDrunk
  
  DRUGS =~ JDDriveHigh + JDSellDrugs + JDIllegalDrugUse + JDCocaineUse + JDIllegalDrugNeedle +
           JDAloneDrugUse + JDFightOnDrugs + JDWeaponOnDrugs
           
  DELINQUENCY =~ JDDamageProperty + JDStealLess + JDStealMore + JDHurtBadly + JDSellDrugs
  
  CRIME =~ AdultGun + AdultDestroyProperty + AdultStealLess + AdultStealMore + AdultOtherProperty + 
           AdultAttack + AdultSellDrugs + AdultPhysicalAttack + AdultStolenCard
  "

fit <- cfa(JDCrime, data=Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)




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

model <- glm(
  formula = AIncarceration ~ JIncarceration + Age + Victim + Divorce + Gender + Black + Hispanic + Asian + HighestGrade,
  data = Waves,
  family = binomial(link = "logit"))

model
summary(model)

# The reason for the difference between R and SAS is that R thinks that the 
# binary variables are all double-precision variables.

# Evidence:
typeof(Waves$JIncarceration)






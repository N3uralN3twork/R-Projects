setwd("W:\00_FIRE\Analytic_Data")
# SEM and CFA library
library(lavaan)
library(lavaanPlot)

# Import the SAS dataset
library(haven)
PAPER <- read_sas("Paper8_wave4.sas7bdat", NULL)
PAPER2 <- PAPER[complete.cases(PAPER$GSWGT4_2),]  # 
View(PAPER2)

##################ROUND1: just like dissertation but incarceration for dependent variable##############################
# Create SEM Model
ModelPAPER<- 
  "
#Latent Factors

NHOOD =~ Z_WHITE + Z_MED_HH_IN + Z_LESS_POVERTY + Z_COLL + Z_LESS_UNEMP + Z_OWNER
Attachment =~ LookOut + FeelSafe + KnowMost + StopNTalk + HappyLiving + UnhappyMove
PA_N =~ PA_LESSDrug + PA_SafeDrug + PA_Obligation + PA_Move

#Regressions

NHOOD ~  HISPANIC + NH_Black + other2 + female1 + pa_baplus
PA_N ~ HISPANIC + NH_Black + other2 + pa_baplus
Attachment ~ NHOOD + PA_N + HISPANIC + NH_Black + other2 + female1 + pa_baplus
JIncarceration ~ Attachment + NHOOD + PA_N + HISPANIC + NH_Black + other2 + female1 + pa_baplus

#correlation
NHOOD ~~ PA_N

"
fitbinary <- cfa(ModelPAPER, data=PAPER2, std.lv=TRUE)
summary(fitbinary, fit.measures=TRUE, standardized=TRUE)
exp(coef(fitbinary))
lavaanPlot(model = fitbinary)

fitweighted <- cfa(ModelPAPER, data=PAPER2, std.lv=TRUE, sampling.weights="GSWGT4_2", estimator="ML")
summary(fitweighted, fit.measures=TRUE, standardized=TRUE)



##################ROUND2 + added school+trauma##############################
# Create SEM Model
ModelPAPER<- 
  "
#Latent Factors

NHOOD =~ Z_WHITE + Z_MED_HH_IN + Z_LESS_POVERTY + Z_COLL + Z_LESS_UNEMP + Z_OWNER
Attachment =~ LookOut + FeelSafe + KnowMost + StopNTalk + HappyLiving + UnhappyMove
PA_N =~ PA_LESSDrug + PA_SafeDrug + PA_Obligation + PA_Move
TRAUMA =~ Ace_Emotional_Neglect + ACE_Physical_Abuse + ACE_Emotional_Abuse + ACE_Sexual_Abuse
       + Ace_Suicide + ACE_Foster_Home + Ace_Direct_Witness + Ace_Victim +Ace_Parental_Jail

#Regressions

NHOOD ~  HISPANIC + NH_Black + other2 + female1 + pa_baplus
PA_N ~ HISPANIC + NH_Black + other2 + pa_baplus
Attachment ~ NHOOD + PA_N + HISPANIC + NH_Black + other2 + female1 + pa_baplus
EverIncarceration ~ Attachment + NHOOD + PA_N + TRAUMA + HISPANIC + NH_Black + other2 + female1 + pa_baplus + Dropout12 + Suspend12 + EverExpel

#correlation
NHOOD ~~ PA_N

"
fitbinary <- cfa(ModelPAPER, data=PAPER2, std.lv=TRUE)
summary(fitbinary, fit.measures=TRUE, standardized=TRUE)
exp(coef(fitbinary))
lavaanPlot(model = fitbinary)

fitweighted <- cfa(ModelPAPER, data=PAPER2, std.lv=TRUE, sampling.weights="GSW5", estimator="ML")
summary(fitweighted, fit.measures=TRUE, standardized=TRUE)




##################ROUND3 + added school+trauma + AIncarceration ##############################




# Create SEM Model
ModelPAPER<- 
  "
#Latent Factors

NHOOD =~ Z_WHITE + Z_MED_HH_IN + Z_LESS_POVERTY + Z_COLL + Z_LESS_UNEMP + Z_OWNER
Attachment =~ LookOut + FeelSafe + KnowMost + StopNTalk + HappyLiving + UnhappyMove
PA_N =~ PA_LESSDrug + PA_SafeDrug + PA_Obligation + PA_Move
TRAUMA =~ Ace_Emotional_Neglect + ACE_Physical_Abuse + ACE_Emotional_Abuse + ACE_Sexual_Abuse
       + Ace_Suicide + ACE_Foster_Home + Ace_Direct_Witness + Ace_Victim +Ace_Parental_Jail

#Regressions

NHOOD ~  HISPANIC + NH_Black + other2 + female1 + pa_baplus
PA_N ~ HISPANIC + NH_Black + other2 + pa_baplus
Attachment ~ NHOOD + PA_N + HISPANIC + NH_Black + other2 + female1 + pa_baplus
AIncarceration ~ Attachment + NHOOD + PA_N + TRAUMA + HISPANIC + NH_Black + other2 + female1 + pa_baplus + Dropout12 + Suspend12 + EverExpel

#correlation
NHOOD ~~ PA_N

"
fitbinary <- cfa(ModelPAPER, data=PAPER2, std.lv=TRUE)
summary(fitbinary, fit.measures=TRUE, standardized=TRUE)
exp(coef(fitbinary))
lavaanPlot(model = fitbinary)

fitweighted <- cfa(ModelPAPER, data=PAPER2, std.lv=TRUE, sampling.weights="GSWGT4_2", estimator="ML")
summary(fitweighted, fit.measures=TRUE, standardized=TRUE)




setwd("C:/Users/miqui/OneDrive/R Projects/ADD Health")
library(dplyr)
library(lubridate)
library(rcompanion)
library(readr)
wave1 <- read_delim("Wave1.tsv", "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
View(wave1)

# 128 respondents are not currently in school in Wave 1:
table(wave1$H1GI18)
# TRUE

data1 <- wave1 %>%
  filter(H1GI18 == 1)

vars <- data.frame(names(data1))

# Gender:
# 1 = Male
# 2 = Female

"Aggressive Delinquency (AD):"

# 0 = Never
# 1 = Once
# 2 = More than once

# 1. Getting into a serious physical fight
# 2. Pulling a knife/gun on someone
# 3. Shooting or stabbing someone

table(data1$H1DS5) # Serious physical fight
table(data1$H1FV7) # Pulling a knife/gun on someone
table(data1$H1FV8) # Shooting or stabbing someone

# 0 = Zero days
# 1 = One day
# 2 = Two or Three days
# 3 = Four or Five days
# 4 = Six or more days

table(data1$H1FV9) # Carrying a weapon to school

"Non-aggressive Delinquency (NAD):"

# 0 = Never
# 1 = 1 or 2 times
# 2 = 3 or 4 times
# 3 = 5 or more times

# 1. Lying about whereabouts
# 2. Shoplifting
# 3. Stealing less than $50
# 4. Stealing more than $50

table(data1$H1DS3) # Lying about whereabouts
table(data1$H1DS4) # Shoplifting
table(data1$H1DS13) # Stealing less than $50
table(data1$H1DS9) # Stealing more than $50

"Fixing the variables to remove Refused/Don't Know/Not Applicable"

data1 <- data1 %>%
  mutate(ADPhysicalFight = replace(H1DS5, H1DS5 %in% c(6,8,9), NA)) %>%
  mutate(ADKnifeGun = replace(H1FV7, H1FV7 %in% c(6,8,9), NA)) %>%
  mutate(ADShootStab = replace(H1FV8, H1FV8 %in% c(6,8,9), NA)) %>%
  mutate(ADWeaponSchool = replace(H1FV9, H1FV9 %in% c(6,8,9), NA)) %>%
  mutate(NADLying = replace(H1DS5, H1DS5 %in% c(6,8,9), NA)) %>%
  mutate(NADShoplift = replace(H1DS4, H1DS4 %in% c(6,8,9), NA)) %>%
  mutate(NADStealLess = replace(H1DS13, H1DS13 %in% c(6,8,9), NA)) %>%
  mutate(NADStealMore = replace(H1DS9, H1DS9 %in% c(6,8,9), NA))

# Aggressive Delinquency
table(data1$ADPhysicalFight)
table(data1$ADKnifeGun)
table(data1$ADShootStab)
table(data1$ADWeaponSchool)

# Non-aggressive Delinquency
table(data1$NADLying)
table(data1$NADShoplift)
table(data1$NADStealLess)
table(data1$NADStealMore)

"Creating Generalized Delinquency Variables:"


# Just sum them up for a general delinquency score?
# Max possible aggressive delinquency is 16
# Max possible non-aggressive delinquency score is 12

data1 <- data1 %>%
  mutate(AggDelinq = ADPhysicalFight + ADKnifeGun + ADShootStab + ADWeaponSchool) %>%
  mutate(NonAggDelinq = NADLying + NADShoplift + NADStealLess + NADStealMore)

table(data1$AggDelinq)
table(data1$NonAggDelinq)  

###############################
###  Attachment to Parents  ###
###############################
"Attachment to parents (AtP):"

# 1 = Not at all
# 5 = Very much

# 1. How close do you feel to your mother?
# 2. How close do you feel to your father?
# 3. How much do you think she cares about you?
# 4. How much do you think he cares about you?

table(data1$H1WP9) # Close to mother
table(data1$H1WP13) # Close to father
table(data1$H1WP10) # Mother cares about you
table(data1$H1WP14) # Father cares about you

"Fixing the variables to remove Refused/Don't Know/Not Applicable:"

data1 <- data1 %>%
  mutate(AtPCloseMother = replace(H1WP9, H1WP9 %in% c(6, 7, 8, 9), NA)) %>%
  mutate(AtPCloseFather = replace(H1WP13, H1WP13 %in% c(6, 7, 8, 9), NA)) %>%
  mutate(AtPMotherCare = replace(H1WP10, H1WP10 %in% c(6, 7, 8, 9), NA)) %>%
  mutate(AtPFatherCare = replace(H1WP14, H1WP14 %in% c(6, 7, 8, 9), NA))
  
  
table(data1$AtPCloseMother)  
table(data1$AtPCloseFather)
table(data1$AtPMotherCare)
table(data1$AtPFatherCare)

"Create the Attachment to Parents variable:"

data1 <- data1 %>%
  mutate(AttachParents = AtPCloseMother + AtPCloseFather + AtPMotherCare + AtPFatherCare)

table(data1$AttachParents)


###############################
###  Attachment to School   ###
###############################
"Attachment to School (AtS):"

# Note: You have to reverse the order of the responses (6-i)

# 1 = Not at all
# 5 = Very much

# 1. I feel close to people at school
# 2. I am happy to be at school
# 3. The teachers treat students fairly
# 4. I feel like I am part of the school

table(data1$H1ED19) # Close to people at school
table(data1$H1ED22) # Happy to be at school
table(data1$H1ED23) # Treat students fairly
table(data1$H1ED20) # Part of the school


"Fixing the variables to remove Refused/Don't Know/Not Applicable:"

data1 <- data1 %>%
  mutate(AtSClose = replace(H1ED19, H1ED19 %in% c(6, 7, 8), NA)) %>%
  mutate(AtSHappy = replace(H1ED22, H1ED22 %in% c(6, 7, 8), NA)) %>%
  mutate(AtSFairly = replace(H1ED23, H1ED23 %in% c(6, 7, 8), NA)) %>%
  mutate(AtSPartOf = replace(H1ED20, H1ED20 %in% c(6, 7, 8), NA))

# Reversing the Likert scale:

data1 <- data1 %>%
  mutate(AtSClose = 6-AtSClose) %>%
  mutate(AtSHappy = 6-AtSHappy) %>%
  mutate(AtSFairly = 6-AtSFairly) %>%
  mutate(AtSPartOf = 6-AtSPartOf)

# Attachment to School
table(data1$AtSClose)
table(data1$AtSHappy)
table(data1$AtSFairly)
table(data1$AtSPartOf)



##########################################
###  Attachment to Delinquent Peers   ###
##########################################

"Attachment to Delinquent Peers (AtDP):"

# Asked to 3 best friends of the respondent

# 0 = Zero friends
# 3 = All 3 friends

# 6-9 = NA

# 1. Smoke at least 1 cigarette a day
# 2. Drank alcohol at least once per month
# 3. Used marijuana at least once per month

table(data1$H1TO9)  # Cigarette
table(data1$H1TO29) # Alcohol
table(data1$H1TO33) # Marijuana

data1 <- data1 %>%
  mutate(AtDPCigs = replace(H1TO9, H1TO9 %in% c(6, 8, 9), NA)) %>%
  mutate(AtDPAlcohol = replace(H1TO29, H1TO29 %in% c(6, 8, 9), NA)) %>%
  mutate(AtDPWeed = replace(H1TO33, H1TO33 %in% c(6, 8, 9), NA))

# Attachment to Delinquent Peers
table(data1$AtDPCigs)
table(data1$AtDPAlcohol)
table(data1$AtDPWeed)

"Creating the Attachment to Delinquent Peers variable:"

data1 <- data1 %>%
  mutate(AtDelinqPeers = AtDPCigs + AtDPAlcohol + AtDPWeed)

table(data1$AtDelinqPeers)



###############################
###  Academic Performance   ###
###############################

"Academic Performance (AP):"

# 1 = D
# 2 = C
# 3 = B
# 4 = A


# 1. Grade in English/Language Arts
# 2. Grade in Mathematics
# 3. Grade in History/Social Studies
# 4. Grade in Science

table(data1$H1ED11) # English
table(data1$H1ED12) # Math
table(data1$H1ED13) # History
table(data1$H1ED14) # Science


"Fixing the variables to remove Refused/Don't Know/Not Applicable/Other:"

data1 <- data1 %>%
  mutate(APEnglish = replace(H1ED11, H1ED11 %in% c(5, 6, 96, 97, 98, 99), NA)) %>%
  mutate(APMath = replace(H1ED12, H1ED12 %in% c(5, 6, 96, 97, 98, 99), NA)) %>%
  mutate(APHistory = replace(H1ED13, H1ED13 %in% c(5, 6, 96, 97, 98, 99), NA)) %>%
  mutate(APScience = replace(H1ED14, H1ED14 %in% c(5, 6, 96, 97, 98, 99), NA))

# Academic Performance:
table(data1$APEnglish)
table(data1$APMath)
table(data1$APHistory)
table(data1$APScience)

"Reversing the grading:"

data1 <- data1 %>%
  mutate(APEnglish = 5-APEnglish,
         APMath = 5-APMath,
         APHistory = 5-APHistory,
         APScience = 5-APScience)

# Academic Performance:
table(data1$APEnglish)
table(data1$APMath)
table(data1$APHistory)
table(data1$APScience)

data1 <- data1 %>%
  mutate(AcadPerform = APEnglish + APMath + APHistory + APScience)

"Select only the necessary variables for analyses:"
dataset <- data1 %>%
  select(AID, BIO_SEX, S1, H1DS5, H1FV7, H1FV8, 
         H1FV9, H1DS5, H1DS4, H1DS13, H1DS9,
         H1WP9, H1WP13, H1WP10, H1WP14, H1ED19,
         H1ED20, H1ED22, H1ED23, H1TO33, H1TO9,
         H1TO29, H1ED11, H1ED12, H1ED13, H1ED14,
         ADPhysicalFight, ADKnifeGun, ADShootStab,
         ADWeaponSchool, NADLying, NADShoplift, 
         NADStealLess, NADStealMore, AtPCloseMother,
         AtPCloseFather, AtPMotherCare, AtPFatherCare,
         AtSClose, AtSHappy, AtSPartOf, AtSFairly,
         AtDPCigs, AtDPAlcohol, AtDPWeed, APEnglish,
         APMath, APHistory, APScience, AggDelinq,
         NonAggDelinq, AttachParents, AtDelinqPeers,
         AcadPerform)

table(dataset$AggDelinq)
table(dataset$NonAggDelinq)
table(dataset$AttachParents)
table(dataset$AtDelinqPeers)

###########
### CFA ###
###########

"Both fa and cfa give very similar results in terms of metrics
I included both for reference"

"Confirmatory Factor Analysis:"

# Aggressive Delinquency
AD.model <- 
  "
  # Latent Variables
  AD =~ ADPhysicalFight + ADKnifeGun + ADShootStab + ADWeaponSchool
  "

fit <- cfa(AD.model, data = dataset, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)

# Non-aggressive Delinquency
NAD.model <- 
  "
  # Latent Variables
  NAD =~ NADLying + NADShoplift + NADStealLess + NADStealMore
  "

fit1 <- cfa(NAD.model, data = dataset, std.lv=TRUE)
summary(fit1, fit.measures=TRUE, standardized=TRUE)

# Attachment to Parents
AtParent.model <-
  "
  # Latent Variables
  AParent =~ AtPCloseMother + AtPCloseFather + AtPMotherCare + AtPFatherCare
  "

fit2 <- cfa(AtParent.model, data = dataset, std.lv=TRUE)
summary(fit1, fit.measures=TRUE, standardized=TRUE)


# Attachment to Delinquent Peers
AtDPeers.model <-
  "
  # Latent variables
  ADPeers =~ AtDPCigs + AtDPAlcohol + AtDPWeed
  "

fit3 <- cfa(AtDPeers.model, data = dataset, std.lv=TRUE)
summary(fit3, fit.measures=TRUE, standardized=TRUE)

# Or

DelinqPeers <- dataset %>%
  select(AtDPCigs, AtDPAlcohol, AtDPWeed)

solution <- fa(DelinqPeers, nfactors = 1)
solution

# Academic Performance
AP.model <- 
  "
  APerform =~ APEnglish + APMath + APHistory + APScience
  "

fit4 <- cfa(AP.model, data = dataset, std.lv=TRUE)
summary(fit4, fit.measures=TRUE, standardized=TRUE)

# Or 

Academic <- dataset %>%
  select(APEnglish, APMath, APHistory, APScience)

solution <- fa(Academic, nfactors = 1)
print(solution)





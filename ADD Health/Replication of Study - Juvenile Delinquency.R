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

"Select only the necessary variables for analyses:"
data1 <- data1 %>%
  select(AID, BIO_SEX, S1, H1DS5, H1FV7, H1FV8, 
         H1FV9, H1DS5, H1DS4, H1DS13, H1DS9,
         H1WP9, H1WP13, H1WP10, H1WP14, H1ED19,
         H1ED20, H1ED22, H1ED23)

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

# Asked of 3 best friends of the respondent

# 0 = Zero friends
# 3 = All 3 friends

# 6-9 = NA

# 1. Smoke at least 1 cigarette a day
# 2. Drank alcohol at least once per month
# 3. Used marijuana at least once per month

table(data1$H1TO9)  # Cigarette
table(data1$H1TO29) # Alcohol
table(data1$H1TO33) # Marijuana






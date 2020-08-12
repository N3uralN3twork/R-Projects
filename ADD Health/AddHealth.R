###############################
### Load Data and Libraries ###
###############################

"Read in the data and necessary libraries:"
setwd("C:/Users/miqui/OneDrive/R Projects/ADD Health")
library(dplyr)
library(lubridate)
library(rcompanion)
library(readr)
library(lavaan)
library(psych)
wave1 <- read_delim("Wave1.tsv", "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
View(wave1)


library(readr)
wave2 <- read_delim("Wave2.tsv", "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
View(wave2)

library(readr)
wave3 <- read_delim("Wave3.tsv", "\t", 
                    escape_double = FALSE, trim_ws = TRUE)
View(wave3)

library(readr)
wave4 <- read_delim("Wave4.tsv", "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
View(wave4)

"Turn into data-frames:"

wave1 <- as.data.frame(wave1)
wave2 <- as.data.frame(wave2)
wave3 <- as.data.frame(wave3)
wave4 <- as.data.frame(wave4)

table(unique(wave1$AID))
table(is.na(wave1$AID))

"Merge waves by AID:"

# Remove the ALL=TRUE option to only keep obs in all data sets

waves <- merge(wave1, wave2, by="AID")
waves <- merge(waves, wave3, by="AID")
waves <- merge(waves, wave4, by="AID")

"Calculating the Age:"

waves <- waves %>% 
  mutate(IDate = mdy(paste(IMONTH, IDAY, IYEAR)))

waves <- waves %>%
  mutate(BDate = mdy(paste(H1GI1M, 15, H1GI1Y)))

waves <- waves %>%
  mutate(Age = as.numeric((IDate - BDate)/365.25))

summary(waves$Age)

"Age @ First Incarceration"

waves <- waves %>%
  mutate(FirstIncarcAge = replace(H4CJ20, H4CJ20 %in% c(NA, 96, 97, 98), NA))
table(waves$FirstIncarcAge)
table(is.na(waves$FirstIncarcAge))

"Months Incarcerated as a Juvenile"

# 0 = 0 or legitimate skip
# NA = NA/96/98
# [1-11] = [1-11]

waves <- waves %>%
  mutate(JIncarceMonths = replace(H4CJ24Y, H4CJ24Y %in% c(NA, 96, 98), NA)) %>%
  mutate(JIncarceMonths2 = replace(H4CJ24M, H4CJ24M %in% c(NA, 96, 98), NA)) %>%
  mutate(JIncarceMonths = replace(JIncarceMonths, JIncarceMonths == 97, 0)) %>%
  mutate(JIncarceMonths2 = replace(JIncarceMonths2, JIncarceMonths2 == 97, 0)) %>%
  mutate(JIncarceMonths = (12*JIncarceMonths)) %>%
  mutate(JIncarceMonths = JIncarceMonths + JIncarceMonths2)


table(waves$H4CJ24Y)
table(waves$H4CJ24M)
table(waves$JIncarceMonths)
describe(waves$JIncarceMonths)


"Months Incarcerated as an Adult:"

# Replace Missing/Refused/Don't Knows with NA's
# Convert years in prison to months in prison
# Add months in prison to converted months to get total months in prison

waves <- waves %>%
  mutate(AIncarceMonths = replace(H4CJ25Y, H4CJ25Y %in% c(NA, 96, 98), NA)) %>%
  mutate(AIncarceMonths2 = replace(H4CJ25M, H4CJ25M %in% c(NA, 96, 98), NA)) %>%
  mutate(AIncarceMonths = replace(AIncarceMonths, AIncarceMonths == 97, 0)) %>%
  mutate(AIncarceMonths2 = replace(AIncarceMonths2, AIncarceMonths2 == 97, 0)) %>%
  mutate(AIncarceMonths = (12*AIncarceMonths)) %>%
  mutate(AIncarceMonths = AIncarceMonths + AIncarceMonths2)


table(waves$H4CJ25Y)
table(waves$H4CJ25M)
table(waves$AIncarceMonths)
summary(waves$AIncarceMonths)

"Calculating the Race:"

waves <- waves %>%
  mutate(Race = case_when(
    H1GI4 == 1 ~ "Hispanic",
    H1GI6B == 1 ~ "Black",
    H1GI6D == 1 ~ "Asian",
    H1GI6C == 1 ~ "Other",
    H1GI6E == 1 ~ "Other",
    H1GI6A == 1 ~ "White"))

waves$Race <- as.factor(waves$Race)
waves <- within(waves, Race <- relevel(Race, ref="White")) # Set "White" as the reference group
table(waves$Race)
table(is.na(waves$Race))

"Black:"

waves <- waves %>%
  mutate(Black = case_when(
    Race == "Black" ~ 1,
    TRUE ~ 0))

table(waves$Black)

"Asian:"

waves <- waves %>%
  mutate(Asian = case_when(
    Race == "Asian" ~ 1,
    TRUE ~ 0))

"Parent's Highest Education:"

# NA = missing/refused
# 1 = HS or less
# 2 = some college / trade school
# 3 = College or higher

waves <- waves %>%
  mutate(PEducation = case_when(
    is.na(PA12) ~ NaN,
    PA12 == 96 ~ NaN,
    PA12 %in% c(1,2,3, 4,5,10) ~ 1,
    PA12 %in% c(6,7) ~ 2,
    PA12 %in% c(8, 9) ~ 3))

table(waves$PEducation)
table(is.na(waves$PEducation))

"SES:"

table(waves$PA12)
table(waves$PB8)
table(is.na(waves$PA12))

waves <- waves %>%
  mutate(P1Education = case_when(
    is.na(PA12) ~ NaN,
    PA12 == 96 ~ NaN,
    PA12 %in% c(1,2,3,4,5,6,7,10) ~ 0,
    PA12 %in% c(8, 9) ~ 1)) %>%
  mutate(P2Education = case_when(
    is.na(PB8) ~ NaN,
    PB8 %in% c(96, 97, 11, 12) ~ NaN,
    PB8 %in% c(1,2,3,4,5,6,7,10) ~ 0,
    PB8 %in% c(8,9) ~ 1)) %>%
  mutate(SES = case_when(
    is.na(P1Education) & is.na(P2Education) ~ NaN,
    is.na(P1Education) & P2Education == 0 ~ 0,
    is.na(P1Education) & P2Education == 1 ~ 1,
    P1Education == 0 & is.na(P2Education) ~ 0,
    P1Education == 0 & P2Education == 0 ~ 0,
    P1Education == 0 & P2Education == 1 ~ 1,
    P1Education == 1 & is.na(P2Education) ~ 1,
    P1Education == 1 & P2Education == 0 ~ 1,
    P1Education == 1 & P2Education == 1 ~ 1))

table(waves$P1Education, waves$P2Education)
table(waves$SES)

"Two Parent Home"

# 0 = No
# 1 = Yes/Legitimate Skip
# NA =  NA/Refused

table(waves$S11)
table(waves$S17)

waves <- waves %>%
  mutate(Mother = case_when(
    is.na(S11) ~ NaN,
    S11 == 9 ~ NaN,
    S11 == 0 ~ 0,
    S11 == 1 ~ 1)) %>%
  mutate(Father = case_when(
    is.na(S17) ~ NaN,
    S17 == 9 ~ NaN,
    S17 == 0 ~ 0,
    S17 == 1 ~ 1)) %>%
  mutate(TwoParentHome = case_when(
    is.na(Mother) & is.na(Father) ~ NaN,
    is.na(Mother) & Father == 0 ~ NaN,
    is.na(Mother) & Father == 1 ~ NaN,
    Mother == 0 & is.na(Father) ~ NaN,
    Mother == 0 & Father == 0 ~ 0,
    Mother == 0 & Father == 1 ~ 0,
    Mother == 1 & is.na(Father) ~ NaN,
    Mother == 1 & Father == 0 ~ 0,
    Mother == 1 & Father == 1 ~ 1))

table(waves$Mother, waves$Father)
table(is.na(waves$Mother), is.na(waves$Father))
table(waves$TwoParentHome)

"Divorce:"
# 0 = Single/Married/Widowed
# 1 = Divorced/Separated
# NA = NA/Refused

table(waves$PA10)

waves <- waves %>%
  mutate(Divorce = case_when(
    is.na(PA10) ~ NaN,
    PA10 == 96 ~ NaN,
    PA10 %in% c(1,2,3) ~ 0,
    PA10 %in% c(4,5) ~ 1))

table(waves$Divorce)
table(is.na(waves$Divorce))

"Geography:"

# 0 = Rural
# 1 = Urban/Suburban/Mostly Retail/Mostly Industrial
# NA = NA/Other/Refused/Don't Know/Not Applicable


waves <- waves %>% 
  mutate(Geography = case_when(
    is.na(H1IR12) ~ NaN,
    H1IR12 %in% c(96,98,99) ~ NaN,
    H1IR12 == 1 ~ 0,
    H1IR12 %in% c(2,3,4,5) ~ 1))

table(waves$Geography)
table(is.na(waves$Geography))

"Gender:"

waves <- waves %>%
  mutate(Gender = case_when(
    BIO_SEX3 == 1 ~ "Male",
    BIO_SEX3 == 2 ~ "Female"))

table(waves$Gender)
table(is.na(waves$Gender))

"Gender Recoded:"
# 0 = Male
# 1 = Female

waves <- waves %>%
  mutate(Gender.Coded = recode(
    Gender,
    "Female" = 1,
    "Male" = 0))

table(waves$Gender.Coded)

"Hispanic:"
# 0 = No
# 1 = Yes
# NA = Refused/Don't Know

table(waves$H1GI4)
table(is.na(waves$H1GI4))

waves <- waves %>%
  mutate(Hispanic = case_when(
    is.na(H1GI4) ~ NaN,
    H1GI4 %in% c(6,8) ~ NaN,
    H1GI4 == 0 ~ 0,
    H1GI4 == 1 ~ 1))

table(waves$Hispanic)
table(is.na(waves$Hispanic))

"Citizenship"

# 0 = No
# 1 = Yes
# NA = Refused/Don't Know

table(waves$H1GI14)

waves <- waves %>%
  mutate(Citizenship = case_when(
    is.na(H1GI14) ~ NaN,
    H1GI14 %in% c(6,8) ~ NaN,
    H1GI14 == 0 ~ 0,
    H1GI14 == 1 ~ 1,
    H1GI14 == 7 ~ 1))

table(waves$Citizenship)
table(is.na(waves$Citizenship))

"Employed as an Adult:"

# Wave 3 respondents between 18 and 24 years old
# 0 = No
# 1 = Yes
# NA = NA/Not applicable/Refused

table(waves$H3DA28)
table(is.na(waves$H3DA28))
waves <- waves %>%
  mutate(AEmployed = case_when(
    is.na(H3DA28) ~ NaN,
    H3DA28 %in% c(6,8,9) ~ NaN,
    H3DA28 == 0 ~ 0,
    H3DA28 == 1 ~ 1))

table(waves$AEmployed)
table(is.na(waves$AEmployed))

"Does your mother have a job?"

# 0 = no job
# 1 = any type of job
# NA = NA/Not Applicable/Refused/Don't Know/No Mother

table(waves$H1RM4)

waves <- waves %>%
  mutate(MotherEmployed = case_when(
    is.na(H1RM4) ~ NaN,
    H1RM4 %in% c(96, 97, 98, 99) ~ NaN,
    H1RM4 == 16 ~ 0,
    H1RM4 %in% seq(1,15) ~ 1))

table(waves$MotherEmployed)
table(is.na(waves$MotherEmployed))

"Does your father have a job?"

# 0 = no job
# 1 = any type of job
# NA = NA/Not Applicable/Refused/Don't Know/No Father

table(waves$H1RF4)

waves <- waves %>%
  mutate(FatherEmployed = case_when(
    is.na(H1RF4) ~ NaN,
    H1RF4 %in% c(96, 97, 98, 99) ~ NaN,
    H1RF4 == 16 ~ 0,
    H1RF4 %in% seq(1,15) ~ 1))

table(waves$FatherEmployed)
table(is.na(waves$FatherEmployed))

"Unemployment:"

# 0 = Yes, both parents unemployed
# 1 = At least 1 parent has a job
# NA = NA

waves <- waves %>%
  mutate(Unemployment = case_when(
    is.na(MotherEmployed) & is.na(FatherEmployed) ~ NaN,
    is.na(MotherEmployed) & FatherEmployed == 0 ~ NaN,
    MotherEmployed == 0 & is.na(FatherEmployed) ~ NaN,
    MotherEmployed == 1 | FatherEmployed == 1 ~ 0,
    MotherEmployed == 0 & FatherEmployed == 0 ~ 1))

table(waves$MotherEmployed, waves$FatherEmployed)
table(is.na(waves$MotherEmployed), is.na(waves$FatherEmployed))
table(waves$Unemployment)
table(is.na(waves$Unemployment))

"Mother's Hours Work per Week:"

# NA = 0/Refused/Not Applicable/No Dad
# [1-168] = [1-168]
# 40 = Don't Know

table(waves$H1RM7)
table(is.na(waves$H1RM7))

waves <- waves %>%
  mutate(MotherHoursHolder = replace(H1RM7, H1RM7 %in% c(NA, 996, 997, 999), NA)) %>%
  mutate(MotherHoursWeek = replace(MotherHoursHolder, MotherHoursHolder == 998, 40))

table(waves$MotherEmployed, waves$H1RM7)
table(is.na(waves$MotherHoursHolder))
summary(waves$MotherHoursWeek)

"Father's Hours Work per Week:"

# NA = 0/Refused/Not Applicable/No Dad
# [1-168] = [1-168]
# 40 = Don't Know

table(waves$H1RF7)
table(is.na(waves$H1RF7))

waves <- waves %>%
  mutate(FatherHoursHolder = replace(H1RF7, H1RF7 %in% c(NA, 996, 997, 999), NA)) %>%
  mutate(FatherHoursWeek = replace(FatherHoursHolder, FatherHoursHolder == 998, 40))

table(waves$FatherEmployed, waves$H1RF7)
table(is.na(waves$FatherHoursHolder))
summary(waves$FatherHoursWeek)

"Mother Regular Overtime?"

# 0 = No
# 1 = Yes, usually over 40 hours per week

waves <- waves %>%
  mutate(MotherOvertime = case_when(
    is.na(MotherHoursWeek) ~ NaN,
    MotherHoursWeek <= 40 ~ 0,
    MotherHoursWeek > 40 ~ 1))

table(waves$MotherOvertime)
table(is.na(waves$MotherOvertime))

"Father Regular Overtime?"

# 0 = No
# 1 = Yes, usually over 40 hours per week

waves <- waves %>%
  mutate(FatherOvertime = case_when(
    is.na(FatherHoursWeek) ~ NaN,
    FatherHoursWeek <= 40 ~ 0,
    FatherHoursWeek > 40 ~ 1))

table(waves$FatherOvertime)
table(is.na(waves$FatherOvertime))

"W1 Grade:"

table(waves$S3) # What grade are you in?
table(waves$H1GI20) # What grade are you in?
table(is.na(waves$H1GI20))

waves <- waves %>%
  mutate(W1Grade = replace(H1GI20, H1GI20 %in% c(NA, 96, 97, 98, 99), NaN))

table(waves$W1Grade)
table(is.na(waves$W1Grade))

"W1 Grade Level:"

waves <- waves %>%
  mutate(W1GradeLevel = case_when(
    W1Grade %in% c(7,8) ~ "Middle",
    W1Grade %in% c(9,10,11,12) ~ "High"))

table(waves$W1GradeLevel)
table(waves$W1Grade)

"W2 Grade:"

# 7-12
# 13 = Beyond High School
# NA = NA/Not in School/Don't Know/No Grades

table(waves$H2GI9) # What grade are you in?

waves <- waves %>%
  mutate(W2Grade = replace(H2GI9, H2GI9 %in% c(NA, 14, 97, 98), NA))

table(waves$W2Grade)
table(is.na(waves$W2Grade))

"W2 Grade Level:"

waves <- waves %>%
  mutate(W2GradeLevel = case_when(
    W2Grade %in% c(7,8) ~ "Middle",
    W2Grade %in% c(9,10,11,12) ~ "High",
    W2Grade == 13 ~ "Beyond"))

table(waves$W2GradeLevel)
table(waves$W2Grade)

"Family Size:"

# 1 = Lives by self
# 6 = 6 or more people

table(waves$S27)
table(is.na(waves$S27))
waves <- waves %>%
  mutate(FamilySize = replace(S27, S27 %in% c(7, 99), NA))
table(waves$FamilySize)

"Death:"

# 0 = no parent has died
# 1 = a parent has died

table(waves$H1NM3)
table(waves$H1NF3)

waves <- waves %>%
  mutate(MotherHolder = replace(H1NM3, H1NM3 %in% c(NA, 96, 98), NA)) %>%
  mutate(MotherAlive = case_when(
    MotherHolder %in% seq(0, 18) ~ 0,
    MotherHolder == 97 ~ 1))

waves <- waves %>%
  mutate(FatherHolder = replace(H1NF3, H1NF3 %in% c(NA, 98), NA)) %>%
  mutate(FatherAlive = case_when(
    FatherHolder %in% seq(0, 18) ~ 0,
    FatherHolder == 97 ~ 1))

table(waves$MotherAlive)
table(waves$FatherAlive)

waves <- waves %>%
  mutate(Death = case_when(
    is.na(MotherAlive) ~ NaN,
    is.na(FatherAlive) ~ NaN,
    MotherAlive == 0 ~ 1,
    FatherAlive == 0 ~ 1,
    MotherAlive == 1 & FatherAlive == 1 ~ 0))

table(waves$Death)

"Highest Grade Completed:"

# Continuous
# Only middle school and above
# Anything higher than grade 12 is grade 12

table(highgrade15)


table(waves$H3ED1)

waves <- waves %>%
  mutate(HighestGrade = replace(H3ED1, H3ED1 %in% c(97, 98, 99), NA)) %>%
  mutate(HighGrade15 = replace(HighestGrade, HighestGrade %in% c(13,14,15,16,17,18,19,20,21,22), 12))

table(waves$HighestGrade)
table(waves$HighGrade15)

"Did you dropout of Middle School?:"
# 0 = No
# 1 = Yes

table(waves$HighGrade15)

waves <- waves %>%
  mutate(MiddleDropout = case_when(
    is.na(HighGrade15) ~ NaN,
    HighGrade15 %in% c(6,7) ~ 1,
    TRUE ~ 0))

table(waves$MiddleDropout)

"Did you drop out of High School?:"
waves <- waves %>%
  mutate(HighDropout = case_when(
    is.na(HighGrade15) ~ NaN,
    HighGrade15 %in% c(8,9,10,11) ~ 1,
    TRUE ~ 0))

table(waves$HighDropout)
table(is.na(waves$HighDropout))

"Have you ever been suspended?"

# 0 = No
# 1 = Yes

table(waves$H1ED7)

waves <- waves %>%
  mutate(EverSuspend = replace(H1ED7, H1ED7 %in% c(6, 8), NA))

table(waves$EverSuspend)
table(is.na(waves$EverSuspend))
"Grade of Last Suspension:"

table(waves$H1ED8)
table(waves$EverSuspend, waves$H1ED8)

waves <- waves %>%
  mutate(SuspendGrade = replace(H1ED8, H1ED8 %in% c(96, 97, 98, 99), NA))

table(waves$SuspendGrade)

"Elementary School Suspend:"

# 0 = No
# 1 = Yes

waves <- waves %>%
  mutate(ESuspend = case_when(
    SuspendGrade %in% seq(1, 5) ~ 1,
    TRUE ~ 0))

table(waves$ESuspend)

"Middle School Suspend:"

# 0 = No
# 1 = Yes

waves <- waves %>%
  mutate(MSuspend = case_when(
    SuspendGrade %in% c(6, 7, 8) ~ 1,
    TRUE ~ 0))

table(waves$MSuspend)

"High School Suspend:"

# 0 = No
# 1 = Yes

waves <- waves %>%
  mutate(HSuspend = case_when(
    SuspendGrade %in% c(9, 10, 11, 12) ~ 1,
    TRUE ~ 0))

table(waves$HSuspend)

"Expulsion from School:"

# 0 = No
# 1 = Yes

table(waves$H1ED9)

waves <- waves %>%
  mutate(Expel = replace(H1ED9, H1ED9 %in% c(6, 8), NA))

table(waves$Expel)


"Aggressive Delinquency (AD):"

# 0 = Never
# 1 = Once
# 2 = More than once

# 1. Getting into a serious physical fight
# 2. Pulling a knife/gun on someone
# 3. Shooting or stabbing someone

table(waves$H1DS5) # Serious physical fight
table(waves$H1FV7) # Pulling a knife/gun on someone
table(waves$H1FV8) # Shooting or stabbing someone

# 0 = Zero days
# 1 = One day
# 2 = Two or Three days
# 3 = Four or Five days
# 4 = Six or more days

table(waves$H1FV9) # Carrying a weapon to school

"Non-aggressive Delinquency (NAD):"

# 0 = Never
# 1 = 1 or 2 times
# 2 = 3 or 4 times
# 3 = 5 or more times

# 1. Lying about whereabouts
# 2. Shoplifting
# 3. Stealing less than $50
# 4. Stealing more than $50

table(waves$H1DS3) # Lying about whereabouts
table(waves$H1DS4) # Shoplifting
table(waves$H1DS13) # Stealing less than $50
table(waves$H1DS9) # Stealing more than $50

"Fixing the variables to remove Refused/Don't Know/Not Applicable"

waves <- waves %>%
  mutate(ADPhysicalFight = replace(H1DS5, H1DS5 %in% c(6,8,9), NA)) %>%
  mutate(ADKnifeGun = replace(H1FV7, H1FV7 %in% c(6,8,9), NA)) %>%
  mutate(ADShootStab = replace(H1FV8, H1FV8 %in% c(6,8,9), NA)) %>%
  mutate(ADWeaponSchool = replace(H1FV9, H1FV9 %in% c(6,8,9), NA)) %>%
  mutate(NADLying = replace(H1DS3, H1DS3 %in% c(6,8,9), NA)) %>%
  mutate(NADShoplift = replace(H1DS4, H1DS4 %in% c(6,8,9), NA)) %>%
  mutate(NADStealLess = replace(H1DS13, H1DS13 %in% c(6,8,9), NA)) %>%
  mutate(NADStealMore = replace(H1DS9, H1DS9 %in% c(6,8,9), NA))

# Aggressive Delinquency
table(waves$ADPhysicalFight)
table(waves$ADKnifeGun)
table(waves$ADShootStab)
table(waves$ADWeaponSchool)

# Non-aggressive Delinquency
table(waves$NADLying)
table(waves$NADShoplift)
table(waves$NADStealLess)
table(waves$NADStealMore)

"Creating Generalized Delinquency Variables:"


# Just sum them up for a general delinquency score?
# Max possible aggressive delinquency is 16
# Max possible non-aggressive delinquency score is 12

waves <- waves %>%
  mutate(AggDelinq = ADPhysicalFight + ADKnifeGun + ADShootStab + ADWeaponSchool) %>%
  mutate(NonAggDelinq = NADLying + NADShoplift + NADStealLess + NADStealMore)

table(waves$AggDelinq)
table(waves$NonAggDelinq)  

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

table(waves$H1WP9) # Close to mother
table(waves$H1WP13) # Close to father
table(waves$H1WP10) # Mother cares about you
table(waves$H1WP14) # Father cares about you

"Fixing the variables to remove Refused/Don't Know/Not Applicable:"

waves <- waves %>%
  mutate(AtPCloseMother = replace(H1WP9, H1WP9 %in% c(6, 7, 8, 9), NA)) %>%
  mutate(AtPCloseFather = replace(H1WP13, H1WP13 %in% c(6, 7, 8, 9), NA)) %>%
  mutate(AtPMotherCare = replace(H1WP10, H1WP10 %in% c(6, 7, 8, 9), NA)) %>%
  mutate(AtPFatherCare = replace(H1WP14, H1WP14 %in% c(6, 7, 8, 9), NA))


table(waves$AtPCloseMother)  
table(waves$AtPCloseFather)
table(waves$AtPMotherCare)
table(waves$AtPFatherCare)

"Create the Attachment to Parents variable:"

waves <- waves %>%
  mutate(AttachParents = AtPCloseMother + AtPCloseFather + AtPMotherCare + AtPFatherCare)

table(waves$AttachParents)


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

table(waves$H1ED19) # Close to people at school
table(waves$H1ED22) # Happy to be at school
table(waves$H1ED23) # Treat students fairly
table(waves$H1ED20) # Part of the school


"Fixing the variables to remove Refused/Don't Know/Not Applicable:"

waves <- waves %>%
  mutate(AtSClose = replace(H1ED19, H1ED19 %in% c(6, 7, 8), NA)) %>%
  mutate(AtSHappy = replace(H1ED22, H1ED22 %in% c(6, 7, 8), NA)) %>%
  mutate(AtSFairly = replace(H1ED23, H1ED23 %in% c(6, 7, 8), NA)) %>%
  mutate(AtSPartOf = replace(H1ED20, H1ED20 %in% c(6, 7, 8), NA))

# Reversing the Likert scale:

waves <- waves %>%
  mutate(AtSClose = 6-AtSClose) %>%
  mutate(AtSHappy = 6-AtSHappy) %>%
  mutate(AtSFairly = 6-AtSFairly) %>%
  mutate(AtSPartOf = 6-AtSPartOf)

# Attachment to School
table(waves$AtSClose)
table(waves$AtSHappy)
table(waves$AtSFairly)
table(waves$AtSPartOf)



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

table(waves$H1TO9)  # Cigarette
table(waves$H1TO29) # Alcohol
table(waves$H1TO33) # Marijuana

waves <- waves %>%
  mutate(AtDPCigs = replace(H1TO9, H1TO9 %in% c(6, 8, 9), NA)) %>%
  mutate(AtDPAlcohol = replace(H1TO29, H1TO29 %in% c(6, 8, 9), NA)) %>%
  mutate(AtDPWeed = replace(H1TO33, H1TO33 %in% c(6, 8, 9), NA))

# Attachment to Delinquent Peers
table(waves$AtDPCigs)
table(waves$AtDPAlcohol)
table(waves$AtDPWeed)

"Creating the Attachment to Delinquent Peers variable:"

waves <- waves %>%
  mutate(AtDelinqPeers = AtDPCigs + AtDPAlcohol + AtDPWeed)

table(waves$AtDelinqPeers)



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

table(waves$H1ED11) # English
table(waves$H1ED12) # Math
table(waves$H1ED13) # History
table(waves$H1ED14) # Science


"Fixing the variables to remove Refused/Don't Know/Not Applicable/Other:"

waves <- waves %>%
  mutate(APEnglish = replace(H1ED11, H1ED11 %in% c(5, 6, 96, 97, 98, 99), NA)) %>%
  mutate(APMath = replace(H1ED12, H1ED12 %in% c(5, 6, 96, 97, 98, 99), NA)) %>%
  mutate(APHistory = replace(H1ED13, H1ED13 %in% c(5, 6, 96, 97, 98, 99), NA)) %>%
  mutate(APScience = replace(H1ED14, H1ED14 %in% c(5, 6, 96, 97, 98, 99), NA))

# Academic Performance:
table(waves$APEnglish)
table(waves$APMath)
table(waves$APHistory)
table(waves$APScience)

"Reversing the grading:"

waves <- waves %>%
  mutate(APEnglish = 5-APEnglish,
         APMath = 5-APMath,
         APHistory = 5-APHistory,
         APScience = 5-APScience)

# Academic Performance:
table(waves$APEnglish)
table(waves$APMath)
table(waves$APHistory)
table(waves$APScience)

waves <- waves %>%
  mutate(AcadPerform = APEnglish + APMath + APHistory + APScience)

summary(waves$AcadPerform)

"Victim Variable:"

"Should have Youth and Adult victim separate and then a cumulative var?"

# 0 = not a victim in past 12 months
# 1 = victim in past 12 months

# 1. You were shot/stabbed
# 2. You got jumped
# 3. You had a knife/gun pulled on you
# 4. You saw someone get shot/stabbed

"Variables included"

# 1. You were cut/shot/stabbed:
table(waves$H1FV3)
table(waves$H1FV4)
table(waves$H2FV3)
table(waves$H2FV4)
table(waves$H3DS18D)
table(waves$H3DS18E)
table(waves$H4DS16)

# 2. You were jumped:
table(waves$H1FV6)
table(waves$H2FV5)
table(waves$H3DS18F)
table(waves$H3DS18G)
table(waves$H4DS18)

# 3. You had a knife/gun pulled on you:
table(waves$H1FV2)
table(waves$H2FV2)
table(waves$H3DS18B)
table(waves$H3DS18C)
table(waves$H4DS15)

# 4. You saw someone get shot/stabbed:
table(waves$H1FV1)
table(waves$H2FV1)
table(waves$H3DS18A)
table(waves$H4DS14)

"Cleaning up each variable:"
# You were shot/stabbed:
waves <- waves %>%
  mutate(JH1FV3 = case_when(
    H1FV3 %in% c(6, 8, 9) ~ NaN,
    H1FV3 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH1FV4 = case_when(
    H1FV4 %in% c(6, 8, 9) ~ NaN,
    H1FV4 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV3 = case_when(
    H2FV3 %in% c(6, 8) ~ NaN,
    H2FV3 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV4 = case_when(
    H2FV4 %in% c(6, 8) ~ NaN,
    H2FV4 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH3DS18D = replace(H3DS18D, H3DS18D %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH3DS18E = replace(H3DS18E, H3DS18E %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH4DS16 = replace(H4DS16, H4DS16 %in% c(NA, 6, 8), NA))

table(waves$JH1FV3)
table(waves$JH1FV4)
table(waves$JH2FV3)
table(waves$JH2FV4)
table(waves$AH3DS18D)
table(waves$AH3DS18E)
table(waves$AH4DS16)

# 2. You were jumped:

waves <- waves %>%
  mutate(JH1FV6 = case_when(
    H1FV6 %in% c(6, 8, 9) ~ NaN,
    H1FV6 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV5 = case_when(
    H2FV5 %in% c(6, 8) ~ NaN,
    H2FV5 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH3DS18F = replace(H3DS18F, H3DS18F %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH3DS18G = replace(H3DS18G, H3DS18G %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH4DS18 = replace(H4DS18, H4DS18 %in% c(NA, 6, 8), NA))

table(waves$JH1FV6)
table(waves$JH2FV5)
table(waves$AH3DS18F)
table(waves$AH3DS18G)
table(waves$AH4DS18)

# 3. You had a knife/gun pulled on you:
waves <- waves %>%
  mutate(JH1FV2 = case_when(
    H1FV2 %in% c(6, 8, 9) ~ NaN,
    H1FV2 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV2 = case_when(
    H2FV2 %in% c(6, 8) ~ NaN,
    H2FV2 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH3DS18B = replace(H3DS18B, H3DS18B %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH3DS18C = replace(H3DS18C, H3DS18C %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH4DS15 = replace(H4DS15, H4DS15 %in% c(NA, 6, 8), NA))

table(waves$JH1FV2)
table(waves$JH2FV2)
table(waves$AH3DS18B)
table(waves$AH3DS18C)
table(waves$AH4DS15)

# 4. You saw someone get shot/stabbed:
waves <- waves %>%
  mutate(JH1FV1 = case_when(
    H1FV1 %in% c(6, 8, 9) ~ NaN,
    H1FV1 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV1 = case_when(
    H2FV1 %in% c(6, 8) ~ NaN,
    H2FV1 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH3DS18A = case_when(
    H3DS18A %in% c(6, 8, 9) ~ NaN,
    H3DS18A == 1 ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH4DS14 = replace(H4DS14, H4DS14 %in% c(NA, 6, 8), NA))

table(waves$JH1FV1)
table(waves$JH2FV1)
table(waves$AH3DS18A)
table(waves$AH4DS14)

"Creating the Juvenile Victim Variable:"

table(waves$JH1FV3)
table(waves$JH1FV4)
table(waves$JH2FV3)
table(waves$JH2FV4)
table(waves$JH1FV6)
table(waves$JH2FV5)
table(waves$JH1FV2)
table(waves$JH2FV2)
table(waves$JH1FV1)
table(waves$JH2FV1)

waves <- waves %>%
  mutate(JVictim = case_when(
    JH1FV3 == 1 | JH1FV4 == 1 |
    JH2FV3 == 1 | JH2FV4 == 1 |
    JH1FV6 == 1 | JH2FV5 == 1 |
    JH1FV2 == 1 | JH2FV2 == 1 |
    JH1FV1 == 1 | JH2FV1 == 1 ~ 1,
    TRUE ~ 0))

table(waves$JVictim)

test <-  waves %>%
  select(JH1FV3, JH1FV4, JH2FV3, JH2FV4, JH1FV6,
         JH2FV5, JH1FV2, JH2FV2, JH1FV1, JH2FV1) %>%
  mutate(Total = rowSums(.[1:10],na.rm = TRUE))
table(test$Total)

"Creating the Adult Victim Variable:"

table(waves$AH3DS18D)
table(waves$AH3DS18E)
table(waves$AH4DS16)
table(waves$AH3DS18F)
table(waves$AH3DS18G)
table(waves$AH4DS18)
table(waves$AH3DS18B)
table(waves$AH3DS18C)
table(waves$AH4DS15)
table(waves$AH3DS18A)
table(waves$AH4DS14)

waves <- waves %>%
  mutate(AVictim = case_when(
    AH3DS18D == 1 | AH3DS18E == 1 | AH4DS16 == 1 |
      AH3DS18F == 1 | AH3DS18G == 1 | AH4DS18 == 1 |
      AH3DS18B == 1 | AH3DS18C == 1 | AH4DS15 == 1 |
      AH3DS18A == 1 | AH4DS14 == 1 ~ 1,
    TRUE ~ 0))

table(waves$AVictim)

test <-  waves %>%
  select(AH3DS18D, AH3DS18E, AH4DS16, AH3DS18F, AH3DS18G, AH4DS18,
         AH3DS18B, AH3DS18C, AH4DS15, AH3DS18A, AH4DS14) %>%
  mutate(Total = rowSums(.[1:11], na.rm = TRUE))
table(test$Total)


"Creating the Victim Variable:"

# 0 = Was Not a youth or adult victim
# 1 = Was a youth or adult victim

waves <- waves %>%
  mutate(Victim = case_when(
    JVictim == 1 | AVictim == 1 ~ 1,
    TRUE ~ 0))

table(waves$Victim)
table(waves$JVictim, waves$AVictim) # Check if TRUE

"Can't Pay your Bills: "

# 0 = can pay bills
# 1 = can't pay bills

table(waves$PA56)

waves <- waves %>%
  mutate(CantPayBills = case_when(
    PA56 %in% c(NA, 6) ~ NaN,
    PA56 == 0 ~ 1,
    PA56 == 1 ~ 0))

table(waves$CantPayBills)

###################
### TRAUMA Vars ###
###################

"Neglect:"
table(waves$H3MA2) # basic needs
table(waves$H3MA1) # left home alone

# Basic Needs
waves <- waves %>%
  mutate(BasicNeeds = case_when(
    H3MA2 %in% c(NA, 96, 98, 99) ~ NaN,
    H3MA2 %in% c(1, 2, 3, 4, 5) ~ 1,
    TRUE ~ 0))

table(waves$BasicNeeds)

# Left Home Alone
waves <- waves %>%
  mutate(HomeAlone = case_when(
    H3MA1 %in% c(NA, 96, 98, 99) ~ NaN,
    H3MA1 %in% c(1, 2, 3, 4, 5) ~ 1,
    H3MA1 == 6 ~ 0))

table(waves$HomeAlone)


"Physical Abuse:"
table(waves$H3MA3) # slapped/hit/kicked

waves <- waves %>%
  mutate(SlapHitKick = case_when(
    H3MA3 %in% c(NA, 96, 98, 99) ~ NaN,
    H3MA3 %in% c(1, 2, 3, 4, 5) ~ 1,
    H3MA3 == 6 ~ 0))

table(waves$SlapHitKick)


"Sexual Abuse:"
table(waves$H3MA4) # touched in sexual way

waves <- waves %>%
  mutate(Touched = case_when(
    H3MA4 %in% c(NA, 96, 98, 99, 0) ~ NaN,
    H3MA4 %in% c(1, 2, 3, 4, 5) ~ 1,
    H3MA4 == 6 ~ 0))

table(waves$Touched)


"Emotional Abuse:"
table(waves$H4MA1) # hurt your feelings/feel unloved

waves <- waves %>%
  mutate(HurtFeelings = case_when(
    H4MA1 %in% c(NA, 96, 98) ~ NaN,
    H4MA1 %in% c(1, 2, 3, 4, 5) ~ 1,
    H4MA1 == 6 ~ 0))

table(waves$HurtFeelings)


"Parental Incarceration:"
table(waves$H4WP9) # Bio Father jail
table(waves$H4WP3) # Bio Mother jail
table(waves$H4WP30) # Father Figure jail
table(waves$H4WP16) # Mother Figure jail

# Bio Father Jail
waves <- waves %>%
  mutate(BioFatherJail = replace(H4WP9, H4WP9 %in% c(6, 8), NA))

table(waves$BioFatherJail)

# Bio Mother Jail
waves <- waves %>%
  mutate(BioMotherJail = replace(H4WP3, H4WP3 %in% c(6, 8), NA))

table(waves$BioMotherJail)

# Father Figure Jail
waves <- waves %>%
  mutate(FigFatherJail = replace(H4WP30, H4WP30 %in% c(6, 7, 8), NA))

table(waves$FigFatherJail)

# Mother Figure Jail
waves <- waves %>%
  mutate(FigMotherJail = replace(H4WP16, H4WP16 %in% c(7, 8), NA))

table(waves$FigMotherJail)

# Any parental figure incarcerated:

waves <- waves %>%
  mutate(aceParentJail = case_when(
    is.na(BioMotherJail) & is.na(BioFatherJail) & is.na(FigMotherJail) & is.na(FigFatherJail) ~ NaN,
    BioMotherJail == 1 | BioFatherJail == 1 | FigMotherJail == 1 | FigFatherJail == 1 ~ 1,
    TRUE ~ 0))

table(waves$aceParentJail)

"Parent Binge Drinker:"

# More 5 drinks on one occasion
table(waves$PA62)

waves <- waves %>%
  mutate(BingeDrink = case_when(
    PA62 %in% c(NA, 96) ~ NaN,
    PA62 %in% c(2, 3, 4, 5, 6) ~ 1,
    PA62 == 1 ~ 0))

table(waves$BingeDrink)

"Creating the Trauma variable:"

# 0 = no traumas experienced
# 1 = at least 1 trauma experienced

waves <- waves %>%
  mutate(Trauma = case_when(
    JVictim == 1 | BasicNeeds == 1 | HomeAlone == 1 |
      SlapHitKick == 1 | Touched == 1 | HurtFeelings == 1 |
      BioFatherJail == 1 | BioMotherJail == 1 | FigFatherJail == 1 |
      FigMotherJail == 1 | BingeDrink ==1 ~ 1,
    TRUE ~ 0))

table(waves$Trauma)

"Homeless:"
# 0 = never been homeless
# 1 = has been homeless at least once
# NA = Skip/Don't Know/NA

waves <- waves %>%
  mutate(Homeless = replace(H3HR24, H3HR24 %in% c(7, 8, 9), NA))

table(waves$Homeless)

"Adult Delinquency:"

# Non-aggressive Adult Delinquency:

table(waves$H4DS2) # Steal More $50
table(waves$H4DS6) # Steal Less $50
table(waves$H4DS8) # Steal Property
table(waves$H4DS1) # Damage others property

# Aggressive Adult Delinquency:
table(waves$H4DS20) # Shot / Stabbed someone
table(waves$H4DS11) # Got into a serious physical fight
table(waves$H4DS19) # You pulled a knife/gun on someone
table(waves$H4DS5) # Sold drugs

"Fixing the variables to remove Refused/Don't Know/Not Applicable"

"Adult Steal More than $50:"
waves <- waves %>%
  mutate(AStealMore = case_when(
    H4DS2 %in% c(6, 8) ~ NaN,
    H4DS2 == 0 ~ 0,
    H4DS2 %in% c(1, 2, 3) ~ 1))
table(waves$AStealMore)

"Adult Steal Less than $50:"
waves <- waves %>%
  mutate(AStealLess = case_when(
    H4DS6 %in% c(6, 8) ~ NaN,
    H4DS6 == 0 ~ 0,
    H4DS6 %in% c(1, 2, 3) ~ 1))
table(waves$AStealLess)

"Adult Steal Property:"
waves <- waves %>%
  mutate(AStealProperty = case_when(
    H4DS8 %in% c(6, 8) ~ NaN,
    H4DS8 == 0 ~ 0,
    H4DS8 %in% c(1, 2, 3) ~ 1))
table(waves$AStealProperty)

"Adult Damage Other Property:"
waves <- waves %>%
  mutate(ADamageProperty = case_when(
    H4DS1 %in% c(6, 8) ~ NaN,
    H4DS1 == 0 ~ 0,
    H4DS1 %in% c(1, 2, 3) ~ 1))
table(waves$ADamageProperty)

"Adult Shoot/Stab Someone:"
waves <- waves %>%
  mutate(AShootStab = replace(H4DS20, H4DS20 %in% c(NA, 6, 8), NA))
table(waves$AShootStab)

" Adult Serious Physical Fight:"
waves <- waves %>%
  mutate(APhysicalFight = case_when(
    H4DS11 %in% c(6, 8) ~ NaN,
    H4DS11 == 0 ~ 0,
    H4DS11 %in% c(1, 2, 3) ~ 1))
table(waves$APhysicalFight)

"Adult Pulled a Knife/Gun on Someone"
waves <- waves %>%
  mutate(AKnifeGun = replace(H4DS19, H4DS19 %in% c(NA, 6, 8), NA))
table(waves$AKnifeGun)

"Adult Sell Drugs:"
waves <- waves %>%
  mutate(ASellDrugs = case_when(
    H4DS5 %in% c(6, 8) ~ NaN,
    H4DS5 == 0 ~ 0,
    H4DS5 %in% c(1, 2, 3) ~ 1))
table(waves$ASellDrugs)

"Foster Home:"

waves <- waves %>%
  mutate(FosterHome = replace(H3OD31, H3OD31 %in% c(NA, 6, 8, 9), NA))
table(waves$FosterHome)

"Juvenile Delinquency:

The prefix will be JD"

# Drunk Driving:
table(waves$H1JO9)

waves <- waves %>%
  mutate(JDDriveDrunk = case_when(
    H1JO9 %in% c(6, 8) ~  NaN,
    H1JO9 %in% c(0, 7) ~ 0,
    H1JO9 == 1 ~ 1)) 

table(waves$JDDriveDrunk)

#############
### Drugs ###
#############

# Driven while High on Drugs:

waves <- waves %>%
  mutate(JDDriveHigh = case_when(
    H1JO19 %in% c(6, 8) ~ NaN,
    H1JO19 %in% c(0, 7) ~ 0,
    H1JO19 == 1 ~ 1))

table(waves$JDDriveHigh)

# Sell Marijuana / Other Drugs in Wave 1:

waves <- waves %>%
  mutate(JDSellDrugs = case_when(
    H1DS12 %in% c(6, 8, 9) ~ NaN,
    H1DS12 == 0 ~ 0,
    H1DS12 %in% c(1,2,3) ~ 1))

table(waves$JDSellDrugs)

# Use illegal drugs in past 30 days:

waves <- waves %>%
  mutate(JDIllegalDrugUse = case_when(
    H1TO42 %in% c(996, 998, 999) ~ NaN,
    H1TO42 %in% c(0, 997) ~ 0,
    H1TO42 %in% seq(1, 900) ~ 1))

table(waves$JDIllegalDrugUse)

# Cocaine use:

waves <- waves %>%
  mutate(JDCocaineUse = case_when(
    H1TO35 %in% c(996, 998, 999) ~ NaN,
    H1TO35 == 997 ~ 0,
    TRUE ~ 1))

table(waves$JDCocaineUse)

# Using illegal drugs via a Needle:

waves <- waves %>%
  mutate(JDIllegalDrugNeedle = case_when(
    H1TO43 %in% c(6, 8, 9) ~ NaN,
    H1TO43 %in% c(0, 7) ~ 0,
    H1TO43 == 1 ~ 1))

table(waves$JDIllegalDrugNeedle)

# Alone first time drug use:

waves <- waves %>%
  mutate(JDAloneDrugUse = case_when(
    H1JO24 %in% c(6, 8) ~ NaN,
    H1JO24 %in% c(0, 7) ~ 0,
    H1JO24 == 1 ~ 1))

table(waves$JDAloneDrugUse)

# Fight while on drugs:

waves <- waves %>%
  mutate(JDFightOnDrugs = case_when(
    H1JO21 %in% c(6, 8) ~ NaN,
    H1JO21 %in% c(0, 7) ~ 0,
    H1JO21 == 1 ~ 1))

table(waves$JDFightOnDrugs)

# Carrying a weapon while on drugs:

waves <- waves %>%
  mutate(JDWeaponOnDrugs = case_when(
    H1JO23 %in% c(6, 8) ~ NaN,
    H1JO23 %in% c(0, 7) ~ 0,
    H1JO23 == 1 ~ 1))

table(waves$JDWeaponOnDrugs)


################
### STEALING ###
################

# Stealing something less than $50:
waves <- waves %>%
  mutate(JDStealLess = case_when(
    H1DS13 %in% c(6, 8, 9) ~ NaN,
    H1DS13 == 0 ~ 0,
    H1DS13 %in% c(1, 2, 3) ~ 1))

table(waves$JDStealLess)

# Stealing something more than $50 in past 12 months:

waves <- waves %>%
  mutate(JDStealMore = case_when(
    H1DS9 %in% c(6, 8, 9) ~ NaN,
    H1DS9 == 0 ~ 0,
    H1DS9 %in% c(1, 2, 3) ~ 1))

table(waves$JDStealMore)

# Stealing from a store:

waves <- waves %>%
  mutate(JDStealStore = case_when(
    H1DS4 %in% c(6, 8, 9) ~ NaN,
    H1DS4 == 0 ~ 0,
    H1DS4 %in% c(1, 2, 3) ~ 1))

table(waves$JDStealStore)


#############
### OTHER ###
#############

# Pulled a knife/gun on someone past 12 months:
waves <- waves %>%
  mutate(JDKnifeGun = case_when(
    H1FV7 %in% c(6, 8, 9) ~ NaN,
    H1FV7 == 0 ~ 0,
    H1FV7 %in% c(1, 2) ~ 1))

table(waves$JDKnifeGun)


# You shot/stabbed someone past 12 months:
waves <- waves %>%
  mutate(JDShootStab = case_when(
    H1FV8 %in% c(6, 8, 9) ~ NaN,
    H1FV8 == 0 ~ 0,
    H1FV8 %in% c(1, 2) ~ 1))

table(waves$JDShootStab)


# Lie to parents/guardians about whereabouts past 12 months:
waves <- waves %>%
  mutate(JDLying = case_when(
    H1DS3 %in% c(6, 8, 9) ~ NaN,
    H1DS3 == 0 ~ 0,
    H1DS3 %in% c(1, 2, 3) ~ 1))

table(waves$JDLying)


# Physical Fight past 12 months:
waves <- waves %>%
  mutate(JDPhysicalFight = case_when(
    H1JO11 %in% c(6, 8) ~ NaN,
    H1JO11 %in% c(0, 7) ~ 0,
    H1JO11 == 1 ~ 1))

table(waves$JDPhysicalFight)


# Serious Physical Fight past 12 months:
waves <- waves %>%
  mutate(JDSeriousPhysicalFight = case_when(
    H1DS5 %in% c(6, 8, 9) ~ NaN,
    H1DS5 == 0 ~ 0,
    H1DS5 %in% c(1, 2, 3) ~ 1))

table(waves$JDSeriousPhysicalFight)

# Damage Property past 12 months:
waves <- waves %>%
  mutate(JDDamageProperty = case_when(
    H1DS2 %in% c(6, 8, 9) ~ NaN,
    H1DS2 == 0 ~ 0,
    H1DS2 %in% c(1, 2, 3) ~ 1))

table(waves$JDDamageProperty)

# Used a weapon in a fight:
waves <- waves %>%
  mutate(JDWeaponFight = case_when(
    H1JO26 %in% c(6, 8, 9) ~ NaN,
    H1JO26 == 0 ~ 0,
    H1JO26 == 1 ~ 1))

table(waves$JDWeaponFight)


# Hurt Someone Badly Enough past 12 months:
waves <- waves %>%
  mutate(JDHurtBadly = case_when(
    H1DS6 %in% c(6, 8, 9) ~ NaN,
    H1DS6 == 0 ~ 0,
    H1DS6 %in% c(1, 2, 3) ~ 1))

table(waves$JDHurtBadly)


# Paint graffiti past 12 months:
waves <- waves %>%
  mutate(JDGraffitPaint = case_when(
    H1DS1 %in% c(6, 8, 9) ~ NaN,
    H1DS1 == 0 ~ 0,
    H1DS1 %in% c(1, 2, 3) ~ 1))

table(waves$JDGraffitPaint)

"Adult Crime:"

# Adult steal more than $50 in past 12 months:

table(waves$H4DS2)

waves <- waves %>%
  mutate(AdultStealMore = case_when(
    H4DS2 %in% c(6, 8) ~ NaN,
    H4DS2 == 0 ~ 0,
    H4DS2 %in% c(1, 2, 3) ~ 1))

table(waves$AdultStealMore)

# Adult steal less than $50 in past 12 months:

table(H4DS6)

waves <- waves %>%
  mutate(AdultStealLess = case_when(
    H4DS6 %in% c(6, 8) ~ NaN,
    H4DS6 == 0 ~ 0,
    H4DS6 %in% c(1, 2, 3) ~ 1))

table(waves$AdultStealLess)


# Adult pulled a knife/gun on someone past 12 months:

table(waves$H4DS19)

waves <- waves %>%
  mutate(AdultGun = case_when(
    H4DS19 %in% c(6, 8) ~ NaN,
    H4DS19 == 0 ~ 0,
    H4DS19 == 1 ~ 1))

table(waves$AdultGun)


# Adult shoot/stab someone past 12 months:

table(waves$H4DS20)

waves <- waves %>%
  mutate(AdultShootStab = case_when(
    H4DS20 %in% c(6, 8) ~ NaN,
    H4DS20 == 0 ~ 0,
    H4DS20 == 1 ~ 1))

table(waves$AdultShootStab)


# Adult Stolen/Other Property:

table(waves$H4DS8)
table(waves$H3DS8)

waves <- waves %>%
  mutate(AH4DS8 = case_when(
    H4DS8 %in% c(6, 8) ~ NaN,
    H4DS8 == 0 ~ 0,
    H4DS8 %in% c(1, 2, 3) ~ 1)) %>%
  mutate(AH3DS8 = case_when(
    H3DS8 %in% c(6, 8, 9) ~ NaN,
    H3DS8 == 0 ~ 0,
    H3DS8 %in% c(1, 2, 3) ~ 1)) %>%
  mutate(AdultOtherProperty = case_when(
    is.na(AH3DS8) & is.na(AH4DS8) ~ NaN,
    AH3DS8 == 1 | AH4DS8 == 1 ~ 1,
    TRUE ~ 0))

table(waves$AdultOtherProperty)


# Adult Physical Attack:

# If either one is TRUE, then APhysicalAttack is 1

table(waves$H4DS11)
table(waves$H3DS16)

waves <- waves %>%
  mutate(AH3DS16 = case_when( # First fighting variable
    H3DS16 %in% c(96, 98, 99) ~ NaN,
    H3DS16 == 0 ~ 0,
    H3DS16 %in% seq(1, 56) ~ 1)) %>%
  mutate(AH4DS11= case_when( # Second fighting variable
    H4DS11 %in% c(6, 8) ~ NaN,
    H4DS11 == 0 ~ 0,
    H4DS11 %in% c(1, 2, 3) ~ 1)) %>%
  mutate(AdultPhysicalAttack = case_when(
    is.na(AH3DS16) & is.na(AH4DS11) ~ NaN,
    AH3DS16 == 0 & AH4DS11 == 0 ~ 0,
    AH3DS16 == 1 | AH4DS11 == 1 ~ 1,
    TRUE ~ NaN))

table(waves$AdultPhysicalAttack)

# Adult Attack:

table(waves$H4CJ9I)

waves <- waves %>%
  mutate(AdultAttack = case_when(
    H4CJ9I %in% c(6, 8) ~ NaN,
    H4CJ9I %in% c(0, 7) ~ 0,
    H4CJ9I == 1 ~ 1))

table(waves$AdultAttack)


# Adult Destroy Property:

table(waves$H4DS1)

waves <- waves %>%
  mutate(AdultDestroyProperty = case_when(
    H4DS1 %in% c(6, 8)  ~ NaN,
    H4DS1 == 0 ~ 0,
    H4DS1 %in% c(1, 2, 3) ~ 1))

table(waves$AdultDestroyProperty)



# Adult Sell Drugs:

# H3DS5 in (1, 2, 3) or H4DS5 in (1, 2, 3) THEN AdultSellDrugs == 1

table(waves$H3DS5)
table(waves$H4DS5)

waves <- waves %>%
  mutate(AH3DS5 = case_when(
    H3DS5 %in% c(6, 8, 9) ~ NaN,
    H3DS5 == 0 ~ 0,
    H3DS5 %in% c(1, 2, 3) ~ 1)) %>%
  mutate(AH4DS5 = case_when(
    H4DS5	%in% c(6, 8) ~ NaN,
    H4DS5	== 0 ~ 0,
    H4DS5	 %in% c(1, 2, 3) ~ 1)) %>%
  mutate(AdultSellDrugs = case_when(
    is.na(AH3DS5) & is.na(AH4DS5) ~ NaN,
    AH3DS5 == 0 & AH4DS5 == 0 ~ 0,
    AH3DS5 == 1 | AH4DS5 == 1 ~ 1,
    TRUE ~ NaN))

table(waves$AdultSellDrugs)


# Adult Stolen Credit/ATM Card:

# 1 == if H3DS9 in (1, 2, 3) or H4DS9 in (1, 2, 3)
table(waves$H3DS9)
table(waves$H4DS9)

waves <- waves %>%
  mutate(AH3DS9 = case_when( # Wave 3
    H3DS9 %in% c(6, 8, 9) ~ NaN,
    H3DS9 == 0 ~ 0,
    H3DS9 %in% c(1, 2 ,3) ~ 1)) %>%
  mutate(AH4DS9 = case_when( # Wave 4
    H4DS9 %in% c(6, 8) ~ NaN,
    H4DS9 == 0 ~ 0,
    H4DS9 %in% c(1, 2, 3) ~ 1)) %>%
  mutate(AdultStolenCard = case_when(
    is.na(AH3DS9) & is.na(AH4DS9) ~ NaN,
    AH3DS9 == 0 & AH4DS9 == 0 ~ 0,
    AH3DS9 == 1 | AH4DS9 == 1 ~ 1,
    TRUE ~ NaN))

table(waves$AdultStolenCard)

"Creating Generalized Aggressive/Non-Aggressive Crime Variables:"

# Crime:

# AC = Aggressive Crime
# NAC = Non-aggressive Crime

waves <- waves %>%
  mutate(ACPhysicalAttack = replace(H4DS11, H4DS11 %in% c(6, 8), NaN)) %>%
  mutate(ACGun = AdultGun) %>%
  mutate(ACShootStab = AdultShootStab) %>%
  mutate(ACAttack = AdultAttack) %>%
  mutate(NACOtherProperty1 = replace(H4DS8, H4DS8 %in% c(6, 8), NaN)) %>%
  mutate(NACOtherProperty2 = replace(H3DS8, H3DS8 %in% c(6, 8, 9), NaN)) %>%
  mutate(NACOtherProperty = NACOtherProperty1 + NACOtherProperty2) %>%
  mutate(NACStealLess = replace(H4DS6, H4DS6 %in% c(6, 8), NaN)) %>%
  mutate(NACStealMore = replace(H4DS2, H4DS2 %in% c(6, 8), NaN))

table(waves$ACPhysicalAttack)
table(waves$ACGun)
table(waves$ACShootStab)
table(waves$ACAttack)
table(waves$NACOtherProperty)
table(waves$NACStealLess)
table(waves$NACStealMore)

waves <- waves %>%
  mutate(AggCrime = ACPhysicalAttack + ACGun + ACShootStab + ACAttack) %>%
  mutate(NonAggCrime = NACOtherProperty + NACStealLess + NACStealMore)

table(waves$AggCrime)
table(waves$NonAggCrime)


"Adverse Childhood Experiences:"

# Supervisory Neglect:

table(Waves$HomeAlone)

# Emotional Neglect:

table(waves$H1PF23)
table(waves$H1PF1)

waves <- waves %>%
  mutate(aceEmotionalNeglect1 = case_when(
    H1PF23 %in% c(6, 7, 8, 9) ~ NaN,
    H1PF23 %in% c(4, 5) ~ 0,
    TRUE ~ 1)) %>%
  mutate(aceEmotionalNeglect2 = case_when(
    H1PF1 %in% c(6, 7, 8) ~ NaN,
    H1PF1 %in% c(4, 5) ~ 0,
    TRUE ~ 1)) %>%
  mutate(aceEmotionalNeglect = case_when(
    is.na(aceEmotionalNeglect1) & is.na(aceEmotionalNeglect2) ~ NaN,
    aceEmotionalNeglect1 == 1 | aceEmotionalNeglect2 == 1 ~ 1,
    TRUE ~ 0))

table(waves$aceEmotionalNeglect)

# Physical Abuse:
table(Waves$SlapHitKick)

# Emotional Abuse:
table(Waves$HurtFeelings)

# Sexual Abuse:
table(Waves$Touched)

# Suicidal Attempt of Household Adults:
table(waves$H1SU6)

waves <- waves %>%
  mutate(aceSuicide = case_when(
    H1SU6 %in% c(6, 8, 9) ~ NaN,
    H1SU6 == 0 ~ 0,
    H1SU6 == 1 ~ 1))

table(waves$aceSuicide)
# Parental Alcohol Misuse:
table(Waves$BingeDrink)

# Parental Separation/Divorce:
table(Waves$Divorce)

# Household Adult Incarceration:
table(waves$H4WP3)
table(waves$H4WP30)
table(waves$H4WP16)
table(waves$H4WP9)

# Experience in the Foster Care System:
table(Waves$FosterHome)

# Direct Witnessing of Violence:
table(waves$JH1FV1)

# Being the Victim of Violence:
table(waves$JH1FV2)
table(waves$JH1FV3)
table(waves$JH1FV4)

waves <- waves %>%
  mutate(aceVictim = case_when(
    is.na(JH1FV2) & is.na(JH1FV3) & is.na(JH1FV4) ~ NaN,
    JH1FV2 == 1 | JH1FV3 == 1 | JH1FV4 == 1 ~ 1,
    TRUE ~ 0))

table(waves$aceVictim)






"Juvenile Incarceration:"

# 0 = No
# 1 = Yes
# NA = Missing/NA

waves <- waves %>%
  mutate(JIncarceration = case_when(
    H4CJ5 %in% c(996, 998) ~ NA_real_,
    H4CJ5 == 997 ~ 0,
    H4CJ5 %in% seq(1, 95) ~ 1))

table(waves$JIncarceration)
table(is.na(waves$JIncarceration))

"Adult Incarceration"

# 0 = No
# 1 = Yes
# NA = Missing/NA

waves <- waves %>%
  mutate(AIncarceration = case_when(
    is.na(H4CJ6) ~ NaN,
    H4CJ6 %in% c(96, 98) ~ NA_real_,
    H4CJ6 == 97 ~ 0,
    H4CJ6 %in% seq(1,90) ~ 1))

table(waves$AIncarceration)
table(is.na(waves$AIncarceration))


#########################
variables <- as.data.frame(names(waves))
#########################
"Select only the necessary variables:"
attach(waves)
Waves <- waves %>%
  select(AID, H1GI1Y,
         PEducation, Race, Black, Asian, Age, Geography, Gender, Gender.Coded, Hispanic, Citizenship,
         EverSuspend, Divorce, JIncarceration, AIncarceration, FirstIncarcAge, TwoParentHome,
         P1Education, P2Education, SES, JIncarceMonths, AIncarceMonths, AEmployed,
         MotherEmployed, FatherEmployed, Unemployment, MotherHoursWeek, FatherHoursWeek,
         MotherOvertime, FatherOvertime, W1Grade, W1GradeLevel, W2Grade, W2GradeLevel,
         MiddleDropout, HighDropout, FamilySize, Death, HighestGrade, HighGrade15,
         SuspendGrade, ESuspend, MSuspend, HSuspend, Expel, ADPhysicalFight, ADKnifeGun, 
         ADShootStab, ADWeaponSchool, NADLying, NADShoplift, 
         NADStealLess, NADStealMore, AtPCloseMother,
         AtPCloseFather, AtPMotherCare, AtPFatherCare,
         AtSClose, AtSHappy, AtSPartOf, AtSFairly,
         AtDPCigs, AtDPAlcohol, AtDPWeed, APEnglish,
         APMath, APHistory, APScience, AggDelinq,
         NonAggDelinq, AttachParents, AtDelinqPeers, AcadPerform,
         JVictim, AVictim, Victim, CantPayBills, BasicNeeds, HomeAlone, SlapHitKick,
         Touched, HurtFeelings, BioFatherJail, BioMotherJail, FigFatherJail, FigMotherJail,
         BingeDrink, Trauma, Homeless, AStealMore, AStealLess, AStealProperty, ADamageProperty,
         AShootStab, APhysicalFight, AKnifeGun, ASellDrugs, FosterHome,
         JDDriveDrunk, JDDriveHigh, JDSellDrugs, JDIllegalDrugUse, JDCocaineUse, JDIllegalDrugNeedle,
         JDAloneDrugUse, JDFightOnDrugs, JDWeaponOnDrugs, JDStealLess, JDStealMore, JDStealStore,
         JDKnifeGun, JDShootStab, JDLying, JDPhysicalFight, JDSeriousPhysicalFight, JDDamageProperty,
         JDWeaponFight, JDHurtBadly, JDGraffitPaint, AdultStealMore, AdultStealLess, AdultGun, AdultShootStab,
         AdultOtherProperty, AdultPhysicalAttack, AdultAttack, AdultDestroyProperty, AdultSellDrugs,
         AdultStolenCard, ACPhysicalAttack, ACGun, ACShootStab, ACAttack, NACOtherProperty, NACStealLess,
         NACStealMore, AggCrime, NonAggCrime, aceEmotionalNeglect, aceSuicide, aceVictim, JH1FV1,
         aceParentJail)

"Rename some of the variables:"
#New = Old

Waves <- Waves %>%
          rename(BirthYear = H1GI1Y,
                 aceWitnessViolence = JH1FV1)
attach(Waves)

names(Waves)
vars <- data.frame(names(Waves))

Waves <- waves %>%
  select(JIncarceration, Victim, Divorce, Gender, Black, Hispanic, Asian) %>%
  mutate_if(is.double, as.factor)
##############################
### Descriptive Statistics ###
##############################
str(Waves)

"Continuous Variables:" 

# Age

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(Age, na.rm = TRUE),
            sd = sd(Age, na.rm = TRUE),
            median = median(Age, na.rm = TRUE),
            IQR = IQR(Age, na.rm = TRUE))

# Age of First Incarceration
# Lots of NAs since they haven't been arrested yet
summary(Waves$FirstIncarcAge)
sd(FirstIncarcAge, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(FirstIncarcAge, na.rm = TRUE),
            sd = sd(FirstIncarcAge, na.rm = TRUE),
            median = median(FirstIncarcAge, na.rm = TRUE),
            IQR = IQR(FirstIncarcAge, na.rm = TRUE))

# Months Incarcerated as a Juvenile
summary(Waves$JIncarceMonths)
sd(Waves$JIncarceMonths, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(JIncarceMonths, na.rm = TRUE),
            sd = sd(JIncarceMonths, na.rm = TRUE),
            median = median(JIncarceMonths, na.rm = TRUE),
            IQR = IQR(JIncarceMonths, na.rm = TRUE))

# Months Incarcerated as an Adult
summary(Waves$AIncarceMonths)
sd(Waves$AIncarceMonths, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(AIncarceMonths, na.rm = TRUE),
            sd = sd(AIncarceMonths, na.rm = TRUE),
            median = median(AIncarceMonths, na.rm = TRUE),
            IQR = IQR(AIncarceMonths, na.rm = TRUE))

# Mother's Hours worked per Week
summary(Waves$MotherHoursWeek)
sd(Waves$MotherHoursWeek, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(MotherHoursWeek, na.rm = TRUE),
            sd = sd(MotherHoursWeek, na.rm = TRUE),
            median = median(MotherHoursWeek, na.rm = TRUE),
            IQR = IQR(MotherHoursWeek, na.rm = TRUE))

# Father's Hours worked per Week
summary(Waves$FatherHoursWeek)
sd(Waves$FatherHoursWeek, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(FatherHoursWeek, na.rm = TRUE),
            sd = sd(FatherHoursWeek, na.rm = TRUE),
            median = median(FatherHoursWeek, na.rm = TRUE),
            IQR = IQR(FatherHoursWeek, na.rm = TRUE))

# Highest Grade Completed Capped
summary(HighGrade15)
sd(Waves$HighGrade15, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(HighGrade15, na.rm = TRUE),
            sd = sd(HighGrade15, na.rm = TRUE),
            median = median(HighGrade15, na.rm = TRUE),
            IQR = IQR(HighGrade15, na.rm = TRUE))

# Highest Grade Completed Uncapped
summary(HighestGrade)
sd(Waves$HighestGrade, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(HighestGrade, na.rm = TRUE),
            sd = sd(HighestGrade, na.rm = TRUE),
            median = median(HighestGrade, na.rm = TRUE),
            IQR = IQR(HighestGrade, na.rm = TRUE))

# Grade of Last Suspension
summary(SuspendGrade)
sd(Waves$SuspendGrade, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(SuspendGrade, na.rm = TRUE),
            sd = sd(SuspendGrade, na.rm = TRUE),
            median = median(SuspendGrade, na.rm = TRUE),
            IQR = IQR(SuspendGrade, na.rm = TRUE))

# Academic Performance
summary(AcadPerform)
sd(Waves$AcadPerform, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(AcadPerform, na.rm = TRUE),
            sd = sd(AcadPerform, na.rm = TRUE),
            median = median(AcadPerform, na.rm = TRUE),
            IQR = IQR(AcadPerform, na.rm = TRUE))

"Categorical Variables:" 

crossTab <- function(X){
  print(table(X))
  print(round(prop.table(table(X))*100, 1))
  print(table(Gender, X))
  print(round(prop.table(table(Gender, X), margin = 1)*100, 1))
}

# Gender
table(Gender)
round(prop.table(table(Gender))*100,1)

# Race
table(Race)
round(prop.table(table(Race))*100, 1)
table(Gender, Race)
round(prop.table(table(Gender, Race), margin = 1)*100, 1)

# Hispanic
crossTab(Hispanic)

# Citizenship
crossTab(Citizenship)

# Black
crossTab(Black)

# Asian
crossTab(Asian)

# Parent's Education
crossTab(PEducation)

# SES
crossTab(SES)

# Divorce
crossTab(Divorce)

# Two Parent Home
crossTab(TwoParentHome)

# Geography
crossTab(Geography)

# Employed as an Adult
crossTab(AEmployed)

# Mother has a Job
crossTab(MotherEmployed)

# Father has a Job
crossTab(FatherEmployed)

# Father Overtime
crossTab(FatherOvertime)

# Mother Overtime
crossTab(MotherOvertime)

# Unemployment
crossTab(Unemployment)

# Grade @ Wave 1
crossTab(W1Grade)

# Middle School or High School @ Wave 1
crossTab(W1GradeLevel)

# Grade @ Wave 2
crossTab(W2Grade)

# Middle School or High School @ Wave 2
crossTab(W2GradeLevel)

# Did they dropout of middle school?
crossTab(MiddleDropout)

# Did they dropout of high school?
crossTab(HighDropout)

# Family Size
crossTab(FamilySize)

# Has either parent died?
crossTab(Death)

# Have you ever been suspended?
crossTab(EverSuspend)

# Were you suspended in elementary school?
crossTab(ESuspend)

# Were you suspended in middle school?
crossTab(MSuspend)

# Were you suspended in high school?
crossTab(HSuspend)

# Have you ever been expelled from school?
crossTab(Expel)

# Were you a victim of crime in your Youth?
crossTab(JVictim)

# Were you a victim of crime as an Adult?
crossTab(AVictim)

# Were you a victim of crime?
crossTab(Victim)

# Was your family impoverished?
crossTab(CantPayBills)

# Have your parents taken care of your basic needs?
crossTab(BasicNeeds)

# Were you left home alone when you should not have been?
crossTab(HomeAlone)

# Did your parents ever slap/hit/kick you?
crossTab(SlapHitKick)

# Did your parents ever touch you in a sexual way?
crossTab(Touched)

# Did your parents ever hurt your feelings / feel unloved?
crossTab(HurtFeelings)

# Has your biological father ever been in jail?
crossTab(BioFatherJail)

# Has your biological mother ever been in jail?
crossTab(BioMotherJail)

# Has your father figure ever been in jail?
crossTab(FigFatherJail)

# Has your mother figure ever been in jail?
crossTab(FigMotherJail)

# Was your parent a binge drinker?
crossTab(BingeDrink)

# Have you ever been homeless for a week or longer?
crossTab(Homeless)

# Have you stolen more than $50 as an adult in past 12 months?
crossTab(AStealMore)

# Have you stolen less than $50 as an adult in the past 12 months?
crossTab(AStealLess)

# Have you stolen other people's property as an adult in the past 12 months?
crossTab(AStealProperty)

# Have you damaged other people's property as an adult in the past 12 months?
crossTab(ADamageProperty)

# Have you shot/stabbed someone as an adult in the past 12 months?
crossTab(AShootStab)

# Have you gotten into a physical fight as an adult in the past 12 months?
crossTab(APhysicalFight)

# Have you pulled a knife/gun on someone as an adult in the past 12 months?
crossTab(AKnifeGun)

# Have you sold drugs as an adult in the past 12 months?
crossTab(ASellDrugs)

# Juvenile Incarceration
table(JIncarceration)
round(prop.table(table(JIncarceration))*100, 1)
table(Gender, JIncarceration)
round(prop.table(table(Gender, JIncarceration), margin = 1)*100, 1)

# Adult Incarceration
table(AIncarceration)
round(prop.table(table(AIncarceration))*100, 1)
table(Gender, AIncarceration)
round(prop.table(table(Gender, AIncarceration), margin = 1)*100, 1)


####################################
### Confirmatory Factor Analysis ###
####################################

"Both fa and cfa give very similar results in terms of metrics
I included both for reference"

"Confirmatory Factor Analysis:"

# Aggressive Delinquency
AD.model <- 
  "
  # Latent Variables
  AD =~ ADPhysicalFight + ADKnifeGun + ADShootStab + ADWeaponSchool
  "

fit <- cfa(AD.model, data = Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)

# or

AggDelinq <- Waves %>%
  select(ADPhysicalFight, ADKnifeGun, ADShootStab, ADWeaponSchool)

solution <- fa(AggDelinq, nfactors = 1)
solution

# Non-aggressive Delinquency
NAD.model <- 
  "
  # Latent Variables
  NAD =~ NADLying + NADShoplift + NADStealLess + NADStealMore
  "

fit1 <- cfa(NAD.model, data = Waves, std.lv=TRUE)
summary(fit1, fit.measures=TRUE, standardized=TRUE)

asdf <- Waves %>%
  select(NADLying)


# Attachment to Parents
AtParent.model <-
  "
  # Latent Variables
  AParent =~ AtPCloseMother + AtPCloseFather + AtPMotherCare + AtPFatherCare
  "

fit2 <- cfa(AtParent.model, data = Waves, std.lv=TRUE)
summary(fit1, fit.measures=TRUE, standardized=TRUE)


# Attachment to Delinquent Peers
AtDPeers.model <-
  "
  # Latent variables
  ADPeers =~ AtDPCigs + AtDPAlcohol + AtDPWeed
  "

fit3 <- cfa(AtDPeers.model, data = Waves, std.lv=TRUE)
summary(fit3, fit.measures=TRUE, standardized=TRUE)

# Or

DelinqPeers <- Waves %>%
  select(AtDPCigs, AtDPAlcohol, AtDPWeed)

solution <- fa(DelinqPeers, nfactors = 1)
solution

# Academic Performance
AP.model <- 
  "
  APerform =~ APEnglish + APMath + APHistory + APScience
  "

fit4 <- cfa(AP.model, data = Waves, std.lv=TRUE)
summary(fit4, fit.measures=TRUE, standardized=TRUE)

# Or 

Academic <- Waves %>%
  select(APEnglish, APMath, APHistory, APScience)

solution <- fa(Academic, nfactors = 1)
print(solution)
print(solution$scores)

"From the above CFA, it looks like the paper used appropriate constructs
to describe:
  Aggressive Delinquency
  Non-aggressive Delinquency
  Attachment to Parents
  Attachment to Delinquent Peers
  Academic Performance"


###############################
###    Regression Models    ###
###############################

"Logistic Regression Models:"

table(AIncarceration)
table(JIncarceration)

# Adult Incarceration via Age
AIage <- glm(
  AIncarceration ~ Age,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIage)
exp(coef(AIage))
nagelkerke(AIage)

# Juvenile Incarceration via Age
JIAge <- glm(
  JIncarceration ~ Age,
  data = Waves,
  family = binomial(link = "logit"))

summary(JIAge)
nagelkerke(JIAge)
exp(coef(JIAge))

# Adult Incarceration via Juvenile Incarceration

AIJI <- glm(
  AIncarceration ~ JIncarceration + Age + Gender + Race + ESuspend + MSuspend + HSuspend,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIJI)
exp(coef(AIJI))
exp(confint(AIJI))
"Odds of being incarcerated as an adult are 10.27 times higher 
if you were incarcerated as a juvenile."

# Juvenile Incarceration via Hours Worked per Father

JIHoursWeek <- glm(
  JIncarceration ~ FatherHoursWeek,
  data = Waves,
  family = binomial(link = "logit"))

summary(JIHoursWeek)

# Adult Incarceration via Hours Worked

AIHoursWeek <- glm(
  AIncarceration ~ MotherHoursWeek + FatherHoursWeek,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIHoursWeek)
exp(coef(AIHoursWeek))

# Juvenile Incarceration via Overtime

JIOvertime <- glm(
  JIncarceration ~ MotherOvertime + FatherOvertime,
  data = Waves,
  family = binomial(link = "logit"))

summary(JIOvertime)

# Adult Incarceration via Overtime

AIOvertime <- glm(
  AIncarceration ~ MotherOvertime + FatherOvertime,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIOvertime)

# Juvenile Incarceration via Two Parent Home

JITwoParent <- glm(
  JIncarceration ~ TwoParentHome,
  data = waves,
  family = binomial(link = "logit"))

summary(JITwoParent)
exp(coef(JITwoParent))

"Odds of being incarcerated as a juvenile is 2.7 times smaller if
you live in a two parent home."

AIHighGrade <- glm(
  AIncarceration ~ HighestGrade,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIHighGrade)
exp(coef(AIHighGrade))

"For every grade you complete, the odds of being
incarcerated as an adult decrease by 1.4 times."

AIRace <- glm(
  AIncarceration ~ Race,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIRace)
exp(coef(AIRace))

# JIncarceration by Suspension status:

JISuspend <- glm(
  JIncarceration ~ EverSuspend,
  data = Waves,
  family = binomial(link = "logit"))

summary(JISuspend)

"The odds of being incarcerated as a juvenile are 3.9 times 
higher if you have ever been suspended."

# AIncarceration by Suspension status:

AISuspend <- glm(
  AIncarceration ~ EverSuspend,
  data = Waves,
  family = binomial(link = "logit"))

summary(AISuspend)
exp(coef(AISuspend))

"The odds of being incarcerated as an adult are 5.6 times 
higher if you have ever been suspended."

# JIncarceration by Expulsion status:

JIExpel <- glm(
  JIncarceration ~ Expel,
  data = Waves,
  family = binomial(link = "logit"))

summary(JIExpel)
exp(coef(JIExpel))

"The odds of being incarcerated as a juvenile are 3.4 times 
higher if you have ever been expelled than not."

JIAPerform <- glm(
  JIncarceration ~ AcadPerform,
  data = Waves,
  family = binomial(link = "logit"))

summary(JIAPerform)
exp(coef(JIAPerform))

"The odds of being incarcerated as a juvenile are 1.23 times 
lower for every 1 unit increase in your overall academic performance"

# AIncarceration by Victim of Crime status:
AIVictim <- glm(
  AIncarceration ~ Victim, 
  data = Waves,
  family = binomial(link = "logit"))

summary(AIVictim)
exp(coef(AIVictim))

"The odds of being incarcerated as an adult are 4.3 times higher
if you were a victim of crime."

# Gender Only
MODEL <- 
  "
  # Regressions
  AIncarceration ~ Gender.Coded
  "

fit <- cfa(MODEL, data = Waves, std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)

gender <- glm(
  AIncarceration ~ Gender,
  data = Waves,
  family = binomial(link = "logit"))

summary(gender)
exp(coef(gender))

"The odds of being incarcerated as an adult are 4.9 times
higher if you are male than female."

# Being Black

AIBlack <- glm(
  AIncarceration ~ Black,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIBlack)
exp(coef(AIBlack))










#####
"Sources: https://www.cpc.unc.edu/projects/addhealth/faqs/aboutdata/index.html#what-is-the-best"

"Read in the data and necessary libraries:"
setwd("C:/Users/miqui/OneDrive/R Projects/ADD Health")
library(dplyr)
library(lubridate)
library(rcompanion)
library(readr)
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


table(H4CJ24Y)
table(H4CJ24M)
table(waves$JIncarceMonths)
summary(waves$JIncarceMonths)


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


table(H4CJ25Y)
table(H4CJ25M)
table(waves$AIncarceMonths)
summary(waves$AIncarceMonths)

"Calculating the Race:"

waves <- waves %>%
  mutate(Race = case_when(
    H1GI4 == 1 ~ "Hispanic",
    S6B == 1 ~ "Black",
    H1GI6D == 1 ~ "Asian",
    H1GI6C == 1 ~ "Other",
    H1GI6E == 1 ~ "Other",
    H1GI6A == 1 ~ "White"))

waves$Race <- as.factor(waves$Race)
waves <- within(waves, Race <- relevel(Race, ref="White")) # Set "White" as the reference group
table(waves$Race)
table(is.na(waves$Race))
table(PA12)
table(is.na(PA12))

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

"Gender:"

waves <- waves %>%
  mutate(Gender = case_when(
    BIO_SEX3 == 1 ~ "Male",
    BIO_SEX3 == 2 ~ "Female"))

table(waves$Gender)
table(is.na(waves$Gender))

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

"Did you drop out of High School"
waves <- waves %>%
  mutate(HighDropout = case_when(
    is.na(HighGrade15) ~ NaN,
    HighGrade15 %in% c(8,9,10,11) ~ 1,
    TRUE ~ 0))

table(waves$HighDropout)

"Juvenile Incarceration:"

# 0 = No
# 1 = Yes
# NA = Missing/NA

waves <- waves %>%
  mutate(JIncarceration = case_when(
    is.na(H3CJ5) ~ NaN,
    H3CJ5 %in% c(96,98,99) ~ NaN,
    H3CJ5 == 97 ~ 0,
    H3CJ5 %in% seq(1,50) ~ 1))

table(waves$JIncarceration)
table(is.na(waves$JIncarceration))

"Adult Incarceration"

# 0 = No
# 1 = Yes
# NA = Missing/NA

waves <- waves %>%
  mutate(AIncarceration = case_when(
    is.na(H4CJ6) ~ NaN,
    H4CJ6 %in% c(96, 98) ~ NaN,
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
  select(AID, PA10, PA12, S1, S6B, S2, S4, IMONTH, IDAY, IYEAR,
         H1GI1Y, H1GI6B, H1GI11, H1GI14, H1GI21, H1IR12, H1FV3,
         H1GI4, H1JO11, H1DS5, H1DS2, H1TO53, H1DS12, H1DS13, H1DS9,
         H2GI10, H2IR12, H2FV1, H2DS10, H2DS11, H3DA31,
         H3DS8, H3OD4B, BIO_SEX3, H3HR24, H3HR25, H3ID32,
         H3ID30, H3ID29, H3CJ5, H3DS16, H3CJ108A, H3LM7,
         H4DS8, H4CJ9I, H4DS1, H4DS19, H4CJ25M, H4DS5, H4DS6, H4DS2,
         H4WP28, H4CJ20, H4ED2, H4CJ1, H4CJ6, H4CJ17, H4CJ24M, H4LM11,
         PEducation, Race, Age, Geography, Gender, Hispanic, Citizenship, 
         Divorce, JIncarceration, AIncarceration, FirstIncarcAge, TwoParentHome,
         P1Education, P2Education, SES, JIncarceMonths, AIncarceMonths, AEmployed,
         MotherEmployed, FatherEmployed, Unemployment, MotherHoursWeek, FatherHoursWeek,
         MotherOvertime, FatherOvertime, W1Grade, W1GradeLevel, W2Grade, W2GradeLevel,
         Dropout, MiddleDropout, HighDropout, FamilySize, Death, HighestGrade, HighGrade15)
attach(Waves)

"Rename some of the variables:"
#New = Old

Waves <- Waves %>%
          rename(Black = S6B,
                 BirthYear = H1GI1Y)

names(Waves)

###############################
### Descriptive Statistics  ###
###############################
str(Waves)

table(Waves$BIO_SEX3)
table(Race)

"Continuous Variables:" 

# Age

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(Age, na.rm = TRUE),
            sd = sd(Age, na.rm = TRUE),
            median = median(Age, na.rm = TRUE),
            IQR = IQR(Age, na.rm = TRUE))

# Age of First Incarceration

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
summary(Waves$HighGrade15)
sd(Waves$HighGrade15, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(HighGrade15, na.rm = TRUE),
            sd = sd(HighGrade15, na.rm = TRUE),
            median = median(HighGrade15, na.rm = TRUE),
            IQR = IQR(HighGrade15, na.rm = TRUE))

# Highest Grade Completed Uncapped
summary(Waves$HighestGrade)
sd(Waves$HighestGrade, na.rm = TRUE)

Waves %>%
  group_by(Gender) %>%
  summarize(mean = mean(HighestGrade, na.rm = TRUE),
            sd = sd(HighestGrade, na.rm = TRUE),
            median = median(HighestGrade, na.rm = TRUE),
            IQR = IQR(HighestGrade, na.rm = TRUE))

"Categorical Variables:" 

# Gender
table(Gender)
round(prop.table(table(Gender))*100,1)

# Race
table(Race)
round(prop.table(table(Race))*100, 1)
table(Gender, Race)
round(prop.table(table(Gender, Race), margin = 1)*100, 1)

# Hispanic
table(Hispanic)
round(prop.table(table(Hispanic))*100, 1)
table(Gender, Hispanic)
round(prop.table(table(Gender, Hispanic), margin = 1)*100, 1)

# Citizenship
table(Citizenship)
round(prop.table(table(Citizenship))*100, 1)
table(Gender, Citizenship)
round(prop.table(table(Gender, Citizenship), margin = 1)*100, 1)

# Black
table(Black)
round(prop.table(table(Black))*100, 1)
table(Gender, Black)
round(prop.table(table(Gender, Black), margin = 1)*100, 1)

# Parent's Education
table(PEducation)
round(prop.table(table(PEducation))*100, 1)
table(Gender, PEducation)
round(prop.table(table(Gender, PEducation), margin = 1)*100, 1)

# SES
table(SES)
round(prop.table(table(SES))*100, 1)
table(Gender, SES)
round(prop.table(table(Gender, SES), margin = 1)*100, 1)

# Divorce
table(Divorce)
round(prop.table(table(Divorce))*100, 1)
table(Gender, Divorce)
round(prop.table(table(Gender, Divorce), margin = 1)*100, 1)

# Two Parent Home
table(TwoParentHome)
round(prop.table(table(TwoParentHome))*100, 1)
table(Gender, TwoParentHome)
round(prop.table(table(Gender, TwoParentHome), margin = 1)*100, 1)

# Geography
table(Geography)
round(prop.table(table(Geography))*100, 1)
table(Gender, Geography)
round(prop.table(table(Gender, Geography), margin = 1)*100, 1)

# Employed as an Adult
table(AEmployed)
round(prop.table(table(AEmployed))*100, 1)
table(Gender, AEmployed)
round(prop.table(table(Gender, AEmployed), margin = 1)*100, 1)

# Mother has a Job
table(MotherEmployed)
round(prop.table(table(MotherEmployed))*100, 1)
table(Gender, MotherEmployed)
round(prop.table(table(Gender, MotherEmployed), margin = 1)*100, 1)

# Father has a Job
table(FatherEmployed)
round(prop.table(table(FatherEmployed))*100, 1)
table(Gender, FatherEmployed)
round(prop.table(table(Gender, FatherEmployed), margin = 1)*100, 1)

# Father Overtime
table(FatherOvertime)
round(prop.table(table(FatherOvertime))*100, 1)
table(Gender, FatherOvertime)
round(prop.table(table(Gender, FatherOvertime), margin = 1)*100, 1)

# Mother Overtime
table(MotherOvertime)
round(prop.table(table(MotherOvertime))*100, 1)
table(Gender, MotherOvertime)
round(prop.table(table(Gender, MotherOvertime), margin = 1)*100, 1)

# Unemployment
table(Unemployment)
round(prop.table(table(Unemployment))*100, 1)
table(Gender, Unemployment)
round(prop.table(table(Gender, Unemployment), margin = 1)*100, 1)

# Grade @ Wave 1
table(W1Grade)
round(prop.table(table(W1Grade))*100, 1)
table(Gender, W1Grade)
round(prop.table(table(Gender, W1Grade), margin = 1)*100, 1)

# Middle School or High School @ Wave 1
table(W1GradeLevel)
round(prop.table(table(W1GradeLevel))*100, 1)
table(Gender, W1GradeLevel)
round(prop.table(table(Gender, W1GradeLevel), margin = 1)*100, 1)

# Grade @ Wave 2
table(W2Grade)
round(prop.table(table(W2Grade))*100, 1)
table(Gender, W2Grade)
round(prop.table(table(Gender, W2Grade), margin = 1)*100, 1)

# Middle School or High School @ Wave 2
table(W2GradeLevel)
round(prop.table(table(W2GradeLevel))*100, 1)
table(Gender, W2GradeLevel)
round(prop.table(table(Gender, W2GradeLevel), margin = 1)*100, 1)

# Have they ever dropped out of school?
table(Dropout)
round(prop.table(table(Dropout))*100, 1)
table(Gender, Dropout)
round(prop.table(table(Gender, Dropout), margin = 1)*100, 1)

# Did they dropout of middle school?
table(MiddleDropout)
round(prop.table(table(MiddleDropout))*100, 1)
table(Gender, MiddleDropout)
round(prop.table(table(Gender, MiddleDropout), margin = 1)*100, 1)

# Did they dropout of high school?
table(HighDropout)
round(prop.table(table(HighDropout))*100, 1)
table(Gender, HighDropout)
round(prop.table(table(Gender, HighDropout), margin = 1)*100, 1)

# Family Size
table(FamilySize)
round(prop.table(table(FamilySize))*100, 1)
table(Gender, FamilySize)
round(prop.table(table(Gender, FamilySize), margin = 1)*100, 1)

# Has either parent died?
table(Death)
round(prop.table(table(Death))*100, 1)
table(Gender, Death)
round(prop.table(table(Gender, Death), margin = 1)*100, 1)

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
  AIncarceration ~ JIncarceration + Age + Gender + Race,
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



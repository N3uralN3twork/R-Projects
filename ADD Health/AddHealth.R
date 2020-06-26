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

"Calculating the Race:"

waves <- waves %>%
  mutate(Race = case_when(
    H1GI4 == 1 ~ "Hispanic",
    H1GI6B == 1 ~ "Black",
    H1GI6D == 1 ~ "Asian",
    H1GI6C == 1 ~ "NativeAmerican",
    H1GI6E == 1 ~ "Other",
    H1GI6A == 1 ~ "White"))

table(waves$Race)

table(PA12)
table(is.na(PA12))

"Parent's Highest Education:"

# 0 = missing/refused
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
         H2GI10, H2IR12, H2FV1, H2DS10, H2DS11,
         H3DS8, H3OD4B, BIO_SEX3, H3HR24, H3HR25, H3ID32,
         H3ID30, H3ID29, H3CJ5, H3DS16, H3CJ108A, H3LM7,
         H4DS8, H4CJ9I, H4DS1, H4DS19, H4CJ25M, H4DS5, H4DS6, H4DS2,
         H4WP28, H4CJ20, H4ED2, H4CJ1, H4CJ6, H4CJ17, H4CJ24M, H4LM11,
         PEducation, Race, Age, Geography, Gender, Hispanic, Citizenship, 
         Divorce, JIncarceration, AIncarceration, FirstIncarcAge,
         P1Education, P2Education, SES, JIncarceMonths, AIncarceMonths)
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

# Geography
table(Geography)
round(prop.table(table(Geography))*100, 1)
table(Gender, Geography)
round(prop.table(table(Gender, Geography), margin = 1)*100, 1)

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

# Adult Incarceration via Juvenile Incarceration

AIJI <- glm(
  AIncarceration ~ JIncarceration,
  data = Waves,
  family = binomial(link = "logit"))

summary(AIJI)
exp(coef(AIJI))
"Odds of being incarcerated as an adult are 10.27 times higher 
if you were incarcerated as a juvenile."


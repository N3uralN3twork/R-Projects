#### Load Libraries ####
setwd("C:/Users/MatthiasQ.MATTQ/OneDrive/R Projects/SEM in R")
install.packages("lavaan")
install.packages("pbkrtest")
install.packages("semPlot")
require(lavaan)
require(semPlot)
library(foreign)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(dplyr)


#### Load in the Data ####

db = file.choose()
dataset = read.spss(db, to.data.frame=TRUE)
warnings()
summary(dataset)
attach(dataset)

#### Preprocessing ####
#dataset$MSA97 = as.character(datasetMSA97)
dataset$gender <- as.character(dataset$gender)
table(gender) #3,393 0's and 3,342 1's
table(race)

# Binarizing variables
dataset <- dataset %>%
  mutate(GR = case_when(
    gender == 0 & race == 1 ~ "01",
    gender == 0 & race == 2 ~ "02",
    gender == 0 & race == 3 ~ "03",
    gender == 0 & race == 4 ~ "04",
    gender == 1 & race == 1 ~ "11",
    gender == 1 & race == 2 ~ "12",
    gender == 1 & race == 3 ~ "13",
    gender == 1 & race == 4 ~ "14"
  ))

table(dataset$GR)

dataset <- dataset %>%
  mutate(Aincarceration = case_when(
    adultincarceration == 0 ~ "0",
    adultincarceration != 0 ~ "1"))

dataset <- dataset %>%
  mutate(Jincarceration = case_when(
    juvenileincarceration == 0 ~ "0",
    juvenileincarceration != 0 ~ "1"))

dataset <- dataset %>%
  mutate(ESuspend = case_when(
    elementarysuspend == 0 ~ "0",
    elementarysuspend >= 1 ~ "1"))

dataset <- dataset %>%
  mutate(MSuspend = case_when(
    middlesuspend == 0 ~ "0",
    middlesuspend >= 1 ~ "1"
  ))

dataset <- dataset %>%
  mutate(HSuspend = case_when(
    highsuspend == 0 ~ "0",
    highsuspend >= 1 ~ "1"))

attach(dataset)
variables <- as.data.frame(names(dataset))





#### Full Model ####
FullModel <- 
"
#######Latent Variables
TRAUMACRIME =~ jail 
    + victim + gunshotless12 + gunshot12to18

TRAUMAPOVERTY =~  unemployment + homeless 
    + poverty1997 + poverty1998 + poverty1999 + poverty2000 + poverty2001 + poverty2002
    
TRAUMAFAMILY =~ death + hospitalization + divorce

DELINQUENCY =~ juveniledestroyproperty + juvenilestealless
         + juvenilestealmore + juvenileotherproperty + juvenileattack + juvenileselldrugs
         
CRIME =~ adultgun + adultdestroyproperty + adultstealless + adultstealmore + adultotherproperty
      + adultattack + adultselldrugs

ELEMSCHOOL =~
     + elementmiddledropout 
     + elementarysuspend + middlesuspend

HIGHSCHOOL =~ 
     + highdropout 
     + highsuspend
     
#######Correlations

TRAUMACRIME ~~ TRAUMAPOVERTY 
TRAUMACRIME ~~ TRAUMAFAMILY
TRAUMAPOVERTY ~~ TRAUMAFAMILY
DELINQUENCY ~~ CRIME

#######Regressions


adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + juvenileincarceration + CRIME
      + age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship + gender
      
HIGHSCHOOL ~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + DELINQUENCY + juvenileincarceration
      + age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship + gender
   
ELEMSCHOOL ~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + juvenileincarceration
      + age + twoparenthome + black + hispanic + geography97 + SES + citizenship + gender

"
#### END FULL MODEL ####

# There seems to be a problem with the elementary/middle school latent variable
# Model won't converge if SES is included in the regression
# Highest correlation in SES is 0.2

fit1 <- cfa(FullModel, data=dataset, std.lv=TRUE)
summary(fit1, standardized=TRUE, fit.measures=TRUE)


# Checking the correlations
corrs = lavInspect(fit1, what="cor.all")



#### Gender Model ####
GenderModel <- 
"
#######Latent Variables
TRAUMACRIME =~ jail 
    + victim + gunshotless12 + gunshot12to18

TRAUMAPOVERTY =~  unemployment + homeless 
    + poverty1997 + poverty1998 + poverty1999 + poverty2000 + poverty2001 + poverty2002
    
TRAUMAFAMILY =~ death + hospitalization + divorce

DELINQUENCY =~ juveniledestroyproperty + juvenilestealless
         + juvenilestealmore + juvenileotherproperty + juvenileattack + juvenileselldrugs
         
CRIME =~ adultgun + adultdestroyproperty + adultstealless + adultstealmore + adultotherproperty
      + adultattack + adultselldrugs

ELEMSCHOOL =~
     + elementmiddledropout 
     + elementarysuspend + middlesuspend

HIGHSCHOOL =~ 
     + highdropout 
     + highsuspend
     
#######Correlations

TRAUMACRIME ~~ TRAUMAPOVERTY 
TRAUMACRIME ~~ TRAUMAFAMILY
TRAUMAPOVERTY ~~ TRAUMAFAMILY
DELINQUENCY ~~ CRIME

#######Regressions


adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + juvenileincarceration + CRIME
      + age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship
      
HIGHSCHOOL ~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + DELINQUENCY + juvenileincarceration
      + age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship
   
ELEMSCHOOL ~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + juvenileincarceration
      + age + twoparenthome + black + hispanic + geography97 + SES + citizenship
"
#### END Gender MODEL ####

# There seems to be a problem with the elementary/middle school latent variable
# Model won't converge if SES is included in the regression
# Highest correlation in SES is 0.2

fit1 <- cfa(GenderModel, data=dataset, std.lv=TRUE, group="gender")
summary(fit1, standardized=TRUE, fit.measures=TRUE)



#### Race*Gender MODEL####

# 4 levels in the race variable
# 2 levels in the gender variable
# Total models = 4*2 = 8 models

table(gender, race) # Num. obs. per pair

#### Gender* Race MODEL####
GenderRaceModel <- 
  "
#######Latent Variables
TRAUMACRIME =~ jail 
    + victim + gunshotless12 + gunshot12to18

TRAUMAPOVERTY =~  unemployment + homeless 
    + poverty1997 + poverty1998 + poverty1999 + poverty2000 + poverty2001 + poverty2002
    
TRAUMAFAMILY =~ death + hospitalization + divorce

DELINQUENCY =~ juveniledestroyproperty + juvenilestealless
         + juvenilestealmore + juvenileotherproperty + juvenileattack + juvenileselldrugs
         
CRIME =~ adultgun + adultdestroyproperty + adultstealless + adultstealmore + adultotherproperty
      + adultattack + adultselldrugs

ELEMSCHOOL =~
     + elementmiddledropout 
     + elementarysuspend + middlesuspend

HIGHSCHOOL =~ 
     + highdropout 
     + highsuspend
     
#######Covariances

TRAUMACRIME ~~ TRAUMAPOVERTY 
TRAUMACRIME ~~ TRAUMAFAMILY
TRAUMAPOVERTY ~~ TRAUMAFAMILY
DELINQUENCY ~~ CRIME
ELEMSCHOOL ~~ HIGHSCHOOL

#######Regressions


Aincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + CRIME
                  + age + twoparenthome + geography97 + SES + citizenship

      
HIGHSCHOOL ~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + DELINQUENCY + ELEMSCHOOL 
             + age + twoparenthome + geography97 + SES + citizenship

   
ELEMSCHOOL ~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + DELINQUENCY
             + twoparenthome + geography97 + SES + citizenship

"
#### END Gender*Race MODEL ####


fit1 <- cfa(GenderRaceModel, data=test, std.lv=TRUE, missing="fiml", std.ov=TRUE) # Standardize latent and observed variables
summary(fit1, standardized=TRUE, fit.measures=TRUE)


# Subsets of the full dataset:
test <- filter(dataset, gender==0, race==1) # 1621, Works
test <- filter(dataset, gender==0, race==2) # 957, Works
test <- filter(dataset, gender==0, race==3) # 706, Works if you exclude jincarceration from Aincareration and Highschool regressions
test <- filter(dataset, gender==1, race==1) # 1656, Doesn't work, no convergence
test <- filter(dataset, gender==1, race==2) # 871, Works
test <- filter(dataset, gender==1, race==3) # 701, Works
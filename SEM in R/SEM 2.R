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


#### Load in the Data ####

db = file.choose()
dataset = read.spss(db, to.data.frame=TRUE)
warnings()
summary(dataset)
attach(dataset)

#### Preprocessing ####
dataset$MSA97 = as.character(dataset$MSA97)
dataset$gender <- as.character(dataset$gender)
table(gender) #3,393 0's and 3,342 1's
table(race)
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

ELEMSCHOOL =~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + DELINQUENCY
     + elementmiddledropout 
     + elementarysuspend + middlesuspend

HIGHSCHOOL =~ 
     + highdropout 
     + highsuspend
     + SES
     + citizenship
     
#######Correlations

#TRAUMACRIME ~~ TRAUMAPOVERTY 

#######Regressions


adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + juvenileincarceration + CRIME
      + age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship + gender
      
HIGHSCHOOL ~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + DELINQUENCY + juvenileincarceration + CRIME
      + age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship + gender
    
ELEMSCHOOL ~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + juvenileincarceration
      + age + twoparenthome + black + hispanic + geography97 + citizenship + gender
"

# There seems to be a problem with the elementary/middle school latent variable
# Model won't converge if SES is included in the regression
# Highest correlation in SES is 0.2

fit1 <- cfa(FullModel, data=dataset, std.lv=TRUE)
summary(fit1, standardized=TRUE, fit.measures=TRUE)

# Checking the correlations
corrs = lavInspect(fit1, what="cor.all")




#### By Gender Model ####
GenderModel <-
"
#######Latent
TRAUMACRIME =~ jail 
    + victim + gunshotless12 + gunshot12to18

TRAUMAPOVERTY =~  unemployment + homeless 
    + poverty1997 + poverty1998 + poverty 1999 + poverty2000 + poverty2001 + poverty2002
    
TRAUMAFAMILY =~ death + hospitalization + divorce

DELINQUENCY =~ juveniledestroyproperty + juvenilestealless
         + juvenilestealmore + juvenileotherproperty + juvenileattack + juvenileselldrugs
         
CRIME =~ adultgun + adultdestroyproperty + adultstealless + adultstealmore + adultotherproperty
      + adultattack + adultselldrugs

ELEMSCHOOL  =~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + DELINQUENCY
     + elementmiddledropout 
     + elementarysuspend + middlesuspend
     + SES 
     + citizenship
     
HIGHSCHOOL  =~ 
     + highdropout 
     + highsuspend  
     + SES
     + citizenship
     
#######Correlations

#TRAUMACRIME ~~ TRAUMAPOVERTY 

#######Regressions

adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + CRIME + juvenileincarceration
      +  age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship

HIGHSCHOOL ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL  + juvenileincarceration
      +  age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship

ELEMSCHOOL ~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + juvenileincarceration + geography97 + age + twoparenthome + black
"

# There seems to be a problem with the elementary/middle school latent variable
# Model won't converge if its included in the regressions
fit2 <- cfa(GenderModel, data=dataset, std.lv=TRUE, group = "gender")
summary(fit2, standardized=TRUE, fit.measures=TRUE)








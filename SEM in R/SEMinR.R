### setup environment and libraries

setwd("C:/Users/MatthiasQ.MATTQ/OneDrive/R Projects/SEM in R")
install.packages("lavaan")
install.packages("pbkrtest")
install.packages("semPlot")
require(lavaan)
require("semPlot")
library(foreign)
library(tidyverse)
library(tidygraph)
library(ggraph)

### get spss dataset

db = file.choose()
dataset = read.spss(db, to.data.frame=TRUE)
warnings()
head(dataset)
tail(dataset)
summary(dataset)
attach(dataset)

dataset$MSA97 = as.character(dataset$MSA97)
dataset$gender <- as.character(dataset$gender)
table(dataset$gender) #3,393 0's and 3,342 1's


######  MODEL ###### 
Model1 <- '
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
     + SES + citizenship
        

HIGHSCHOOL  =~ 
     + highdropout 
     + highsuspend 
     + SES
     + citizenship


#######Correlations

#TRAUMACRIME ~~ TRAUMAPOVERTY 

#######Regressions


adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + DELINQUENCY + juvenileincarceration + CRIME
      +  age +  twoparenthome  + black + hispanic  + geography97 + SES + citizenship
   
'
#############END MODEL#############
# By group variable cannot be included in the model, otherwise, its variance will be 0.
# Only way to get by group is to remove the variable from the regression
fit1_gender <- cfa(Model1, data = dataset , std.lv = TRUE, group = "gender") # By GENDER
fitted(fit1_gender)
summary(fit1_gender,standardized=TRUE, fit.measures = TRUE)

# Path Analysis Graphs
semPaths(fit1_gender, whatlabels="par", "std", layout="circle", sizeMan = 5, edge.label.cex = 0.8, line=3,  label.prop = 1, curve = 0.5, intercepts = FALSE, borders = TRUE, sizeInt = 1)
semPaths(fit1_gender, whatlabels="par", "std", layout="spring", sizeMan = 5, edge.label.cex = 0.8, line=3,  label.prop = 1, curve = 0.5, intercepts = FALSE, borders = TRUE, sizeInt = 1)




##### Highschool Model ###### 

HSModel <- '
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

ELEMSCHOOL  =~ TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + DELINQUENCY
     + elementmiddledropout 
     + elementarysuspend + middlesuspend
     + SES + citizenship
       
HIGHSCHOOL  =~ 
     + highdropout 
     + highsuspend
     + SES
     + citizenship
     
#######Correlations

#TRAUMACRIME ~~ TRAUMAPOVERTY 

#######Regressions


adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + DELINQUENCY + juvenileincarceration + CRIME
      +  age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship

HIGHSCHOOL ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL  + DELINQUENCY + juvenileincarceration + CRIME
      +  age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship
   
'

##### End Highschool Model #####

fit2_gender <- cfa(HSModel, data=dataset, std.lv=TRUE, group="gender")
fitted(fit2_gender)
summary(fit2_gender,standardized=TRUE, fit.measures=TRUE)

semPaths(fit2_gender, whatlabels="par", "std", layout="circle",sizeMan = 5, edge.label.cex = 0.8,line=3,  label.prop = 1, curve = 0.5, intercepts = FALSE, borders = TRUE, sizeInt = 1)



##### Elementary/Middle School Model #####


MSModel <- '
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
     + SES + citizenship
     
HIGHSCHOOL  =~ 
     + highdropout 
     + highsuspend  
     + SES
     + citizenship
     
#######Correlations

#TRAUMACRIME ~~ TRAUMAPOVERTY 

#######Regressions

adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + CRIME + juvenileincarceration
      +  age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship + gender

HIGHSCHOOL ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL  + juvenileincarceration
      +  age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship + gender
   
ELEMSCHOOL ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY  + juvenileincarceration 
      +  age +  twoparenthome + black + hispanic  + geography97 + SES + citizenship + gender
'

##### End Middleschool Model #####
# Converges when you don't ask to compute by group and include gender in the regressions
# Does not compute the standard errors when asked to compute by group
fit3 <- cfa(MSModel, data=dataset, std.lv=TRUE)
fitted(fit3)
summary(fit3, standardized=TRUE, fit.measures=TRUE)

semPaths(fit3, whatlabels="par", "std", layout="circle", sizeMan = 5, edge.label.cex = 0.8,line=3,  label.prop = 1, curve = 0.5, intercepts = FALSE, borders = TRUE, sizeInt = 1)










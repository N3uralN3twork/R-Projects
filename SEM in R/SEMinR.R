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


adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + juvenileincarceration + CRIME
      +  age +  twoparenthome + gender +black + hispanic  + geography97 + SES + citizenship
   
'
#############END MODEL

fit1 <- cfa(Model1, data = dataset, std.lv=TRUE)
fitted(fit1)
summary(fit1,standardized=TRUE, fit.measures = TRUE)

semPaths(fit1, whatlabels="par","std", layout="circle",sizeMan = 5, edge.label.cex = 0.8,line=3,  label.prop = 1, curve = 0.5, intercepts = FALSE, borders = TRUE, sizeInt = 1)




##### Highschool Model ###### 

HSModel <- '
#######Latent Variables
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


adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + juvenileincarceration + CRIME
      +  age +  twoparenthome + gender +black + hispanic  + geography97 

HIGHSCHOOL ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL  + juvenileincarceration + CRIME
      +  age +  twoparenthome + gender +black + hispanic  + geography97 + SES + citizenship
   
'

##### End Highschool Model #####

fit2 <- cfa(HSModel, data = dataset, std.lv=TRUE)
fitted(fit2)
summary(fit2,standardized=TRUE, fit.measures = TRUE)

semPaths(fit2, whatlabels="par","std", layout="circle",sizeMan = 5, edge.label.cex = 0.8,line=3,  label.prop = 1, curve = 0.5, intercepts = FALSE, borders = TRUE, sizeInt = 1)



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

adultincarceration ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL + HIGHSCHOOL + juvenileincarceration + CRIME
      +  age +  twoparenthome + gender +black + hispanic  + geography97 + SES + citizenship

HIGHSCHOOL ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY + ELEMSCHOOL  + juvenileincarceration + CRIME
      +  age +  twoparenthome + gender +black + hispanic  + geography97 + SES + citizenship
   
ELEMSCHOOL ~  TRAUMACRIME + TRAUMAPOVERTY + TRAUMAFAMILY  + juvenileincarceration + CRIME
      +  age +  twoparenthome + gender +black + hispanic  + geography97 + SES + citizenship
'

##### End Middleschool Model #####

fit3 <- cfa(MSModel, data = dataset, std.lv=TRUE)
fitted(fit3)
summary(fit3, standardized=TRUE, fit.measures = TRUE)

semPaths(fit3, whatlabels="par","std", layout="circle",sizeMan = 5, edge.label.cex = 0.8,line=3,  label.prop = 1, curve = 0.5, intercepts = FALSE, borders = TRUE, sizeInt = 1)


















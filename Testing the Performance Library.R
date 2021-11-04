setwd("C:/Users/miqui/OneDrive/CSU Classes/Categorical Data Analysis")
library(performance)
library(readr)
library(dplyr)
library(lavaan)
library(foreign)


data("mtcars")

horseshoecrabs <- read_csv("Datasets/Horseshoe Crabs.csv", col_names =FALSE)

colnames(horseshoecrabs) <- c("color", "spine", "width", "satell", "weight")

horseshoecrabs <- horseshoecrabs %>% 
                  mutate(satell = replace(satell, satell > 0, 1))

head(horseshoecrabs, 5)

crabs.fit1 <- glm(satell ~ weight,
                  family = binomial(link = "logit"),
                  data = horseshoecrabs)

summary(crabs.fit1)

# Using the performance library:
check_model(crabs.fit1)

model_performance(crabs.fit1, metrics = "all")


# a 2-dimensional example with K-Means:
x <- rbind(
  matrix(rnorm(100, sd = 0.3), ncol = 2),
  matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)
)
colnames(x) <- c("x", "y")
KMeans2 <- kmeans(x, 2)
KMeans3 <- kmeans(x, 3)
model_performance(model)


# Model accuracy:
modelOne <- lm(mpg ~ wt + cyl, data = mtcars)
performance_accuracy(modelOne,
                     method = "cv",
                     k = 3,
                     verbose = TRUE)

# Logistic Model AUC:
modelTwo <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
performance_accuracy(modelTwo,
                     method = "boot",
                     n = 100,
                     verbose = TRUE)

# Hosmer-Lemeshow goodness-of-fit test:
performance_hosmer(modelTwo)


# Comparing models:
set.seed(1234)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
lm4 <- lm(Sepal.Length ~ Species * Sepal.Width + Petal.Length + Petal.Width, data = iris)

compare_performance(lm1, lm2, lm3, lm4)

# Plotting different models performance:
plot(compare_performance(lm1, lm2, lm3, lm4, rank = TRUE))


# Performance metrics for a lavaan model:

### get spss dataset
db = file.choose()
dataset = read.spss(db, to.data.frame=TRUE)
dataset$MSA97 = as.character(dataset$MSA97)
dataset$gender <- as.character(dataset$gender)

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

lavaOne <- cfa(Model1, data = dataset, std.lv = TRUE)
summary(lavaOne, standardized=TRUE, fit.measures = TRUE)

lavaPerf <- model_performance(lavaOne)


lavaPerf






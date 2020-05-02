"Ridge Regression and the Lasso:
Date: 3 November, 2019
Source: https://www.r-bloggers.com/ridge-regression-and-the-lasso/

The Lasso method has the ability to choose important coefficients, like 
stepwise selection.

When lambda approaches 0, all of the predictors are returned
When lambda approaches infinity, the predictors go to 0"

swiss <- datasets::swiss
#Define x and y
x <- model.matrix(Fertility ~., data = swiss)[,-1]
y <- swiss$Fertility
#Define lambda to use
lambda <- 10^seq(from = 10, to = -2, length = 100)



#To prove the above statements that we made about lambda:
library(glmnet)
set.seed(489)
train <- sample(1:nrow(x), nrow(x)/2)
test  <- (-train)
ytest <- y[test]

#Fit your models
#Ordinary Least Squares
SimpleLinear <- lm(Fertility ~.,
                   data = swiss)
SwissLM <- lm(Fertility ~.,
              data = swiss,
              subset = train)

summary(SimpleLinear)
#Agriculture = -0.17211
#Examination = -0.25801
#Education = -0.87094
#Catholic = 0.104012
#Infant Mortality = 1.07705

#Ridge Regression
Ridge <- glmnet(x, y, alpha = 0, #0 = ridge
                lambda = lambda)
RidgeModel <- glmnet(x[train, ], y[train], alpha = 0, lambda = lambda)

#https://stackoverflow.com/questions/49804793/trying-to-use-exact-true-feature-in-r-glmnet
predict(Ridge, s = 0, x = x, y = y,
        type = 'coefficients', exact = TRUE)[1:6,]
#In case you get an error about x and y being specified
#Agriculture = -0.1721983
#Examination = -0.2590771
#Education = -0.87053
#Catholic = 0.1040307
#Infant Mortality = 1.0770215

#Notice how the coefficients themselves are practically the same
#I wonder why


#Just like you saw with Michelle's book work, we can choose the best lambda
#value instead of relying on a bunch of lambdas
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)

bestLambda <- cv.out$lambda.min
print(bestLambda) #1.386317
#I wonder how the best lambda value is chosen, like which metric it uses

#Predictions
RidgePreds <- predict(RidgeModel, s = bestLambda, newx = x[test, ])
SwissPred <- predict(SwissLM, newdata = swiss[test, ])


#Checking model performance with MSE:
mean((SwissPred - ytest)^2)
#106.0087
mean((RidgePreds-ytest)^2)
#93.02157


#Lasso Regression:
#Lasso takes the absolute value of the coefficient estimates
Lasso <- glmnet(x[train, ], y[train], alpha = 1, #Specify lasso
                lambda = lambda)
LassoPreds <- predict(Lasso, s = bestLambda, newx = x[test, ])
#MSE
mean((LassoPreds-ytest)^2)
#124.1039


#It looks like our ridge model performed the best out of the three models.

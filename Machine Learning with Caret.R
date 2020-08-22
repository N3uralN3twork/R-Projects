library(caret)
library(mlbench)
data("Sonar")

inTrain <- createDataPartition(y = Sonar$Class,
                               p = 0.75,
                               list = FALSE)
training <- Sonar[inTrain, ]
testing  <- Sonar[-inTrain, ]

nrow(training)
nrow(testing)

trControl <- trainControl(method = "repeatedcv",
                          repeats = 2,
                          preProcOptions = c("center", "scale"),
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

#Partial Least Squares
pls.fit <- train(Class ~.,
                 data = training,
                 method = "pls",
                 trControl = trControl,
                 metric = "ROC",
                 tuneLength = 15)
pls.fit

varImp(pls.fit)
ggplot(pls.fit)

#Regularized Discriminant Analysis
rda.fit <- train(Class ~.,
                 data = training,
                 method = "rda",
                 trControl = trControl,
                 metric = "ROC",
                 tuneGrid = data.frame(gamma = (0:4)/4, lambda = 0.75))
rda.fit

ggplot(varImp(rda.fit))
#Predictions
plsClasses <- predict(pls.fit, newdata = testing)

rdaClasses <- predict(rda.fit, newdata = testing)

#Confusion Matrix
"Partial Least Squares"
confusionMatrix(data = plsClasses, reference = testing$Class)

"Regularized Discriminant Analysis"
confusionMatrix(data = rdaClasses, reference = testing$Class)


resamps <- resamples(list(pls = pls.fit,  rda = rda.fit))
summary(resamps)


xyplot(resamps, what = "BlandAltman")


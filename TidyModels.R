library(tidymodels)
library(tidyverse)
data("ames")

ames <- ames %>%
          mutate(Sale_Price = log10(Sale_Price))

set.seed(1234)
inTrain <- initial_split(ames, prop = 0.80)
print(inTrain)

ames_train <- training(inTrain)
ames_test <- testing(inTrain)



lmModel <- linear_reg(mode = "regression") %>%
              set_engine("lm")


# Workflows:
lmWorkflow <- 
  workflow() %>%
  add_model(lmModel) %>%
  add_formula(Sale_Price ~ Longitude + Latitude)

lmWorkflow


lmFit <- lmWorkflow %>% fit(ames_train)

summary(lmFit$fit$fit$fit)

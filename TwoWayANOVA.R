library(dplyr)
setwd("C:/Users/MatthiasQ.MATTQ/Desktop/Python Projects/Sci-kit Learn")

sodium <- read_csv("Datasets/SodiumIntake.csv", col_names = TRUE)
sodium <- as.data.frame(sodium)
str(sodium)
names(sodium)

sodium$Instructor <- as.factor(sodium$Instructor)
sodium$Supplement <- as.factor(sodium$Supplement)

res.2aov <- aov(Sodium ~ Instructor*Supplement,
                data = sodium)

summary(res.2aov)

TukeyHSD(res.2aov, which = "Supplement")

plot(res.2aov)

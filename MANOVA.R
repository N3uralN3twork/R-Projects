"
One-Way MANOVA in R:
Sources:
  https://www.datanovia.com/en/lessons/one-way-manova-in-r/#prerequisites

Notes:
  MANOVA is ANOVA with 2 or more DV's
  Steps in One-Way MANOVA:
    1. Create a new composite variable that is a linear combo of all the DV's
    2. Compare the mean values of this new variable between groups
    
"
library(tidyverse)
library(rstatix)
library(performance)
library(car)
library(broom)

"Data Preparation:

This program uses the built-in Iris dataset."
iris2 <- iris %>%
  select(Sepal.Length, Petal.Length, Species) %>%
  add_column(ID = 1:nrow(iris), .before = 1) # .before = where to place the new column
head(iris2)

"Summary Statistics:"
iris2 %>%
  group_by(Species) %>%
  get_summary_stats(Sepal.Length, Petal.Length, type = "mean_sd")
# That's pretty neat

"
Checking our Assumptions:

1. Adequate sample size == 20 per cell (from PSY 611)
2. Independent observations
3. Absence of univariate/multivariate outliers
4. Multivariate normality
5. Absence of multicollinearity
6. Linearity between DV's
7. Homogeneity of variances via Levene's test
8. Homogeneity of variance-covariance matrices via Box's M test
"

"
1. Adequate sample sizes:
All of the cells in our grouping variable have > 20 observations.
"

"
2. Just assume that the observations are i.i.d. and from a random sample for now.
"

"
3. Absence of univariate/multivariate outliers:
Univariate outliers => 'identify_outliers' function from 'rstatix' package.
'Extreme Outliers' = 1.5*IQR rule, according to the source code.

Multivariate outliers => Mahalanobis Distance, just like PSY 611
Larger MD, more unusual the row is
"
iris2 %>%
  group_by(Species) %>%
  identify_outliers(Sepal.Length)  # Sepal Length outliers

iris2 %>%
  group_by(Species) %>%
  identify_outliers(Petal.Length)  # Petal Length outliers

# There don't appear to be any univariate outliers of concern.


# Compute distance by groups and filter outliers
# Use the minus sign '-' to omit unnecessary columns.
iris2 %>%
  group_by(Species) %>%
  mahalanobis_distance(-ID) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

# No multivariate outliers to be found!

"4. Univariate/Multivariate Normality:
Apparently, MANOVA is robust to deviations from normality, but I'll check anyways.
Mutivariate normality => 'mshapiro_test()'
"
iris2 %>%
  group_by(Species) %>%
  shapiro_test(Sepal.Length, Petal.Length)

# Both Sepal and Petal lengths appear to be normally distributed for each group of Species.

iris2 %>%
  select(Sepal.Length, Petal.Length) %>%
  mshapiro_test()

# Non-significant p-value means we can assume multivariate normality.

"
5. Multicollinearity:
A correlation above 0.90 is an indication of multicollinearity, which is problematic for MANOVA.
For 3 or more variables, use the 'cor_mat()' function instead.
In the case of multicollinearity, you could remove one of the DV's that is strongly represented by the other, similar DV.
"

iris2 %>%
  cor_test(Sepal.Length, Petal.Length)

# Correlation is significant, but just barely below the threshold for multicollinearity.

"
7. Homogeneity of Variance via Levene's test:
One-Way MANOVA assumes that there are equal variances between groups.
"
leveneTest(Sepal.Length ~ Species, data=iris2)
leveneTest(Petal.Length ~ Species, data=iris2)

"
8. Homogeneity of Variance-Covariance matrix:
Implmented via Box's M test.
If significant, then assumption has been violated.
"
box_m(iris2[ , c("Sepal.Length", "Petal.Length")], group = iris2$Species)


"
Computing the One-Way MANOVA:
"
model <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris2)
summary(model, test = "Roy")





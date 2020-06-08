#########################################################
###           1. Set the working directory            ###
#########################################################
import os
abspath = os.path.abspath("C:/Users/miqui/OneDrive/R Projects/SEM in R")
os.chdir(abspath)
#########################################################
###           2. Import Data and Libraries            ###
#########################################################
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
import statsmodels.api as sm

dataset = pd.read_csv("Subset.csv", header=0)

dataset.shape
dataset.columns
dataset.info()
#########################################################
###           3. Data Cleaning                        ###
#########################################################

#########################################################
###           4. Training Split                       ###
#########################################################
X = dataset.drop(["Aincarceration"], axis=1)
X = sm.add_constant(X) # Don't forget to include the intercept
y = dataset["Aincarceration"]
y.value_counts()

#########################################################
###           5. Logistic Regression                  ###
#########################################################

full = sm.Logit(y, X).fit()

dir(full)
print(full.summary())

# Odds Ratios:
print(np.exp(full.params))

# Coefficient p-values:
print(full.pvalues)

test = pd.concat([np.exp(full.params), full.pvalues], axis=1)
test.columns = ["OddsRatio", "p-value"]





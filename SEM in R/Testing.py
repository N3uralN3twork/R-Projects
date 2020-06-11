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
import statsmodels.api as sm
import statsmodels.formula.api as smf

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
X = dataset.drop(["Aincarceration", "GR"], axis=1)
X = sm.add_constant(X) # Don't forget to include the intercept
y = dataset["Aincarceration"]
y.value_counts()

formula = """Aincarceration ~ jail + victim + gunshotless12 + gunshot12to18 + unemployment + homeless +
    poverty1997 + poverty1998 + poverty1999 + poverty2000 + poverty2001 + poverty2002 + 
    death + hospitalization + divorce + juveniledestroyproperty + juvenilestealless +
    juvenilestealmore + juvenileotherproperty + juvenileattack + juvenileselldrugs + 
    adultgun + adultdestroyproperty + adultstealless + adultstealmore + adultotherproperty +
    adultattack + adultselldrugs + elementarysuspend + middlesuspend + elementmiddledropout +
    highsuspend + highdropout + Jincarceration + highgrade15 + gender + black + hispanic + age + 
    twoparenthome + SES + citizenship + geography97"""
#########################################################
###           5. Logistic Regression                  ###
#########################################################

# For all participants
full = sm.Logit(y, X).fit()

fit = smf.glm(formula=formula,
              data=dataset,
              family=sm.families.Binomial()).fit()

# For females only
fit = smf.glm(formula=formula,
              data= dataset[dataset.gender == 0],
              family=sm.families.Binomial()).fit()

WF = smf.glm(formula = formula,
             data = dataset[dataset.GR == 1],
             family = sm.families.Binomial()).fit()
#########################################################
###           6. Results                              ###
#########################################################
dir(full)
print(full.summary())
print(full.conf_int())

dir(fit)
print(fit.summary())
print(fit.summary2())
# Odds Ratios:
print(np.exp(full.params))
print(np.exp(fit.params))

# Coefficient p-values:
print(full.pvalues)
print(fit.pvalues)

results = pd.concat([np.exp(full.params), full.pvalues, np.exp(full.conf_int())], axis=1)
results.columns = ["OddsRatio", "p-value", "Lower", "Upper"]
print(results)

results = pd.concat([np.exp(fit.params), fit.pvalues, np.exp(fit.conf_int())], axis=1)
results.columns = ["OddsRatio", "p-value", "Lower", "Upper"]
print(results)


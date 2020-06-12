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

clean = pd.read_csv("Subset.csv", header=0) # Run this for ease of use
dataset = pd.read_spss("NLS 2 no formats.sav")


dataset.shape
dataset.columns
dataset.info()
#########################################################
###           3. Feature Engineering/Selection        ###
#########################################################

# Select only the variables we want:
dataset = dataset[['adultincarceration', 'jail', 'victim', 'gunshotless12',
       'gunshot12to18', 'unemployment', 'homeless', 'poverty1997',
       'poverty1998', 'poverty1999', 'poverty2000', 'poverty2001',
       'poverty2002', 'death', 'hospitalization', 'divorce',
       'juveniledestroyproperty', 'juvenilestealless', 'juvenilestealmore',
       'juvenileotherproperty', 'juvenileattack', 'juvenileselldrugs',
       'adultgun', 'adultdestroyproperty', 'adultstealless', 'adultstealmore',
       'adultotherproperty', 'adultattack', 'adultselldrugs',
       'elementarysuspend', 'middlesuspend', 'elementmiddledropout',
       'highsuspend', 'highdropout', 'juvenileincarceration', 'highgrade15', 'gender',
       'black', 'hispanic', 'age', 'twoparenthome', 'SES', 'citizenship',
       'geography97', 'race']]

# Create a new variable called GR for Gender-Race:

def GR(df):
    if df["gender"] == 0 and df["race"] == 1:
        return "01"
    elif df["gender"] == 0 and df["race"] == 2:
        return "02"
    elif df["gender"] == 0 and df["race"] == 3:
        return "03"
    elif df["gender"] == 0 and df["race"] == 4:
        return "04"
    elif df["gender"] == 1 and df["race"] == 1:
        return "11"
    elif df["gender"] == 1 and df["race"] == 2:
        return "12"
    elif df["gender"] == 1 and df["race"] == 3:
        return "13"
    elif df["gender"] == 1 and df["race"] == 4:
        return "14"
    else:
        return "Error"

dataset["GR"] = dataset.apply(GR, axis=1)
dataset["GR"].value_counts()

# Dichotomize adult, juvenile incarceration and suspensions

def Aincarceration(df):
    if df["adultincarceration"] == 0:
        return 0
    elif df["adultincarceration"] != 0:
        return 1
dataset["Aincarceration"] = dataset.apply(Aincarceration, axis=1)
dataset["Aincarceration"].value_counts()

def Jincarceration(df):
    if df["juvenileincarceration"] == 0:
        return 0
    elif df["juvenileincarceration"] != 0:
        return 1
dataset["Jincarceration"] = dataset.apply(Jincarceration, axis=1)
dataset["Jincarceration"].value_counts()

def ESuspend(df):
    if df["elementarysuspend"] == 0:
        return 0
    elif df["elementarysuspend"] >= 1:
        return 1
dataset["ESuspend"] = dataset.apply(ESuspend, axis=1)
dataset["ESuspend"].value_counts()

def MSuspend(df):
    if df["middlesuspend"] == 0:
        return 0
    elif df["middlesuspend"] >= 1:
        return 1
dataset["MSuspend"] = dataset.apply(MSuspend, axis=1)
dataset["MSuspend"].value_counts()

def HSuspend(df):
    if df["highsuspend"] == 0:
        return 0
    elif df["highsuspend"] >= 1:
        return 1
dataset["HSuspend"] = dataset.apply(HSuspend, axis=1)
dataset["HSuspend"].value_counts()

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


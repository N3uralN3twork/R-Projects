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

dataset = pd.read_csv("Subset.csv", header=0)

dataset.shape
dataset.columns
#########################################################
###           3. Data Cleaning                        ###
#########################################################
X = dataset.drop(["Aincarceration"], axis=1)
y = dataset["Aincarceration"]
y.value_counts()






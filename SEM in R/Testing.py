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

dataset = pd.read_spss("NLS 2 no formats.sav")

#########################################################
###           3. Data Cleaning                        ###
#########################################################

# Create 7 new variables:








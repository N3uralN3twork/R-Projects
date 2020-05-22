import numpy as np
import pandas as pd

###############################################################################
###                     1.  Define Working Directory                        ###
###############################################################################
import os
abspath = os.path.abspath("C:/Users/miqui/OneDrive/R Projects/SEM in R")
os.chdir(abspath)


nls = pd.read_spss("NLS 2 no formats.sav")

nls.head(5)
nls.info()
nls.columns

# Create new variables




" 1. Create a table of descriptive statistics by ME status:"


# Obtain summary statistics for numeric variables by group:

nls.columns

def numeric(data, var, group):
    mean = data.groupby(str(group))[str(var)].mean()
    std  = data.groupby(str(group))[str(var)].std()
    list = [mean, std]
    print(f"Mean: {list[0]}, \nStd: {list[1]}")


numeric(nls, var="adultincarcemon", group="gender")




variables = pd.DataFrame(nls.columns)







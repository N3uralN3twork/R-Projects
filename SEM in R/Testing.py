###############################################################################
###                     1.  Define Working Directory                        ###
###############################################################################
import os
import pandas as pd
import numpy as np
abspath = os.path.abspath("C:/Users/miqui/OneDrive/R Projects/SEM in R")
os.chdir(abspath)

Services = pd.read_excel("ADAMHSCCNeedsAssessmentData20200522.xlsx", header=0)

Services.columns

test = Services.groupby("IndividualId", axis="columns")



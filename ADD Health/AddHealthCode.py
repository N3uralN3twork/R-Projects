"""
Add Health Data
Author: Matt Quinn
Date: 17 June 2020
"""
import pandas as pd

wave1 = pd.read_csv("C:/Users/miqui/Downloads/ICPSR_21600-V21/ICPSR_21600/DS0001/21600-0001-Data.tsv",
                    sep="\t", verbose=1)
wave1.info()

wave2 = pd.read_csv("C:/Users/miqui/Downloads/ICPSR_21600-Wave2/ICPSR_21600/DS0005/21600-0005-Data.tsv",
                    sep="\t")
wave2.info()

wave1["AID"].value_counts()
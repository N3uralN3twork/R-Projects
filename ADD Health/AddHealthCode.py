"""
Add Health Data
Author: Matt Quinn
Date: 17 June 2020
"""

"Import the necessary libraries and data:"
import pandas as pd
from datetime import datetime, date

wave1 = pd.read_csv("C:/Users/miqui/Downloads/ICPSR_21600-V21/ICPSR_21600/DS0001/21600-0001-Data.tsv",
                    sep="\t", verbose=1)
wave2 = pd.read_csv("C:/Users/miqui/Downloads/ICPSR_21600-Wave2/ICPSR_21600/DS0005/21600-0005-Data.tsv",
                    sep="\t")
wave1.info()
wave2.info()

wave1["AID"].value_counts()
wave2["AID"].value_counts()

"Merge waves by AID:"

waves = wave1.merge(wave2, on=["AID"])

waves = waves[["AID", "IMONTH", "IDAY", "IYEAR", "H1GI1M", "H1GI1Y"]]
waves.info()

"Calculate the Age variable:"

waves["IYEAR"] = waves["IYEAR"] + 1900


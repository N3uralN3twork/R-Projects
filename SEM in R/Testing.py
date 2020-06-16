formula = """Aincarceration ~ jail + victim + gunshotless12 + gunshot12to18 + unemployment + homeless +
    poverty1997 + poverty1998 + poverty1999 + poverty2000 + poverty2001 + poverty2002 + 
    death + hospitalization + divorce + juveniledestroyproperty + juvenilestealless +
    juvenilestealmore + juvenileotherproperty + juvenileattack + juvenileselldrugs + 
    adultgun + adultdestroyproperty + adultstealless + adultstealmore + adultotherproperty +
    adultattack + adultselldrugs + elementarysuspend + middlesuspend + elementmiddledropout +
    highsuspend + highdropout + Jincarceration + highgrade15 + gender + hispanic + age + 
    twoparenthome + SES + citizenship + geography97"""

dataset["GR"].value_counts()
GR_fit = smf.glm(formula = formula,
             data = dataset[dataset.GR == "02"],
             family = sm.families.Binomial()).fit()

print(GR_fit.summary())
print(GR_fit.summary2())
results = pd.concat([np.exp(GR_fit.params), GR_fit.pvalues, np.exp(GR_fit.conf_int())], axis=1)
results = pd.DataFrame(results)
results.columns = ["OddsRatio", "p-value", "Lower", "Upper"]
print(results)
results[(results["p-value"] <= 0.05)]
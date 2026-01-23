import pandas as pd
import statsmodels.formula.api as smf
from statsmodels.stats.weightstats import ttest_ind
import os
# Set working directory (adjust the path accordingly)

os.chdir('/Users/fabianfuentes/Library/CloudStorage/OneDrive-InstitutoTecnologicoydeEstudiosSuperioresdeMonterrey/cursos/Econometrics/econometrics_mek/Week 2/')

# Load data (using pandas instead of foreign library)
jobcorpsfile = "data/jobcorps.dta"
JobCorps = pd.read_stata(jobcorpsfile)

# Explore data file (equivalent pandas functions)
print(JobCorps.head())
print(JobCorps.columns)
print(JobCorps.shape)
print(JobCorps.dtypes)
print(JobCorps.describe())

# Compare hispanics in control and treatment group (using boolean indexing for filtering)
print(JobCorps[JobCorps['treatmnt'] == 0]['hispanic'].mean())
print(JobCorps[JobCorps['treatmnt'] == 1]['hispanic'].mean())

# Difference in means test (using statsmodels.stats.weightstats)
ttest_ind(JobCorps[JobCorps['treatmnt'] == 0]['hispanic'], JobCorps[JobCorps['treatmnt'] == 1]['hispanic'])

# OLS models (using statsmodels.formula.api)
model1 = smf.ols('treatmnt ~ white + black + hispanic', data=JobCorps).fit()
print(model1.summary())

target_vars = JobCorps.columns[19:36].drop('earnq16')  # Exclude earnq16

model2 = smf.ols(formula=f'treatmnt ~ {" + ".join(target_vars)}', data=JobCorps).fit()
print(model2.summary())

# Selection on observables
model3 = smf.ols('attrit ~ ' + " + ".join(target_vars), data=JobCorps).fit()
print(model3.summary())

# Average Treatment Effect
model_ATE = smf.ols('earnq16 ~ treatmnt', data=JobCorps).fit()
print(model_ATE.summary())

# Effect on the Treated and Non-Treated
ToT = JobCorps[JobCorps['treatmnt'] == 1]['earnq16'].mean()
NT = JobCorps[JobCorps['treatmnt'] == 0]['earnq16'].mean()
print(ToT - NT)

# Compare with regression coefficient
print(model_ATE.params)

# Check for correlation between residuals and treatment
print(model_ATE.resid.corr(JobCorps['treatmnt'].dropna()))

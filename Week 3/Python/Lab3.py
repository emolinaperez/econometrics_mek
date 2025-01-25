# Load Required Libraries
import os
import pandas as pd
import numpy as np
from scipy.stats import ttest_ind, t
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import statsmodels.api as sm
import seaborn as sns

from statsmodels.graphics.tsaplots import plot_acf

# Load Data 

# URL of the .dta file
data_file = "https://raw.githubusercontent.com/emolinaperez/econometrics_mek/main/Week%203/data/database_angrist_levy_09.dta"

# Load the .dta file into a dataFrame
data = pd.read_stata(data_file)

# Structure of the dataFrame
data.info()

# Summary statistics for the DataFrame
data.describe()

# Table 1- Descriptive Statistics

# Create the summary table
summary_table = pd.DataFrame({
    "Variable": [
        "Bagrut rate",
        "Arab school",
        "Religious school",
        "Father's education",
        "Mother's education",
        "Number of siblings",
        "Immigrant",
        "Lagged Bagrut"
    ],
    "All_Mean": [
        data.loc[data['panel'] == 1, 'zakaibag'].mean(skipna=True),  # Bagrut rate
        data.loc[data['panel'] == 1, 'semarab'].mean(skipna=True),  # Arab school
        data.loc[data['panel'] == 1, 'semrel'].mean(skipna=True),   # Religious school
        data.loc[data['panel'] == 1, 'educav'].mean(skipna=True),   # Father's education
        data.loc[data['panel'] == 1, 'educem'].mean(skipna=True),   # Mother's education
        data.loc[data['panel'] == 1, 'm_ahim'].mean(skipna=True),   # Number of siblings
        data.loc[data['panel'] == 1, 'ole5'].mean(skipna=True),     # Immigrant
        data.loc[data['panel'] == 1, 'lagscore'].mean(skipna=True)  # Lagged Bagrut
    ],
    "Boys_Mean": [
        data.loc[(data['panel'] == 1) & (data['boy'] == 1), 'zakaibag'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 1), 'semarab'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 1), 'semrel'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 1), 'educav'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 1), 'educem'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 1), 'm_ahim'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 1), 'ole5'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 1), 'lagscore'].mean(skipna=True)
    ],
    "Girls_Mean": [
        data.loc[(data['panel'] == 1) & (data['boy'] == 0), 'zakaibag'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 0), 'semarab'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 0), 'semrel'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 0), 'educav'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 0), 'educem'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 0), 'm_ahim'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 0), 'ole5'].mean(skipna=True),
        data.loc[(data['panel'] == 1) & (data['boy'] == 0), 'lagscore'].mean(skipna=True)
    ]
})

# Display the summary table
summary_table


# t-test to compare the means of semrel and semarab

# Separate the groups
group_semrel = data.loc[data['panel'] == 1, 'semrel'].dropna()
group_semarab = data.loc[data['panel'] == 1, 'semarab'].dropna()

# Perform the two-sample t-test (Welch's t-test for unequal variances)
t_stat, p_value = ttest_ind(group_semrel, group_semarab, equal_var=False)

# Calculate confidence interval for the difference in means
mean_group_semrel = group_semrel.mean()
mean_group_semarab = group_semarab.mean()
mean_diff = mean_group_semrel - mean_group_semarab
std_error = np.sqrt(group_semrel.var(ddof=1) / len(group_semrel) + group_semarab.var(ddof=1) / len(group_semarab))
df = len(group_semrel) + len(group_semarab) - 2
t_crit = t.ppf(0.975, df)  # Two-tailed test at 95% confidence level
ci_lower = mean_diff - t_crit * std_error
ci_upper = mean_diff + t_crit * std_error

# Hypotheses
null_hypothesis = "The means of the two groups are equal (mean_semrel = mean_semarab)."
alternative_hypothesis = "The means of the two groups are not equal (mean_semrel ≠ mean_semarab)."

# Decision rule (alpha = 0.05)
alpha = 0.05
decision = "Reject the null hypothesis" if p_value < alpha else "Fail to reject the null hypothesis"

# Display the results
print("Two-Sample T-Test:")
print(f"Hypotheses:")
print(f"Null Hypothesis: {null_hypothesis}")
print(f"Alternative Hypothesis: {alternative_hypothesis}")
print(f"\nResults:")
print(f"T-Statistic: {t_stat:.4f}")
print(f"P-Value: {p_value:.4f}")
print(f"Confidence Interval: [{ci_lower:.8f}, {ci_upper:.8f}]")
print(f"Mean in Group semrel: {mean_group_semrel:.7f}")
print(f"Mean in Group semarab: {mean_group_semarab:.7f}")
print(f"\nDecision: {decision}")


# t-test to compare the means of the treated variable across the groups defined by semarab

# Separate the groups based on `semarab`
treated_semarab_0 = data.loc[(data['panel'] == 1) & (data['semarab'] == 0), 'treated'].dropna()
treated_semarab_1 = data.loc[(data['panel'] == 1) & (data['semarab'] == 1), 'treated'].dropna()

# Perform the two-sample t-test (Welch's t-test for unequal variances)
t_stat, p_value = ttest_ind(treated_semarab_0, treated_semarab_1, equal_var=False)

# Calculate confidence interval for the difference in means
mean_group_0 = treated_semarab_0.mean()
mean_group_1 = treated_semarab_1.mean()
mean_diff = mean_group_0 - mean_group_1
std_error = np.sqrt(
    treated_semarab_0.var(ddof=1) / len(treated_semarab_0) +
    treated_semarab_1.var(ddof=1) / len(treated_semarab_1)
)
df = len(treated_semarab_0) + len(treated_semarab_1) - 2
t_crit = t.ppf(0.975, df)  # Two-tailed test at 95% confidence level
ci_lower = mean_diff - t_crit * std_error
ci_upper = mean_diff + t_crit * std_error

# Hypotheses
null_hypothesis = "The means of the `treated` variable are equal across `semarab` groups."
alternative_hypothesis = "The means of the `treated` variable are not equal across `semarab` groups."

# Decision rule (alpha = 0.05)
alpha = 0.05
decision = "Reject the null hypothesis" if p_value < alpha else "Fail to reject the null hypothesis"

# Display the results
print("Two-Sample T-Test:")
print(f"Hypotheses:")
print(f"Null Hypothesis: {null_hypothesis}")
print(f"Alternative Hypothesis: {alternative_hypothesis}")
print(f"\nResults:")
print(f"T-Statistic: {t_stat:.4f}")
print(f"P-Value: {p_value:.4f}")
print(f"Confidence Interval: [{ci_lower:.8f}, {ci_upper:.8f}]")
print(f"Mean in Group semarab=0: {mean_group_0:.7f}")
print(f"Mean in Group semarab=1: {mean_group_1:.7f}")
print(f"\nDecision: {decision}")


# t-test to compare the means of the treated variable across the groups defined by semrel

# Separate the groups based on `semrel`
treated_semrel_0 = data.loc[(data['panel'] == 1) & (data['semrel'] == 0), 'treated'].dropna()
treated_semrel_1 = data.loc[(data['panel'] == 1) & (data['semrel'] == 1), 'treated'].dropna()

# Perform the two-sample t-test (Welch's t-test for unequal variances)
t_stat, p_value = ttest_ind(treated_semrel_0, treated_semrel_1, equal_var=False)

# Calculate confidence interval for the difference in means
mean_group_0 = treated_semrel_0.mean()
mean_group_1 = treated_semrel_1.mean()
mean_diff = mean_group_0 - mean_group_1
std_error = np.sqrt(
    treated_semrel_0.var(ddof=1) / len(treated_semrel_0) +
    treated_semrel_1.var(ddof=1) / len(treated_semrel_1)
)
df = len(treated_semrel_0) + len(treated_semrel_1) - 2
t_crit = t.ppf(0.975, df)  # Two-tailed test at 95% confidence level
ci_lower = mean_diff - t_crit * std_error
ci_upper = mean_diff + t_crit * std_error

# Hypotheses
null_hypothesis = "The means of the `treated` variable are equal across `semrel` groups."
alternative_hypothesis = "The means of the `treated` variable are not equal across `semrel` groups."

# Decision rule (alpha = 0.05)
alpha = 0.05
decision = "Reject the null hypothesis" if p_value < alpha else "Fail to reject the null hypothesis"

# Display the results
print("Two-Sample T-Test:")
print(f"Hypotheses:")
print(f"Null Hypothesis: {null_hypothesis}")
print(f"Alternative Hypothesis: {alternative_hypothesis}")
print(f"\nResults:")
print(f"T-Statistic: {t_stat:.4f}")
print(f"P-Value: {p_value:.4f}")
print(f"Confidence Interval: [{ci_lower:.8f}, {ci_upper:.8f}]")
print(f"Mean in Group semrel=0: {mean_group_0:.7f}")
print(f"Mean in Group semrel=1: {mean_group_1:.7f}")
print(f"\nDecision: {decision}")


#Estimate liner model:

# Filter the data for panel == 1
data_subset = data[data['panel'] == 1]

# Fit the linear regression model using the formula syntax
model1 = smf.ols('zakaibag ~ treated', data=data_subset).fit()
print(model1.summary())

# mean of residuals
model1.resid.mean()


# Residual Plots
fig, ax = plt.subplots(2, 2, figsize=(12, 10))

# Residuals vs Fitted
sns.residplot(x=model1.fittedvalues, y=model1.resid, lowess=True, line_kws={'color': 'red'}, ax=ax[0, 0])
ax[0, 0].axhline(0, color='gray', linestyle='--')
ax[0, 0].set_title("Residuals vs Fitted")
ax[0, 0].set_xlabel("Fitted Values")
ax[0, 0].set_ylabel("Residuals")

# Normal Q-Q
sm.qqplot(model1.resid, line="45", fit=True, ax=ax[0, 1])
ax[0, 1].set_title("Normal Q-Q")

# Scale-Location (Spread-Location)
abs_resid = abs(model1.resid)**0.5
sns.scatterplot(x=model1.fittedvalues, y=abs_resid, ax=ax[1, 0])
ax[1, 0].axhline(0, color='gray', linestyle='--')
ax[1, 0].set_title("Scale-Location")
ax[1, 0].set_xlabel("Fitted Values")
ax[1, 0].set_ylabel("Sqrt(|Residuals|)")

# Residuals vs Leverage
sm.graphics.influence_plot(model1, criterion="cooks", size=2, ax=ax[1, 1])
ax[1, 1].set_title("Residuals vs Leverage")

plt.tight_layout()
plt.show()

# Plot the autocorrelation function (ACF) of residuals
plt.figure(figsize=(8, 6))
plot_acf(model1.resid, lags=20, alpha=0.05, title="ACF of Residuals")
plt.show()

from scipy.stats import pearsonr

#correlation between observables and residuals

# Observables and residuals
observables = data_subset['zakaibag']
residuals = model1.resid

# Calculate Pearson correlation
correlation, p_value = pearsonr(observables, residuals)

# Display the results
print("Correlation between observables and residuals:")
print(f"Correlation coefficient: {correlation:.4f}")
print(f"P-Value: {p_value:.4f}")


# School covariates, quartile dumies, micro covariates, see Table - 2  

import re

# covariates

# Extract covariates that start with "_Ip" in column names
ip = [col for col in data.columns if re.match(r"^_Ip", col)]

# Define additional covariates
additional_covariates = ["treated", "semarab", "semrel", "ls50", "ls75", "ls100", "educav", "educem", "ah4", "ole5"]

# Combine all covariates
covar = additional_covariates + ip

# Create the formula dynamically
fm2 = f"zakaibag ~ {' + '.join(covar)}"

print("Formula for regression:")
print(fm2)

# Fit the regression model using formula `fm2`
model2 = smf.ols(fm2, data=data_subset).fit()

# Display the summary of the model
print(model2.summary())


from scipy.stats import spearmanr

data_cor = data[data['panel'] == 1].copy()

# Add residuals from model2 to the DataFrame
data_cor['residuals'] = model2.resid

# Select columns for correlation analysis
columns_to_correlate = [
    "zakaibag", "treated", "semarab", "semrel", "ls50",
    "ls75", "ls100", "educav", "educem", "ah4", "ole5"
]

# Compute the Spearman correlation matrix
spearman_corr = data_cor[columns_to_correlate].corr(method='spearman')

# Display the correlation matrix
print("Spearman Correlation Matrix:")
print(spearman_corr)

# Perform a Spearman correlation test between `treated` and `lagscore`
treated = data_cor['treated']
lagscore = data_cor['lagscore']
correlation, p_value = spearmanr(treated, lagscore)

# Display the test results
print("\nSpearman Correlation Test:")
print(f"Correlation coefficient: {correlation:.4f}")
print(f"P-Value: {p_value:.4f}")



# Select columns for the pairs plot
columns_to_plot = ["zakaibag", "treated", "semarab", "semrel", "ls50","ls75", "ls100", "educav", "educem", "ah4", "ole5"]

# Subset the data
data_subset = data_cor[columns_to_plot]

# Create the pairs plot
sns.pairplot(
    data_subset,
    diag_kind="kde",  # Kernel density estimate for histograms
    corner=True,      # Display only the lower triangle of plots
    plot_kws={'alpha': 0.6}  # Transparency for scatter plots
)

# Adjust the layout and display the plot
plt.suptitle("Pairs Panels: Scatterplots, Histograms, and Correlations", y=1.02)
plt.tight_layout()
plt.show()

#Finally we can use diffent statistics to know how good our model is:
#AIC	Lower the better; The Akaike’s information criterion - AIC (Akaike, 1974)
#BIC	Lower the better; Bayesian information criterion - BIC (Schwarz, 1978)
#both measure of the goodness of fit of an estimated statistical model and can also be used for model selection. Both criteria depend on the maximized value of the likelihood function L for the estimated model.
#For model comparison, the model with the lowest AIC and BIC score is preferred.

# AIC and BIC for model1
aic_model1 = model1.aic
bic_model1 = model1.bic

print(f"AIC for model1: {aic_model1:.4f}")
print(f"BIC for model1: {bic_model1:.4f}")

# AIC and BIC for model2
aic_model2 = model2.aic
bic_model2 = model2.bic

print(f"AIC for model2: {aic_model2:.4f}")
print(f"BIC for model2: {bic_model2:.4f}")


from statsmodels.iolib.summary2 import summary_col

# Define subsets
data_subset = data[data['panel'] == 1]

# Model 1
model1 = smf.ols('zakaibag ~ treated + semarab + semrel', data=data_subset).fit()

# Model 2
model2 = smf.ols('zakaibag ~ treated + semarab + semrel + ls50 + ls75 + ls100 + educav + educem + ah4 + ole5', data=data_subset).fit()

# Model 3
ip_vars = [col for col in data.columns if col.startswith('_Ip')]
model3_formula = f"zakaibag ~ treated + semarab + semrel + {' + '.join(ip_vars)}"
model3 = smf.ols(model3_formula, data=data_subset).fit()

# Model 4
covar_vars = ["treated", "semarab", "semrel", "ls50", "ls75", "ls100", "educav", "educem", "ah4", "ole5"] + ip_vars
model4_formula = f"zakaibag ~ {' + '.join(covar_vars)}"
model4 = smf.ols(model4_formula, data=data_subset).fit()

# Generate summary table
results_table = summary_col(
    [model1, model2, model3, model4],
    stars=True, 
    model_names=["Model 1", "Model 2", "Model 3", "Model 4"],
    info_dict={
        'R-squared': lambda x: f"{x.rsquared:.4f}",
        'No. Observations': lambda x: f"{int(x.nobs)}"
    }
)

print(results_table)


# AIC values for all models
aic_values = [model1.aic, model2.aic, model3.aic, model4.aic]

# BIC values for all models
bic_values = [model1.bic, model2.bic, model3.bic, model4.bic]

criteria_df = pd.DataFrame({
    "Model": ["Model 1", "Model 2", "Model 3", "Model 4"],
    "AIC": aic_values,
    "BIC": bic_values
})
criteria_df

# Back to hypothesis testing

# load the `CASchools` dataset
caschools = pd.read_csv('Week 3/data/CASchools.csv')
caschools.head()

# add student-teacher ratio
caschools['STR'] = caschools['students'] / caschools['teachers']

# add average test-score
caschools['score'] = (caschools['read'] + caschools['math']) / 2

# estimate the model
linear_model = smf.ols('score ~ STR', data=caschools).fit()

# print the summary of the coefficients to the console
print(linear_model.summary())

# Note that summary() does not perform the normal approximation but calculates values using the t 
# distribution instead. Generally, the degrees of freedom of the assumed distribution are 
# determined in the following manner: DF = n-(number of covariates)-1

# determine residual degrees of freedom
residual_df = linear_model.df_resid


from scipy.stats import norm

# Define the range of t-values
t = np.linspace(-6, 6, 1000)

# Define t_act
tact = -4.75

# Plot the standard normal distribution
plt.plot(t, norm.pdf(t, 0, 1), color="steelblue", lw=2)
plt.title("Calculating the p-value of a Two-sided Test when t^act = -4.75", fontsize=12)
plt.xlabel("t")
plt.ylabel("Density")

# Highlight critical regions
# Left tail
plt.fill_between(t, 0, norm.pdf(t, 0, 1), where=(t <= -1.96), color="orange", alpha=0.6, label="Critical region")
# Right tail
plt.fill_between(t, 0, norm.pdf(t, 0, 1), where=(t >= 1.96), color="orange", alpha=0.6)

# Add vertical lines for critical values and tact
plt.axvline(-1.96, color="red", linestyle="--", label="Critical value (-1.96, 1.96)")
plt.axvline(1.96, color="red", linestyle="--")
plt.axvline(tact, color="green", linestyle="--", label="t act = -4.75")
plt.axvline(-tact, color="green", linestyle="--")

# Add annotations for arrows
plt.annotate("0.025 = α/2", xy=(-3.5, 0.22), xytext=(-2.5, 0.25),
             arrowprops=dict(facecolor='black', arrowstyle="->"), fontsize=10)
plt.annotate("0.025 = α/2", xy=(3.5, 0.22), xytext=(2.5, 0.25),
             arrowprops=dict(facecolor='black', arrowstyle="->"), fontsize=10)
plt.annotate("-|t^act|", xy=(-5, 0.16), xytext=(-4.75, 0.1),
             arrowprops=dict(facecolor='black', arrowstyle="->"), fontsize=10)
plt.annotate("|t^act|", xy=(5, 0.16), xytext=(4.75, 0.1),
             arrowprops=dict(facecolor='black', arrowstyle="->"), fontsize=10)

# Add ticks for critical values and tact
plt.xticks([0, -1.96, 1.96, -tact, tact], ['0', '-1.96', '1.96', f'-{tact}', f'{tact}'])

# Add legend
plt.legend()

# Show the plot
plt.tight_layout()
plt.show()


# Set seed for reproducibility
np.random.seed(4)

Y = np.random.normal(loc=5, scale=5, size=100)  # n=100, mean=5, sd=5

# Plot the data
plt.figure(figsize=(8, 6))
plt.scatter(range(len(Y)), Y, color="steelblue", s=50, label="Sample Data")
plt.title("Randomly Generated Sample Data", fontsize=14)
plt.xlabel("Index", fontsize=12)
plt.ylabel("Value", fontsize=12)
plt.axhline(y=5, color="red", linestyle="--", label="Mean = 5")  # Reference mean line

plt.grid(alpha=0.3)
plt.show()

#we can compute this interval in R by hand.

mean_Y = np.mean(Y)

print(mean_Y)

# Compute the confidence interval
CI_lower = mean_Y - 1.96 * 5 / 10  # 1.96 corresponds to 95% confidence level
CI_upper = mean_Y + 1.96 * 5 / 10

# Combine results into a dictionary for display
CI = {"CIlower": CI_lower, "CIupper": CI_upper}

# Print the confidence interval
print(CI)

# Knowing that mu=5 we see that, for our example data, the confidence interval covers true value.


# Initialize vectors for lower and upper confidence interval boundaries
n_iterations = 10000
lower = np.zeros(n_iterations)
upper = np.zeros(n_iterations)

# Loop for sampling, estimation, and confidence interval calculation
np.random.seed(4)  # Set seed for reproducibility
for i in range(n_iterations):
    Y = np.random.normal(loc=5, scale=5, size=100)  # Generate random sample
    mean_Y = np.mean(Y)
    lower[i] = mean_Y - 1.96 * 5 / 10  # Lower bound of CI
    upper[i] = mean_Y + 1.96 * 5 / 10  # Upper bound of CI

# join vectors of interval bounds in a matrix
CIs = np.column_stack((lower, upper))

# we see that not in all cases this is true

# identify intervals not covering mu
ID = np.where(~((CIs[:100, 0] <= 5) & (5 <= CIs[:100, 1])))[0]

# Initialize the plot
plt.figure(figsize=(8, 6))
plt.title("Confidence Intervals", fontsize=14)
plt.xlabel(r"$\mu$", fontsize=12)
plt.ylabel("Sample", fontsize=12)
plt.xlim(3, 7)
plt.ylim(1, 100)

# Set up the color vector
colors = ["gray"] * 100
for idx in ID:
    colors[idx] = "red"

# Draw the reference line at mu = 5
plt.axvline(x=5, color="black", linestyle="--", linewidth=1, label=r"$\mu=5$")

# Add horizontal bars representing the CIs
for j in range(100):
    plt.plot([CIs[j, 0], CIs[j, 1]], [j + 1, j + 1], color=colors[j], linewidth=2)

# Add legend
plt.legend()
plt.tight_layout()
plt.show()

# compute 95% confidence interval for coefficients in 'linear_model'

confidence_intervals = linear_model.conf_int(alpha=0.05)

# Add column names for clarity
confidence_intervals.columns = ["Lower Bound", "Upper Bound"]
confidence_intervals

from scipy.stats import t

# compute 95% confidence interval for coefficients in 'linear_model' by hand

# Extract the coefficient, standard error, and degrees of freedom for the second predictor
coef = linear_model.params[1]  # Coefficient for the second predictor
std_error = linear_model.bse[1]  # Standard error for the second predictor
df_residuals = linear_model.df_resid  # Residual degrees of freedom

# Compute the critical t-value for 95% confidence
t_crit = t.ppf(0.975, df_residuals)  # Two-tailed test, alpha=0.05

# Compute the confidence interval
ci_lower = coef - t_crit * std_error
ci_upper = coef + t_crit * std_error

# Display the confidence interval
print({"lower": ci_lower, "upper": ci_upper})

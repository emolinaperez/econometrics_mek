rm(list=ls())
# load packages
if (!require(pacman)) install.packages("pacman") 
p_load('ggplot2', 'sandwich','lmtest', 'estimatr', 'clubSandwich', 'stargazer') # clubSandwich: for clustered robust standard errors

# 1. Load the dataset  
data <- read.csv("Week 5/data/DDK2011_corrected.csv")

# 2. Data Cleaning and Preparation
# Check for missing values
summary(data)

# Filter to include only non-missing total scores
data_clean <- data[!is.na(data$totalscore), ]

# 3. Exploratory Data Analysis
# Summary statistics
str(data_clean)

# 4. Descriptive Statistics (Replicating Table 1)
descriptive_stats <- function(var) {
  mean_tracking <- mean(data_clean[data_clean$tracking == 1, var], na.rm = TRUE)
  sd_tracking <- sd(data_clean[data_clean$tracking == 1, var], na.rm = TRUE)
  mean_non_tracking <- mean(data_clean[data_clean$tracking == 0, var], na.rm = TRUE)
  sd_non_tracking <- sd(data_clean[data_clean$tracking == 0, var], na.rm = TRUE)
  p_value <- t.test(data_clean[data_clean$tracking == 1, var], 
                    data_clean[data_clean$tracking == 0, var])$p.value
  return(c(mean_tracking, sd_tracking, mean_non_tracking, sd_non_tracking, p_value))
}

# Variables to analyze
vars_to_analyze <- c("agetest", "girl", "std_mark", "totalscore")

# Apply the function to the variables
descriptive_results <- t(sapply(vars_to_analyze, descriptive_stats))
colnames(descriptive_results) <- c("Mean_Tracking", "SD_Tracking", "Mean_Non_Tracking", "SD_Non_Tracking", "P_Value")

# Convert to data frame for easier export
descriptive_results_df <- data.frame(Variable = vars_to_analyze, descriptive_results)

# Display the descriptive statistics
descriptive_results_df

# Visualize the distribution of total scores by tracking status using ggplot
ggplot(data, aes(x = std_mark, fill = factor(tracking))) +
  geom_histogram(aes(y = ..density..), color = "black")  +
  facet_wrap(~tracking, labeller = as_labeller(c(`0` = "Non-Tracking Schools", `1` = "Tracking Schools"))) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Distribution of Total Scores by Tracking Status", 
       x = "Total Score", 
       y = "Frequency", 
       fill = "Tracking") +
  theme_minimal()

  ggplot(data, aes(x = std_mark, fill = factor(tracking))) +
   geom_density()  +
  facet_wrap(~tracking, labeller = as_labeller(c(`0` = "Non-Tracking Schools", `1` = "Tracking Schools"))) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Distribution of Total Scores by Tracking Status", 
       x = "Total Score", 
       y = "Frequency", 
       fill = "Tracking") +
  theme_minimal()

# 5. Replicating Key Results (Replicating Table 2)
# Simple OLS regression: Impact of tracking on total scores

# Standardize
# Z= X−μ/σ
data_clean$totalscore_std <- scale(data_clean$totalscore)
data_clean$mathscoreraw_std <- scale(data_clean$mathscoreraw)
data_clean$litscore_std <- scale(data_clean$litscore)

# OLS regression for Total Scores
model_ols <- lm(totalscore_std ~ tracking, data = data_clean)
summary(model_ols)

# formatted summary table of the OLS regression 
stargazer(model_ols, type='text', title='Table 1: Total Scores')

# OLS regression for Math Scores
model_math <- lm(mathscoreraw_std ~ tracking, data = data_clean)
summary(model_math)

# formatted summary table of the OLS regression 
stargazer(model_math, type='text', title='Table 2: Math Scores')

# OLS regression for Literacy Scores
model_literacy <- lm(litscore_std ~ tracking, data = data_clean)
summary(model_literacy)

# formatted summary table of the OLS regression 
stargazer(model_math, type='text', title='Table 3: Literacy Scores')

# Robust standard errors
coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
coeftest(model_math, vcov = vcovHC(model_math, type = "HC1"))
coeftest(model_literacy, vcov = vcovHC(model_literacy, type = "HC1"))

# 6. Clustered Robust Standard Errors
# Assuming clustering at the school level (replace 'school_id' with the actual cluster variable)
# Example: Clustered standard errors for total scores
model_clustered <- lm(totalscore_std ~ tracking, data = data_clean)
summary(model_clustered)

cluster_se <- vcovCL(model_clustered, cluster = ~schoolid)
coeftest(model_ols, vcov = cluster_se)


# 7. Heterogeneous Effects by Achievement Levels
quartile_cutoffs <- quantile(data_clean$percentile, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
data_clean$achievement_quartile <- cut(data_clean$percentile, 
                                       breaks = quartile_cutoffs, 
                                       include.lowest = TRUE, 
                                       labels = 1:4)

# OLS with interaction terms
model_interaction <- lm(totalscore_std ~ tracking * achievement_quartile, data = data_clean)
summary(model_interaction)

# 8. Peer Effects Analysis
model_peer_effects <- lm(totalscore_std ~ rMEANstream_std_baselinemark, data = data_clean)
summary(model_peer_effects)

# 9. Regression Discontinuity Analysis (Optional)
rd_data <- data_clean[data_clean$percentile >= 45 & data_clean$percentile <= 55, ]

model_rd <- lm(totalscore ~ tracking, data = rd_data)
summary(model_rd)

# 10. Tests for Selection on Observables
balance_test <- function(var) {
  t.test(data_clean[data_clean$tracking == 1, var], 
         data_clean[data_clean$tracking == 0, var])
}

balance_age <- balance_test("agetest")
balance_gender <- balance_test("girl")
balance_baseline_score <- balance_test("std_mark")

print(balance_age)
print(balance_gender)
print(balance_baseline_score)

# 11. Consistency Checks
reset_test <- resettest(model_ols, power = 2:3, type = "fitted")
print(reset_test)

bp_test <- bptest(model_ols)
print(bp_test)

# 13. Central Limit Theorem (CLT) Build-up Exercise
set.seed(123)
skewed_data <- rexp(10000, rate = 1)

sample_means <- function(sample_size, n_samples) {
  replicate(n_samples, mean(sample(skewed_data, sample_size, replace = TRUE)))
}

means_10 <- sample_means(10, 1000)
means_30 <- sample_means(30, 1000)
means_100 <- sample_means(100, 1000)

clt_data <- data.frame(
  sample_mean = c(means_10, means_30, means_100),
  sample_size = factor(rep(c(10, 30, 100), each = 1000))
)

clt_plot <- ggplot(clt_data, aes(x = sample_mean, fill = sample_size)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  facet_wrap(~sample_size, scales = "free", ncol = 1) +
  labs(title = "Central Limit Theorem Demonstration", 
       x = "Sample Mean", 
       y = "Frequency", 
       fill = "Sample Size") +
  theme_minimal()

print(clt_plot)

# Save the outputs
stargazer(model_ols,model_math,model_literacy, model_interaction, type='text')
stargazer(model_ols,model_math,model_literacy, model_interaction, type = "html", title = "Table 1: results", 
          out = "models.html")

data.frame(Age_Balance_Test = balance_age$p.value, 
                     Gender_Balance_Test = balance_gender$p.value, 
                     Baseline_Score_Balance_Test = balance_baseline_score$p.value)
                     
data.frame(RESET_Test_pvalue = reset_test$p.value, 
                     BP_Test_pvalue = bp_test$p.value)                     

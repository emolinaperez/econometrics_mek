# Load libraries + Set simulation seed
if (!require(pacman)) install.packages("pacman")
library(pacman) ; p_load('tidyverse','stargazer')
set.seed(635)

N <- 5000

data <- tibble(
  # ~30% of firms are in the tech sector
  tech = rbinom(N, 1, .3)) %>% 
  # ~50% of tech firms are engaged in IP litigation.
  # ~10% of non-tech firms are engaged in IP litigation.
  mutate(IP = as.numeric(
    runif(N) > .9 - .4*tech)) %>%
 
  # Tech firms are very profitable...
  # ...but IP litigation negatively affects them.
  mutate(profit = 2 * tech 
         - .3 * IP
         + rnorm(N, mean = 5))

# Correlation between profits and IT litigation
m1 <- lm(profit ~ IP, data)

# Controlling for tech sector
m2 <- lm(profit ~ IP + tech, data)

stargazer(m1, m2, type = "text" )

# Residualize outcome
data$R_profit <- lm(profit ~ tech, data)$residuals

# Residualize treatment
data$R_IP <- lm(IP ~ tech, data)$residuals

# Correlation of residuals = causal effect
corr_resids <- lm(R_profit ~ R_IP, data)

stargazer(m1, m2, corr_resids,
          type = "text" )

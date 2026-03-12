################################################################################
# Lab 8: Econometrics
################################################################################

#load library
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load('car','modelsummary')

#Read data
data <- read.table("Week 8/data/MRW1992.txt",header=TRUE)
N <- matrix(data$N,ncol=1)
lndY <- matrix(log(data$Y85)-log(data$Y60),ncol=1)
lnY60 <- matrix(log(data$Y60),ncol=1)
lnI <- matrix(log(data$invest/100),ncol=1)
lnG <- matrix(log(data$pop_growth/100+0.05),ncol=1)
lnS <- matrix(log(data$school/100),ncol=1)
xx <- as.matrix(cbind(lnY60,lnI,lnG,lnS,matrix(1,nrow(lndY),1)))
x <- xx[N==1,]
y <- lndY[N==1]
n <- nrow(x)
k <- ncol(x)  

#Unrestricted regression  
invx <-solve(t(x)%*%x)
b_ols <- solve((t(x)%*%x),(t(x)%*%y))  
e_ols <- rep((y-x%*%b_ols),times=k)  
xe_ols <- x*e_ols  
V_ols <- (n/(n-k))*invx%*%(t(xe_ols)%*%xe_ols)%*%invx
se_ols <- sqrt(diag(V_ols))
print(b_ols)  
print(se_ols) 

#Constrained regression
R <- c(0,1,1,1,0)
iR <- invx%*%R%*%solve(t(R)%*%invx%*%R)%*%t(R)
b_cls <- b_ols - iR%*%b_ols 
e_cls <- rep((y-x%*%b_cls),times=k)
xe_cls <- x*e_cls  
V_tilde <- (n/(n-k+1))*invx%*%(t(xe_cls)%*%xe_cls)%*%invx 
V_cls <- V_tilde - iR%*%V_tilde - V_tilde%*%t(iR) +iR%*%V_tilde%*%t(iR)  
se_cls <- sqrt(diag(V_cls))
print(b_cls)
print(se_cls)  

#Efficient minimum distance  
Vr <- V_ols%*%R%*%solve(t(R)%*%V_ols%*%R)%*%t(R)  
b_emd <- b_ols - Vr%*%b_ols  
e_emd <- rep((y-x%*%b_emd),times=k)
xe_emd <- x*e_emd
V2 <- (n/(n-k+1))*invx%*%(t(xe_emd)%*%xe_emd)%*%invx
V_emd <- V2 - V2%*%R%*%solve(t(R)%*%V2%*%R)%*%t(R)%*%V2 

#with linear models
data$Y <- log(data$Y85)
data$GDP_1960 <- log(data$Y60)  
data$I_GDP <-  log(data$invest/100)
data$n_g_s <- log(data$pop_growth/100+0.05)
data$log_school <- log(data$school/100)

# Unrestricted regression
model.u <- lm( Y ~ I_GDP + n_g_s , data=subset(data,N==1))
summary(model.u)

# Restricted regression
model.r <- lm( Y ~ I_GDP + n_g_s , data=subset(data,N==1))
summary(model.r)

# test of restriction
car::linearHypothesis(model.r, "I_GDP + n_g_s = 0")

# p-value
car::linearHypothesis(model.r, "I_GDP + n_g_s = 0")$`Pr(>F)`[2]

# Implied α
# The "Implied α" (implied alpha) shown in the Mankiw, Romer and Weil table refers 
# to the share of capital in the Cobb-Douglas production function of the Solow model.

# alpha
alpha <- coef(model.r)[2] / (1 + coef(model.r)[2])
alpha

#################################################
# Table 1: ESTIMATION SOLOW OF THE TEXTBOOK MODE
#################################################

##########################
# Unrestricted regression
##########################

# Non-oil, unrestricted regression
model1a <- lm(log(Y85) ~ I_GDP + n_g_s,
             data=subset(data, N==1))

# Intermediate, unrestricted regression
model1b <- lm(log(Y85) ~ I_GDP + n_g_s,
             data=subset(data, I==1))

# OECD, unrestricted regression
model1c <- lm(log(Y85) ~ I_GDP + n_g_s, 
             data=subset(data, O==1))

# Table 1 - Unrestricted regression
modelsummary(list(model1a, model1b, model1b),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = c("nobs", "r.squared"),
             coef_rename = c('(Intercept)' = "CONSTANT", 
                             'I_GDP' = 'ln(I/GDP)',
                             'n_g_s' = 'ln(n + g + δ)'))

##########################
# Restricted regression
##########################

# ln(I/GDP) - ln(n + g + δ)

# Non-oil, restricted regression
model2a <- lm(log(Y85) ~ I(I_GDP - n_g_s), 
             data=subset(data, N==1))

# Intermediate, restricted regression
model2b <- lm(log(Y85) ~ I(I_GDP - n_g_s), 
             data=subset(data, I==1))

# OECD, restricted regression
model2c <- lm(log(Y85) ~ I(I_GDP - n_g_s), 
             data=subset(data, O==1))

# Table 1 - Restricted regression
modelsummary(list(model2a, model2b, model2c),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = c("nobs", "r.squared"),
             coef_rename = c('(Intercept)' = "CONSTANT", 
                             'I(I_GDP - n_g_s)' = 'ln(I/GDP) - ln(n + g + δ)'))

# test of restriction
test_a <- car::linearHypothesis(model1a, "I_GDP + n_g_s = 0")
test_b <- car::linearHypothesis(model1b, "I_GDP + n_g_s = 0")
test_c <- car::linearHypothesis(model1c, "I_GDP + n_g_s = 0")

# p-values
p_value_a <- test_a$`Pr(>F)`[2]
p_value_b <- test_b$`Pr(>F)`[2]
p_value_c <- test_c$`Pr(>F)`[2]

# Implied α
alpha_a <- coef(model2a)[2] / (1 + coef(model2a)[2])
alpha_b <- coef(model2b)[2] / (1 + coef(model2b)[2])
alpha_c <- coef(model2c)[2] / (1 + coef(model2c)[2])

test_rest <- data.frame(
  Sample = c("Non-oil", "Intermediate", "OECD"),
  p_value = c(p_value_a, p_value_b, p_value_c),
  Implied_alpha = c(alpha_a, alpha_b, alpha_c)
)

test_rest

##################################################
# Table 2: ESTIMATION SOLOW OF THE AUGMENTED MODE
##################################################

# Non-oil
model3a <- lm(log(Y85) ~ I_GDP + n_g_s + log_school, data=subset(data, N==1))
model4a <- lm(log(Y85) ~ I(I_GDP - n_g_s) + I(log_school - n_g_s), data=subset(data, N==1))

# Intermediate
model3b <- lm(log(Y85) ~ I_GDP + n_g_s + log_school, data=subset(data, I==1))
model4b <- lm(log(Y85) ~ I(I_GDP - n_g_s) + I(log_school - n_g_s), data=subset(data, I==1))

# OECD
model3c <- lm(log(Y85) ~ I_GDP + n_g_s + log_school, data=subset(data, O==1))
model4c <- lm(log(Y85) ~ I(I_GDP - n_g_s) + I(log_school - n_g_s), data=subset(data, O==1))

# Table 2 - Unrestricted regression
modelsummary(list(model3a, model3b, model3c),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = c("nobs", "r.squared"),
             coef_rename = c('(Intercept)' = "CONSTANT", 
                             'I_GDP' = 'ln(I/GDP)',
                             'n_g_s' = 'ln(n + g + δ)',
                             'log_school' = 'ln(SCHOOL)'))

# Table 2 - Restricted regression
modelsummary(list(model4a, model4b, model4c),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = c("nobs", "r.squared"),
             coef_rename = c('(Intercept)' = "CONSTANT", 
                             'I(I_GDP - n_g_s)' = 'ln(I/GDP) - ln(n + g + δ)',
                             'I(log_school - n_g_s)' = 'ln(SCHOOL) - ln(n + g + δ)'))

##################################################
# Table 3: TESTS FOR UNCONDITIONAL CONVERGE
##################################################

# Non-oil
model5a <- lm(log(Y85)-log(Y60) ~ log(Y60) , data=subset(data, N==1)) 

# Intermediate
model5b <- lm(log(Y85)-log(Y60) ~ log(Y60) , data=subset(data, I==1)) 

# OECD
model5c <- lm(log(Y85)-log(Y60) ~ log(Y60) , data=subset(data, O==1)) 

# Table 3
modelsummary(list(model5a, model5b, model5c),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = c("nobs", "r.squared"),
             coef_rename = c('(Intercept)' = "CONSTANT", 
                             'log(Y60)' = 'ln(Y60)'))
# Implied λ

# coefficient for log(Y60)
beta_a <- coef(model5a)[2]
T <- 25  # Time period 1985-1960

# calculate lambda (convergence rate)
lambda_a <- -(-log(1 - beta_a) / T)  # Note the extra negative sign to match paper convention

cat(sprintf("Non-oil Implied λ: %.5f\n", lambda_a))

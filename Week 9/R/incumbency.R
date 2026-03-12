rm(list=ls())

# The Electoral Advantage to Incumbency and Voters' Valuation of Politicians' Experience: A Regression 
# Discontinuity Analysis of Elections to the U.S

#install.packages("rdd")
library(rdd)

df_incum <- read.csv('Week 9/data/indiv_final.csv')
head(df_incum)

################################################################################
# yearel: election year

# myoutcomenext: a dummy variable indicating whether the candidate of the incumbent 
# party was elected

# difshare: a normalized running variable: proportion of votes of the party in the 
# previous election - 0.5. If  difshare>0 then the candidate runs for the same 
# party as the incumbent. 
################################################################################


# Create a variable to indicate whether the party of the candidate is the same as the incumbent
df_incum$incum_paty <- as.numeric(df_incum$difshare > 0)

incum_proportion <- sum(df_incum$incum_paty)/nrow(df_incum)

summary(df_incum$incum_paty)

# Check for discontinuities
rdd::DCdensity(df_incum$difshare, 0, verbose = TRUE, htest = TRUE, ext.out=TRUE)

# Analyzing, it can not be rejected that the null hypothesis difference is equals 
# zero. According to the output the p-value associated to this test is 0.962.
# This implies that the assumption that there is no differential density around 
# the cutoff holds in this case. It can also be seen in the plot produced by the 
# R command and that is presented below:


#Parametric Regression

# Now, doing a Parametric Regression keeping only the observations within 50 percentage 
# points of the cutoff (the absolute value of difshare is less than or equal to 0.5). 

# Add extra variables to run some linear models
df_incum$difshare_sq <- (df_incum$difshare)^2
df_incum$difshare_cub <- (df_incum$difshare)^3

# Model 1 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + e_i
m1 <- lm(myoutcomenext ~ incum_paty, 
         data = df_incum, 
         subset = abs(difshare) <= 0.5)
summary(m1)

# Model 2 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + 𝛽_2 Difshare_i + e_i
m2 <- lm(myoutcomenext ~ incum_paty + difshare, 
         data = df_incum, 
         subset = abs(difshare) <= 0.5)
summary(m2)

# Model 3 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + 𝛽_2 Difshare_i + 𝛽_3 Difshare_i ≥0 * Difshare_i + e_i
m3 <- lm(myoutcomenext ~ incum_paty + difshare + incum_paty*difshare, 
         data = df_incum, 
         subset = abs(difshare) <= 0.5)
summary(m3)

# Model 4 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + 𝛽_2 Difshare_i + 𝛽_2 Difshare_i2 + e_i
m4 <- lm(myoutcomenext ~ incum_paty + difshare + difshare_sq, 
         data = df_incum, 
         subset = abs(difshare) <= 0.5)
summary(m4)

# Model 5 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + 𝛽_2 Difshare_i + 𝛽_2 Difshare_i3 + e_i
m5 <- lm(myoutcomenext ~ incum_paty + difshare + difshare_cub, 
         data = df_incum, 
         subset = abs(difshare) <= 0.5)
summary(m5)


# Now, using the RDestimate command in R to estimate the effect non-parametrically 
# and see what 0.4707 is the point estimated obtained using this command.

model <- rdd::RDestimate(myoutcomenext~difshare, data=df_incum, subset = abs(df_incum$difshare) <=0.5)
summary(model)

#Plot A
model1 <- rdd::RDestimate(myoutcomenext ~ difshare, 
                     data = df_incum, 
                     subset = abs(df_incum$difshare) <=0.5)
plot(model1)

#Plot B
model2 <- rdd::RDestimate(myoutcomenext ~ difshare, 
                     data = df_incum, 
                     subset=abs(df_incum$difshare) <=0.5,
                     kernel = "rectangular", bw = 3)
plot(model2)

#Plot C
model3 <- rdd::RDestimate(myoutcomenext ~ difshare, 
                     data = df_incum, 
                     subset=abs(df_incum$difshare) <=0.5,
                     kernel = "rectangular", bw = (1/3))
plot(model3)

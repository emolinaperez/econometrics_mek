rm(list=ls())

#install.packages("rdd")
library(rdd)


indiv <- read.csv('Week 9/data/indiv_final.csv')


# Create a variable to indicate whether the party of the candidate is the same as the incumbent
indiv$incum_paty <- as.numeric(indiv$difshare > 0)

incum_proportion <- sum(indiv$incum_paty)/nrow(indiv)

summary(indiv$incum_paty)

head(indiv)
################################################################################
# yearel: election year

# myoutcomenext: a dummy variable indicating whether the candidate of the incumbent 
# party was elected

# difshare: a normalized running variable: proportion of votes of the party in the 
# previous election - 0.5. If  difshare>0 then the candidate runs for the same 
# party as the incumbent. 
################################################################################

# Check for discontinuities
DCdensity(indiv$difshare, 0, verbose = TRUE, htest = TRUE, ext.out=TRUE)

# Analyzing, it can not be rejected that the null hypothesis difference is equals 
# zero. According to the output the p-value associated to this test is 0.962. T
# This implies that the assumption that there is no differential density around 
# the cutoff holds in this case. It can also be seen in the plot produced by the 
# R command and that is presented below:


#Parametric Regression

# Now, doing a Parametric Regression keeping only the observations within 50 percentage 
# points of the cutoff (the absolute value of difshare is less than or equal to 0.5). 

# Add extra variables to run some linear models
indiv$difshare_sq <- (indiv$difshare)^2
indiv$difshare_cub <- (indiv$difshare)^3

# Model 1 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + e_i
m1 <- lm(myoutcomenext ~ incum_paty, 
         data = indiv, 
         subset = abs(difshare) <= 0.5)
summary(m1)

# Model 2 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + 𝛽_2 Difshare_i + e_i
m2 <- lm(myoutcomenext ~ incum_paty + difshare, 
         data = indiv, 
         subset = abs(difshare) <= 0.5)
summary(m2)

# Model 3 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + 𝛽_2 Difshare_i + 𝛽_3 Difshare_i ≥0 * Difshare_i + e_i
m3 <- lm(myoutcomenext ~ incum_paty + difshare + incum_paty*difshare, 
         data = indiv, 
         subset = abs(difshare) <= 0.5)
summary(m3)

# Model 4 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + 𝛽_2 Difshare_i + 𝛽_2 Difshare_i2 + e_i
m4 <- lm(myoutcomenext ~ incum_paty + difshare + difshare_sq, 
         data = indiv, 
         subset = abs(difshare) <= 0.5)
summary(m4)

# Model 5 - Y_i =𝛽_0 + 𝛽_1 Difshare_i ≥0 + 𝛽_2 Difshare_i + 𝛽_2 Difshare_i3 + e_i
m5 <- lm(myoutcomenext ~ incum_paty + difshare + difshare_cub, 
         data = indiv, 
         subset = abs(difshare) <= 0.5)
summary(m4)


# Now, using the RDestimate command in R to estimate the effect non-parametrically 
# and see what 0.4707 is the point estimated obtained using this command.

model <- RDestimate(myoutcomenext~difshare, data=indiv, subset = abs(indiv$difshare) <=0.5)
summary(model)

#Plot A
model1 <- RDestimate(myoutcomenext ~ difshare, 
                     data = indiv, 
                     subset = abs(indiv$difshare) <=0.5)
plot(model1)

#Plot B
model2 <- RDestimate(myoutcomenext ~ difshare, 
                     data = indiv, 
                     subset=abs(indiv$difshare) <=0.5,
                     kernel = "rectangular", bw = 3)
plot(model2)

#Plot C
model3 <- RDestimate(myoutcomenext ~ difshare, 
                     data = indiv, 
                     subset=abs(indiv$difshare) <=0.5,
                     kernel = "rectangular", bw = (1/3))
plot(model3)
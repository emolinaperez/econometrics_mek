rm(list=ls())

#Sometimes panel data is also called longitudinal data as it adds a temporal dimension to cross-sectional data. 
library(AER)
library(plm)
data(Fatalities)

# pdata.frame() declares the data as panel data.
Fatalities<- pdata.frame(Fatalities, index = c("state", "year"))

# obtain the dimension and inspect the structure
is.data.frame(Fatalities)
dim(Fatalities)
str(Fatalities)

# list the first few observations
head(Fatalities)

# summarize the variables 'state' and 'year'
summary(Fatalities[, c(1, 2)])

# define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# subset the data
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")

# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)

coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")

coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")

# plot the observations and add the estimated regression line for 1982 data
plot(x = as.double(Fatalities1982$beertax), 
     y = as.double(Fatalities1982$fatal_rate), 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1982_mod, lwd = 1.5, col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")


#plot observations and add estimated regression line for 1988 data
plot(x = as.double(Fatalities1988$beertax), 
     y = as.double(Fatalities1988$fatal_rate), 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1988_mod, lwd = 1.5,col="darkred")
legend("bottomright",lty=1,col="darkred","Estimated Regression Line")

#Panel Data with Two Time Periods: “Before and After” Comparisons

# compute the differences 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)

coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")
# plot the differenced data
plot(x = as.double(diff_beertax), 
     y = as.double(diff_fatal_rate), 
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
     cex.main=1,
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20, 
     col = "steelblue")

# add the regression line to plot
abline(fatal_diff_mod, lwd = 1.5,col="darkred")

#add legend
legend("topright",lty=1,col="darkred","Estimated Regression Line")
# compute mean fatality rate over all states for all time periods
mean(Fatalities$fatal_rate)

#Fixed Effects Regression

#Software packages use a so-called “entity-demeaned” OLS algorithm which is computationally more efficient than estimating regression models with  k+nregressors as needed for models

#Application to Traffic Deaths
class(Fatalities$state)
fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state -1 , data = Fatalities)
fatal_fe_lm_mod
summary(fatal_fe_lm_mod)

# obtain demeaned data
fatal_demeaned <- with(Fatalities,
            data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
            beertax = beertax - ave(beertax, state)))

# estimate the regression
summary(lm(fatal_rate ~ beertax - 1, data = fatal_demeaned))

library(plm)
# estimate the fixed effects regression with plm()
fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")


coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")


# Regression with Time Fixed Effects
# estimate a combined time and entity fixed effects regression model
# via lm()
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities)
summary(fatal_tefe_lm_mod)

# via plm()
fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")

# check class of the model object
class(fatal_tefe_lm_mod)
#> [1] "lm"

# obtain a summary based on heteroskedasticity-robust standard errors 
# (no adjustment for heteroskedasticity only)
coeftest(fatal_tefe_lm_mod, vcov = vcovHC, type = "HC1")[1, ]

# obtain a summary based on clustered standard errors 
# (adjustment for autocorrelation + heteroskedasticity)
coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")

#Drunk Driving Laws and Traffic Deaths
# discretize the minimum legal drinking age
Fatalities$drinkagec <- cut(Fatalities$drinkage,
                            breaks = 18:22, 
                            include.lowest = TRUE, 
                            right = FALSE)

# set minimum drinking age [21, 22] to be the baseline level
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")

# mandatory jail or community service?
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
                                             labels = c("no", "yes")))

# the set of observations on all variables for 1982 and 1988
fatal_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988), ]

# estimate all seven models
fat_mod1 <- lm(fatal_rate ~ beertax, data = Fatalities)

fat_mod2 <- plm(fatal_rate ~ beertax + state, data = Fatalities)

fat_mod3 <- plm(fatal_rate ~ beertax + state + year,
                       index = c("state","year"),
                       model = "within",
                       effect = "twoways", 
                       data = Fatalities)

fat_mod4 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fat_mod5 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles,
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fat_mod6 <- plm(fatal_rate ~ beertax + year + drinkage 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fat_mod7 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = fatal_1982_1988)

# gather clustered standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(fat_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod4, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod5, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod6, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod7, type = "HC1"))))

library(modelsummary)

modelsummary(list(fat_mod1, fat_mod2, fat_mod3, fat_mod4, 
                  fat_mod5,fat_mod6,fat_mod7),
             fmt = 4,
             output = 'gt',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = c("nobs", "r.squared"),
             se = rob_se,
             title = "Linear Panel Regression Models of Traffic Fatalities due to Drunk Driving")


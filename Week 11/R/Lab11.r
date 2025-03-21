#quantile regressions 
#In mircoeconomics, an Engel curve (named after German statistician Ernst Engel) describes how household expenditure 
#on a particular good or service varies with household income
#Engel collected food expenditure vs household income for a sample of 235 19th century working class Belgian households

library(quantreg)
data(engel)
attach(engel)
plot(income,foodexp,cex=.25,type="n",xlab="Household Income", ylab="Food Expenditure")
points(income,foodexp,cex=.5,col="blue")
abline(rq(foodexp~income,tau=.5),col="blue")
abline(lm(foodexp~income),lty=2,col="red") #the dreaded ols line
taus = c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)){
  abline(rq(foodexp~income,tau=taus[i]),col="gray")
 }

#analyze estimates 
summary(rq(foodexp~income,tau=taus[1]))


#Plot the slope and intercept of the estimated quantile regression for the Engel data as a function of quantile
income_c <- income - mean(income)
fit1 <- summary(rq(foodexp~income_c,tau=2:98/100))
fit2 <- summary(rq(foodexp~income_c,tau=c(.05, .25, .5, .75, .95)))
plot(fit1,mfrow = c(1,2))

##
#probit regression 
##
mydata <- read.csv('Week 11/data/lab11_data.csv')

## convert rank to a factor (categorical variable)
mydata$rank <- factor(mydata$rank)

## view first few rows
head(mydata)
summary(mydata)
xtabs(~rank + admit, data = mydata)

# estimate the probit model
myprobit <- glm(admit ~ gre + gpa + rank, family = binomial(link = "probit"), 
    data = mydata)

## model summary
summary(myprobit)

#In the output above, the first thing we see is the call, this is R reminding us what the model we ran was, what options we specified, etc.
#Next we see the deviance residuals, which are a measure of model fit. This part of output shows the distribution of the deviance residuals for individual cases used in the model. Below we discuss how to use summaries of the deviance statistic to asses model fit.
#The next part of the output shows the coefficients, their standard errors, the z-statistic (sometimes called a Wald z-statistic), and the associated p-values. Both gre, gpa, and the three terms for rank are statistically significant. The probit regression coefficients give the change in the z-score or probit index for a one unit change in the predictor.
#For a one unit increase in gre, the z-score increases by 0.001.
#For each one unit increase in gpa, the z-score increases by 0.478.
#The indicator variables for rank have a slightly different interpretation. For example, having attended an undergraduate institution of rank of 2, versus an institution with a rank of 1 (the reference group), decreases the z-score by 0.415.
#Below the table of coefficients are fit indices, including the null and deviance residuals and the AIC.


#We can use the confint function to obtain confidence intervals for the coefficient estimates. These will be profiled confidence intervals by default, created by profiling the likelihood function. As such, they are not necessarily symmetric.
confint(myprobit)

#You can also use predicted probabilities to help you understand the model. To do this, we first create a data frame containing the values we want for the independent variables.
newdata <- data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 
    4 * 4), gpa = rep(c(2.5, 3, 3.5, 4), each = 100 * 4), rank = factor(rep(rep(1:4, 
    each = 100), 4)))
dim(newdata)
head(newdata)

library(ggplot2)
newdata[, c("p", "se")] <- predict(myprobit, newdata, type = "response", se.fit = TRUE)[-3]
ggplot(newdata, aes(x = gre, y = p, colour = rank)) + geom_line() + facet_wrap(~gpa)


#lab 10 
# Multicollinearity means that two or more regressors in a multiple regression model are strongly correlated.
#lm will produce a warning in the first line of the coefficient section of the output (1 not defined because of singularities) 
#and ignore the regressor(s) which is (are) assumed to be a linear combination of the other

# define the fraction of English learners
library(AER)
data("CASchools")
dim(CASchools)
summary(CASchools)        
CASchools$score <- (CASchools$read + CASchools$math)*0.5
CASchools$FracEL <- CASchools$english / 100

# estimate the model
mult.mod <- lm(score ~ computer + english + FracEL, data = CASchools) 

# obtain a summary of the model
summary(mult.mod)                                                 

#consider two further examples where our selection of regressors induces perfect multicollinearity.
#First, assume that we intend to analyze the effect of class size on test score by using a dummy variable that identifies classes which are not small

# if STR smaller 12, NS = 0, else NS = 1
CASchools$NS <- ifelse(CASchools$computer < 0, 0, 1)

# estimate the model
mult.mod <- lm(score ~ computer + english + NS, data = CASchools)

# obtain a model summary
summary(mult.mod)     

table(CASchools$NS)
#CASchools$NS is a vector of ones and our data set includes observations
#This obviously violates assumption 4 of GMTheorem: the observations for the intercept are always 1


#Another example of perfect multicollinearity is known as the dummy variable trap. 
#This may occur when multiple dummy variables are used as regressors.
#A common case for this is when dummies are used to sort the data into mutually exclusive categories. 
#For example, suppose we have spatial information that indicates whether a school is located in the North, West, South or East of the U.S. 
#We run into problems when trying to estimate a model that includes a constant and all four direction dummies in the model
#since then for all observations the constant term is a linear combination of the dummies

#thus the “dummy variable trap” means not paying attention and falsely including exhaustive dummies and a constant in a regression model
# set seed for reproducibility
set.seed(1)

# generate artificial data on location
CASchools$direction <- sample(c("West", "North", "South", "East"), 
                              420, 
                              replace = T)

# estimate the model
mult.mod <- lm(score ~ computer + english + direction, data = CASchools)

# Y =  beta0 + beta1*Dummy1+beta2*Dummy2+beta3*Dummy3 
# Y =  beta0 + beta1*0+beta2*1+beta3*0 

# obtain a model summary
summary(mult.mod)                                                 

#Notice that R solves the problem on its own by generating and including the dummies directionNorth, directionSouth and directionWest but omitting directionEast

#consider the case where a perfect linear relationship arises from redundant regressors. 
# Percentage of english speakers 
CASchools$PctES <- 100 - CASchools$english

# estimate the model
mult.mod <- lm(score ~ computer + english + PctES, data = CASchools)

# obtain a model summary
summary(mult.mod)                                                 


#Imperfect Multicollinearity
# load packages
library(MASS)
#install.packages("mvtnorm")
library(mvtnorm)

# set number of observations
n <- 50

# initialize vectors of coefficients
coefs1 <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))
coefs2 <- coefs1

# set seed
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  # for cov(X_1,X_2) = 0.25
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs1[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
  # for cov(X_1,X_2) = 0.85
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 8.5), c(8.5, 10)))
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs2[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
}

# obtain variance estimates
diag(var(coefs1))

diag(var(coefs2))

#We are interested in the variances which are the diagonal elements. We see that due to the high collinearity, the variances of 
#beta1 and beta2 have more than tripled, meaning it is more difficult to precisely estimate the true coefficients.



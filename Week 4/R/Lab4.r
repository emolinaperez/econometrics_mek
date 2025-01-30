rm(list=ls())

#The error term of our regression model is homoskedastic if the variance of the conditional distribution is constant for all observations in our sample
#If instead there is dependence of the conditional variance of on X, the error term is said to be heteroskedastic. 

# load scales package for adjusting color opacities
if (!require(pacman)) install.packages("pacman")
library(pacman) ; p_load('scales','ggplot2')

# generate some heteroskedastic data:

# set seed for reproducibility
set.seed(123) 

# set up vector of x coordinates
x <- rep(c(10, 15, 20, 25), each = 25)

# initialize vector of errors
e <- c()

# sample 100 errors such that the variance increases with x
e[1:25] <- rnorm(25, sd = 10)
e[26:50] <- rnorm(25, sd = 15)
e[51:75] <- rnorm(25, sd = 20)
e[76:100] <- rnorm(25, sd = 25)

# set up y
y <- 720 - 3.3 * x + e

# Estimate the model 
mod <- lm(y ~ x)

# Plot the data
plot(x = x, 
     y = y, 
     main = "An Example of Heteroskedasticity",
     xlab = "Student-Teacher Ratio",
     ylab = "Test Score",
     cex = 0.5, 
     pch = 19, 
     xlim = c(8, 27), 
     ylim = c(600, 710))

# Add the regression line to the plot
abline(mod, col = "darkred")

# Add boxplots to the plot
boxplot(formula = y ~ x, 
        add = TRUE, 
        at = c(10, 15, 20, 25), 
        col = alpha("gray", 0.4), 
        border = "black")

#For this artificial data it is clear that the conditional error variances differ. 
#Specifically, we observe that the variance in test scores (and therefore the variance of the errors committed) increases 
#with the student teacher ratio.




# How responsive are private transfers to income?
# Evidence from a laissez-faire economy
# Donald Cox, Bruce E. Hansen, Emmanuel Jimenez

#load data
Data.File<-"https://raw.githubusercontent.com/emolinaperez/econometrics_mek/main/Week%204/data/CHJ2004.txt"
Data<-read.table(Data.File,header=TRUE)

#verify properties 
sapply(Data,class)

#get summary statistics 
dim(Data)
summary(Data)

#how does transfer vary per income level 
library(ggplot2)
ggplot(Data,aes(x=income,y=transfers))+geom_point()+geom_smooth()
#lets look into things a bit dipper 
ggplot(Data,aes(x=income,y=transfers))+geom_point()+geom_smooth()+ylim(0, 12000)
#let's run the first model 
model1<-lm(transfers~income, data=Data)
summary(model1)
#regression diagnostics
par(mfrow=c(2,2))
plot(model1)

#test assumptions in greater detail
test<- data.frame(yhat = fitted(model1),res = residuals(model1),income=Data$income)

# does residuals' conditional mean is zero? 
ggplot(test, aes(yhat, res)) + geom_point() + geom_hline(yintercept = 0, color = "red") + geom_smooth()

#does resifuals are correlated with the predictor 
ggplot(test, aes(income, res)) + geom_point() + geom_hline(yintercept = 0, color = "red") + geom_smooth()
ggplot(test, aes(income, res)) + geom_point() + geom_hline(yintercept = 0, color = "red") + geom_smooth() + ylim(0, 100000)

#we should add controls to our regression 
colnames(Data)
rms<-c("tabroad","tdomestic","tinkind","tgifts","income","wage","entrepreneourial","rental","interest","pension","dividend","transfers")
dummies<-c("region") #with more than 2 classes 
Treatment<-"income"
Ws <- subset(colnames(Data),!(colnames(Data)%in%c(rms,dummies)))
#create model with controls 
formula.model2 <- as.formula(paste("transfers","~",paste(c(Treatment,Ws),collapse="+"),paste0("+ factor(",dummies,")"),sep=""))
#second run regression
model2<-lm(formula.model2,data=Data)
summary(model2)

#get diagnostics 
par(mfrow=c(2,2))
plot(model2)

#slection biass 
sbias<-lm(income  ~ primary + somesecondary + secondary + someuniversity +
            university + age + female + married + child1 + child7 + child15 +
            size + bothwork + notemployed + marriedf,data=Data)
summary(sbias)

#how strongly correlated are these controls with treatment and residuals  
Data$yhat <- fitted(model2)
Data$res <- residuals(model2)
cortable<-cor(Data[,c(Treatment,Ws,"yhat","res")])  
apply(cortable,c(1,2),function(x){ifelse(abs(x)>0.05,1,0)})
mean(Data$res)

#lets tabulate results accross classes 
#we can create a dummy for income 
Data$income_level <- as.character( 
  cut(Data$income,
      breaks=c(0,as.numeric(quantile(Data$income,0.25)),as.numeric(quantile(Data$income,0.50)),as.numeric(quantile(Data$income,0.75)),max(Data$income)),
      labels= c("Q25","Q50","Q75",">Q75"))
)
#tabulate average transfers per income level
aggregate(list(mean_transfers=Data$transfers),list(income_level=Data$income_level), mean)

#now run the regression per income level
#below Q25 lowest income 
data_section <- subset(Data,income_level=="Q25")
ggplot(data_section,aes(x=income,y=transfers))+geom_point()+geom_smooth()
model3<-lm(formula.model2,data=data_section)
summary(model3)
plot(model3)

#Q25-Q50
data_section <- subset(Data,income_level=="Q50")
ggplot(data_section,aes(x=income,y=transfers))+geom_point()+geom_smooth()
model4<-lm(formula.model2,data=data_section)
summary(model4)
plot(model4)

#Q50-Q75
data_section <- subset(Data,income_level=="Q75")
ggplot(data_section,aes(x=income,y=transfers))+geom_point()+geom_smooth()
model5<-lm(formula.model2,data=data_section)
summary(model5)
plot(model5)

#>Q75
data_section <- subset(Data,income_level==">Q75")
ggplot(data_section,aes(x=income,y=transfers))+geom_point()+geom_smooth()
model6<-lm(formula.model2,data=data_section)
summary(model6)
plot(model6)

#specify polynomial regression 
#below Q25 lowest income 
data_section <- subset(Data,income_level=="Q25")
formula_poly_model <- as.formula(paste("transfers","~",paste(c(paste0("poly(",Treatment,", 5 , raw=TRUE)"),Ws),collapse="+"),paste0("+ factor(",dummies,")"),sep=""))
model_poly <- lm(formula_poly_model,data=data_section)
summary(model_poly)
#plot the model 
ggplot(data_section, aes(x=income,y=transfers)) + geom_point() + stat_smooth(method='lm', formula = y~ poly(x,4)  ) 

#how does this change if we do this for the entire sample
model_poly <- lm(formula_poly_model,data=Data)
summary(model_poly)
ggplot(Data, aes(x=income,y=transfers)) + geom_point() + stat_smooth(method='lm', formula = y~ poly(x,5)  ) 


#A Real-World Example for Heteroskedasticity
#Think about the economic value of education
# load package and attach data
p_load('AER')
data("CPSSWEducation")
attach(CPSSWEducation)

# get an overview
summary(CPSSWEducation)
#>       age          gender        earnings        education    
#>  Min.   :29.0   female:1202   Min.   : 2.137   Min.   : 6.00  
#>  1st Qu.:29.0   male  :1748   1st Qu.:10.577   1st Qu.:12.00  
#>  Median :29.0                 Median :14.615   Median :13.00  
#>  Mean   :29.5                 Mean   :16.743   Mean   :13.55  
#>  3rd Qu.:30.0                 3rd Qu.:20.192   3rd Qu.:16.00  
#>  Max.   :30.0                 Max.   :97.500   Max.   :18.00

# estimate a simple regression model
labor_model <- lm(earnings ~ education)

# plot observations and add the regression line
plot(education, 
     earnings, 
     ylim = c(0, 150))

abline(labor_model, 
       col = "steelblue", 
       lwd = 2)
# print the contents of labor_model to the console
labor_model
# compute a 95% confidence interval for the coefficients in the model
confint(labor_model)
#Since the interval does contain zero we can reject the hypothesis that the coefficient on education is zero at the 5% level. 

#is R estimating homokestaic or heterokestatic CIs? 
# Store model summary in 'model'
model <- summary(labor_model)

# Extract the standard error of the regression from model summary
SER <- model$sigma

# Compute the variation in 'education'
V <- (nrow(CPSSWEducation)-1) * var(education)

# Compute the standard error of the slope parameter's estimator and print it
SE.beta_1.hat <- sqrt(SER^2/V)
SE.beta_1.hat
#> [1] 0.06978281

# Use logical operators to see if the value computed by hand matches the one provided 
# in mod$coefficients. Round estimates to four decimal places
round(model$coefficients[2, 2], 4) == round(SE.beta_1.hat, 4)
#Thus summary() estimates the homoskedasticity-only standard error
#This issue may invalidate inference when using the previously treated tools for hypothesis testing
#we should be cautious when making statements about the significance of regression coefficients on the basis of 
#statistics as computed by summary() or confidence intervals produced by 
#confint() if it is doubtful for the assumption of homoskedasticity to hold!


#Computation of Heteroskedasticity-Robust Standard Errors
# compute heteroskedasticity-robust standard errors
p_load('sandwich')
vcov <- vcovHC(labor_model, type = "HC1") # HC1 uses the same corrrection as in stata 
vcov
#The output of vcovHC() is the variance-covariance matrix of coefficient estimates.
#We are interested in the square root of the diagonal elements of this matrix, i.e., the standard error estimates.
# compute the square root of the diagonal elements in vcov
robust_se <- sqrt(diag(vcov))
robust_se

#Now assume we want to generate a coefficient summary as provided by summary()
#but with robust standard errors of the coefficient estimators, robust statistics and corresponding 
#values for the regression model linear_model. This can be done using coeftest() from the package lmtest, see ?coeftest. 
#Further we specify in the argument vcov. that vcov, the Eicker-Huber-White estimate should be used.

# we invoke the function `coeftest()` on our model
coeftest(labor_model, vcov. = vcov)
#We see that the values reported in the column Std. Error are equal those from sqrt(diag(vcov)).

#How severe are the implications of using homoskedasticity-only standard errors in the presence of heteroskedasticity?
#The answer is: it depends. As mentioned above we face the risk of drawing wrong conclusions when conducting significance tests.



# generate heteroskedastic data 
set.seed(905)

X <- 1:500
Y <- rnorm(n = 500, mean = X, sd = 0.6 * X)

# estimate a simple regression model
reg <- lm(Y ~ X)
summary(reg)

# plot the data
plot(x = X, y = Y, 
     pch = 19, 
     col = "steelblue", 
     cex = 0.8)

# add the regression line to the plot
abline(reg, 
       col = "darkred", 
       lwd = 1.5)

# test hypthesis using the default standard error formula

# Step 1: Extract coefficient and default covariance matrix
coef_X <- coef(reg)["X"]
var_default <- vcov(reg)["X", "X"]  # Homoscedastic covariance

# Step 2: Compute t-statistic and F-statistic
t_stat <- (coef_X - 1) / sqrt(var_default)  # H0: X = 1
f_stat <- t_stat^2  # F = t^2 (for 1 degree of freedom)

# Step 3: Compute p-value (F-distribution)
p_value <- pf(f_stat, df1 = 1, df2 = df.residual(reg), lower.tail = FALSE)

# Step 4: Assign significance codes
signif_code <- if (p_value <= 0.001) "***" else
               if (p_value <= 0.01)  "**"  else
               if (p_value <= 0.05)  "*"   else
               if (p_value <= 0.1)   "."   else " "

# Step 5: Print results with stars and legend
cat(
  "Linear Hypothesis Test (Classical F-Test)\n",
  "F =", round(f_stat, 4), "\n",
  "Pr(>F) =", round(p_value, 5), signif_code, "\n",
  "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n"
)
#linearHypothesis(reg, hypothesis.matrix = "X = 1")$'Pr(>F)'[2] < 0.05


# test hypothesis using the robust standard error formula

# Step 1: Extract coefficient and robust covariance matrix (HC1)
coef_X <- coef(reg)["X"]
var_robust <- vcovHC(reg, type = "HC1")["X", "X"]  # Robust covariance

# Step 2: Compute t-statistic and F-statistic (using robust SEs)
t_stat_robust <- (coef_X - 1) / sqrt(var_robust)  # H0: X = 1
f_stat_robust <- t_stat_robust^2  # F = t^2 (for 1 degree of freedom)

# Step 3: Compute p-value (F-distribution)
p_value_robust <- pf(f_stat_robust, df1 = 1, df2 = df.residual(reg), lower.tail = FALSE)

# Step 4: Assign significance codes
signif_code_robust <- if (p_value_robust <= 0.001) "***" else
                      if (p_value_robust <= 0.01)  "**"  else
                      if (p_value_robust <= 0.05)  "*"   else
                      if (p_value_robust <= 0.1)   "."   else " "

# Step 5: Print results with stars and legend
cat(
  "Linear Hypothesis Test (Robust F-Test)\n",
  "F =", round(f_stat_robust, 4), "\n",
  "Pr(>F) =", round(p_value_robust, 5), signif_code_robust, "\n",
  "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n"
)

#linearHypothesis(reg, hypothesis.matrix = "X = 1", white.adjust = "hc1")$'Pr(>F)'[2] < 0.05

#how bad is this error?
# initialize vectors t and t.rob
t <- c()
t.rob <- c()

# loop sampling and estimation
for (i in 1:10000) {
  
  # sample data
  X <- 1:1000
  Y <- rnorm(n = 1000, mean = X, sd = 0.6 * X)
  
  # estimate regression model
  reg <- lm(Y ~ X)
  
  # homoskedasdicity-only significance test
  t[i] <- linearHypothesis(reg, "X = 1")$'Pr(>F)'[2] < 0.05
  
  # robust significance test
  t.rob[i] <- linearHypothesis(reg, "X = 1", white.adjust = "hc1")$'Pr(>F)'[2] < 0.05
  
}

# compute the fraction of false rejections
round(cbind(t = mean(t), t.rob = mean(t.rob)), 3)
#These results reveal the increased risk of falsely rejecting the null using the homoskedasticity-only standard error 
#for the testing problem at hand: with the common standard error, 7.3% of all tests falsely reject the null hypothesis.
#In contrast, with the robust test statistic we are closer to the nominal level of 5%
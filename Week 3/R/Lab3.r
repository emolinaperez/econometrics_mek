rm(list=ls())
#set your working directory
dir.wd<-'/Users/fabianfuentes/Library/CloudStorage/OneDrive-InstitutoTecnologicoydeEstudiosSuperioresdeMonterrey/cursos/Econometrics/econometrics_mek'
setwd(dir.wd)

#load library
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load('foreign','Hmisc', 'data.table')

#load data
Data.File<-"Week 3/data/database_angrist_levy_09.dta"
Data<-read.dta(Data.File, convert.dates = TRUE, convert.factors = TRUE,missing.type = FALSE,convert.underscore = FALSE, warn.missing.labels = TRUE)

# variables
colnames(Data)

# summary statistics
summary(Data)

# Table 1- Descriptive Statistics

# Panel A.2001
summary_table <- data.table(
  
  Variable = c("Bagrut rate", 
               "Arab school", "Religious school",
               "Father's education", "Mother's education", "Number of siblings", "Inmigrant", "Lagged Bagrut"),
  
  All_Mean = c(
    mean(Data$zakaibag[Data$panel == 1], na.rm = TRUE), # Indicates for Bagrut status
    mean(Data$semarab[Data$panel == 1], na.rm = TRUE),  # Arab school
    mean(Data$semrel[Data$panel == 1], na.rm = TRUE),   # Religious school
    mean(Data$educav[Data$panel == 1], na.rm = TRUE),   # Father's education
    mean(Data$educem[Data$panel == 1], na.rm = TRUE),   # Mother's education
    mean(Data$m_ahim[Data$panel == 1], na.rm = TRUE),   # Number of siblings
    mean(Data$ole5[Data$panel == 1], na.rm = TRUE),     # Indicates immigrants
    mean(Data$lagscore[Data$panel == 1], na.rm = TRUE)  # Lagged Bagrut score
    
  ),
  
  Boys_Mean = c(
    mean(Data$zakaibag[Data$panel == 1 & Data$boy == 1], na.rm = TRUE),
    mean(Data$semarab[Data$panel == 1 & Data$boy == 1], na.rm = TRUE),
    mean(Data$semrel[Data$panel == 1 & Data$boy == 1], na.rm = TRUE),
    mean(Data$educav[Data$panel == 1 & Data$boy == 1], na.rm = TRUE),
    mean(Data$educem[Data$panel == 1 & Data$boy == 1], na.rm = TRUE),
    mean(Data$m_ahim[Data$panel == 1 & Data$boy == 1], na.rm = TRUE),
    mean(Data$ole5[Data$panel == 1 & Data$boy == 1], na.rm = TRUE),
    mean(Data$lagscore[Data$panel == 1 & Data$boy == 1], na.rm = TRUE)
  ),
  
  Girls_Mean = c(
    mean(Data$zakaibag[Data$panel == 1 & Data$boy == 0], na.rm = TRUE),
    mean(Data$semarab[Data$panel == 1 & Data$boy == 0], na.rm = TRUE),
    mean(Data$semrel[Data$panel == 1 & Data$boy == 0], na.rm = TRUE),
    mean(Data$educav[Data$panel == 1 & Data$boy == 0], na.rm = TRUE),
    mean(Data$educem[Data$panel == 1 & Data$boy == 0], na.rm = TRUE),
    mean(Data$m_ahim[Data$panel == 1 & Data$boy == 0], na.rm = TRUE),
    mean(Data$ole5[Data$panel == 1 & Data$boy == 0], na.rm = TRUE),
    mean(Data$lagscore[Data$panel == 1 & Data$boy == 0], na.rm = TRUE)
  )
  
)

summary_table

# t-test to compare the means of semrel and semarab
t.test(Data$semrel[Data$panel==1], Data$semarab[Data$panel==1],
       alternative = c("two.sided"), conf.level = 0.95)

# t-test to compare the means of the treated variable across the groups defined by semarab
t.test(Data$treated[Data$panel==1] ~ Data$semarab[Data$panel==1],
       alternative = c("two.sided"),conf.level = 0.95)

# t-test to compare the means of the treated variable across the groups defined by semrel
t.test(Data$treated[Data$panel==1] ~ Data$semrel[Data$panel==1],
       alternative = c("two.sided"),conf.level = 0.95)

#Estimate liner model:
 model1<-lm(zakaibag ~ treated, data=subset(Data, panel == 1))
 summary(model1)

# mean of residuals
  mean(model1$residuals)

#checking equal variance of residuals
#  par(mfrow=c(2,2))
  plot(model1)

# no autocorrelation of residuals
 acf(model1$residuals)

#correlation between observables and residuals
 cor.test(Data$zakaibag[Data$panel==1], model1$residuals) #very bad assumptio breaks for this model

#what if we want a more comprehensive plot ?
#first, let's estimate a better model
 
# School covariates, quartile dumies, micro covariates, see Table - 2  
 
 ip <- sapply(grep("^_Ip", colnames(Data), value = TRUE), function(x) paste0("`", x, "`"))
 covar <- c("treated", "semarab", "semrel", "ls50", "ls75", "ls100", 
                     "educav", "educem", "ah4", "ole5",ip)
 
 fm2 <- as.formula(paste("zakaibag ~", paste(covar, collapse = " + ")))
 
 
  model2<-lm(fm2, data=subset(Data, panel == 1))
  summary(model2)
  
  Data.cor<-subset(Data, panel == 1)
  Data.cor$residuals<-model2$residuals
  
  cor(Data.cor[,c("zakaibag","treated", "semarab", "semrel", "ls50", "ls75", "ls100", "educav", "educem", "ah4", "ole5")],method='spearman')
  cor.test(Data.cor$treated, Data.cor$lagscore)
  
  p_load(psych)
  pairs.panels(Data.cor[,c("zakaibag","treated", "semarab", "semrel", "ls50", "ls75", "ls100", "educav", "educem", "ah4", "ole5")])

#Finally we can use diffent statistics to know how good our model is:
#AIC	Lower the better; The Akaike’s information criterion - AIC (Akaike, 1974)
#BIC	Lower the better; Bayesian information criterion - BIC (Schwarz, 1978)
#both measure of the goodness of fit of an estimated statistical model and can also be used for model selection. Both criteria depend on the maximized value of the likelihood function L for the estimated model.
#For model comparison, the model with the lowest AIC and BIC score is preferred.
 
 AIC(model1)
 BIC(model1)

 AIC(model2)
 BIC(model2)
 
 # Table 2 -  Treatment Efects ans especification check
 
 model1<-lm(zakaibag ~ treated + semarab + semrel, data=subset(Data, panel == 1))
 model2<-lm(zakaibag ~ treated + semarab + semrel + ls50 + ls75 + ls100 + educav + educem + ah4 + ole5, data=subset(Data, panel == 1))

 model3<-lm(paste0("zakaibag ~ treated + semarab + semrel +", paste(ip, collapse = " + ")) , data=subset(Data, panel == 1))
 model4<-lm(paste("zakaibag ~", paste(covar, collapse = " + ")), data=subset(Data, panel == 1))
 
 p_load(stargazer)
 
 stargazer(model1,model2,model3,model4,type = "text")
 
 # AIC
 list(AIC(model1), AIC(model2), AIC(model3), AIC(model4))
 
 # BIC
 list(BIC(model1), BIC(model2), BIC(model3), BIC(model4))
 
#back to hypothesis testing  
p_load(AER)
p_load(scales)

# load the `CASchools` dataset
data(CASchools)

# add student-teacher ratio
CASchools$STR <- CASchools$students/CASchools$teachers

# add average test-score
CASchools$score <- (CASchools$read + CASchools$math)/2

# estimate the model
linear_model <- lm(score ~ STR, data = CASchools)          

# print the summary of the coefficients to the console
summary(linear_model)$coefficients

#Note that summary() does not perform the normal approximation but calculates 
#values using the t distribution instead. Generally, the degrees of freedom of the assumed  
#distribution are determined in the following manner: DF = n-(number of covariates)-1

# determine residual degrees of freedom
linear_model$df.residual

# Plot the standard normal on the support [-6,6]
t <- seq(-6, 6, 0.01)

plot(x = t, 
     y = dnorm(t, 0, 1), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     yaxs = "i", 
     axes = F, 
     ylab = "", 
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=-4.75"), 
     cex.lab = 0.7,
     cex.main = 1)

tact <- -4.75

axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)

# Shade the critical regions using polygon():

# critical region in left tail
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0), 
        col = 'orange')

# critical region in right tail

polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0), 
        col = 'orange')

# Add arrows and texts indicating critical regions and the p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)

arrows(-5, 0.16, -4.75, 0, length = 0.1)
arrows(5, 0.16, 4.75, 0, length = 0.1)

text(-3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)

text(-5, 0.18, 
     labels = expression(paste("-|",t[act],"|")), 
     cex = 0.7)
text(5, 0.18, 
     labels = expression(paste("|",t[act],"|")), 
     cex = 0.7)

# Add ticks indicating critical values at the 0.05-level, t^act and -t^act 
rug(c(-1.96, 1.96), ticksize  = 0.145, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize  = -0.0451, lwd = 2, col = "darkgreen")

#To get a better understanding of confidence intervals we conduct another simulation study.
# set seed for reproducibility
set.seed(4)

# generate and plot the sample data
Y <- rnorm(n = 100, 
           mean = 5, 
           sd = 5)

plot(Y, 
     pch = 19, 
     col = "steelblue")
#we can compute this interval in R by hand. 
cbind(CIlower = mean(Y) - 1.96 * 5 / 10, CIupper = mean(Y) + 1.96 * 5 / 10)

#Knowing that mu=5 we see that, for our example data, the confidence interval covers true value.

set.seed(1)

# initialize vectors of lower and upper interval boundaries
lower <- numeric(10000)
upper <- numeric(10000)

# loop sampling / estimation / CI
for(i in 1:10000) { 
  Y <- rnorm(100, mean = 5, sd = 5)
  lower[i] <- mean(Y) - 1.96 * 5 / 10
  upper[i] <- mean(Y) + 1.96 * 5 / 10  
}

# join vectors of interval bounds in a matrix
CIs <- cbind(lower, upper)

#do all CIs cover the true value of mu?
mean(CIs[, 1] <= 5 & 5 <= CIs[, 2])

#we see that not in all cases this is true 

# identify intervals not covering mu
ID <- which(!(CIs[1:100, 1] <= 5 & 5 <= CIs[1:100, 2]))

# initialize the plot
plot(0, 
     xlim = c(3, 7), 
     ylim = c(1, 100), 
     ylab = "Sample", 
     xlab = expression(mu), 
     main = "Confidence Intervals")

# set up color vector
colors <- rep(gray(0.6), 100)
colors[ID] <- "red"

# draw reference line at mu=5
abline(v = 5, lty = 2)

# add horizontal bars representing the CIs
for(j in 1:100) {
  
  lines(c(CIs[j, 1], CIs[j, 2]), 
        c(j, j), 
        col = colors[j], 
        lwd = 2)
  
}

# compute 95% confidence interval for coefficients in 'linear_model'
confint(linear_model)


# compute 95% confidence interval for coefficients in 'linear_model' by hand
lm_summ <- summary(linear_model)

#compute those confidence intervals by hand
c("lower" = lm_summ$coef[2,1] - qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2],
  "upper" = lm_summ$coef[2,1] + qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2])

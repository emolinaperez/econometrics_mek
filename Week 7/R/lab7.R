rm(list=ls())

# load packages
if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load('ggplot2','haven', 'fixest', 'modelsummary', 'olsrr')

#load data
Data.File<-"https://raw.githubusercontent.com/emolinaperez/econometrics_mek/main/Week%207/data/AJR2001.dta"
Data<-read_dta(Data.File)

head(Data)

dim(Data)

str(Data)

# Fig1b: Remove latitude and continent dummies

ggplot(Data, aes(x = logmort0, y = risk)) +
  # Add scatter points
  geom_point(size = 2) +
  # Add text labels for each point
  geom_text(aes(label = shortnam), 
            vjust = -0.5,  # Adjust vertical position of text
            size = 3) +  # Text size
  # Add linear fit line
  geom_smooth(method = "lm", 
              se = FALSE,  # Without error bands
              color = "blue") +
  # Customize titles and labels
  labs(title = "Figure 1a: Without Controls",
       x = "Log Settler Mortality",
       y = "Expropriation Risk") +
  # Customize theme and scale
  theme_classic() +  # White background and clean design
  scale_y_continuous(breaks = seq(4, 10, by = 2)) 


# Fig1b: Remove latitude and continent dummies

model1 <- lm(risk ~ asia + africa + other + latitude, data = Data)
Data$Rrisk <- residuals(model1)

# Second regression to get Rlmort
model2 <- lm(logmort0 ~ asia + africa + other + latitude, data = Data)
Data$Rlmort <- residuals(model2)

ggplot(Data, aes(x = Rlmort, y = Rrisk)) +
  # Add scatter points
  geom_point(size = 2) +
  # Add text labels for each point
  geom_text(aes(label = shortnam), 
            vjust = -0.5,  # Adjust vertical position of text
            size = 3) +  # Text size
  # Add linear fit line
  geom_smooth(method = "lm", 
              se = FALSE,  # Without error bands
              color = "blue") +
  # Customize titles and labels
  labs(title = "Figure 1b: Controlling for Latitude and Continent Dummies",
       x = "Residual Log Settler Mortality",
       y = "Residual Expropriation Risk") +
  # Customize theme and scale
  theme_classic() +  # White background and clean design
  scale_y_continuous(breaks = seq(-2, 3, by = 2))


# TABLE 4—IV REGRESSIONS OF LOG GDP PER CAPITA

# Panel C: OLS
ols1 <- lm(loggdp ~ risk, data = Data)
ols2 <- lm(loggdp ~ risk + latitude, data = Data)
ols3 <- lm(loggdp ~ risk + latitude + africa + asia + other, data = Data)

# table
modelsummary(list(ols1,ols2, ols3),
             coef_omit = "(Intercept)",
             coef_map = c(
               "risk"= "Expropriation risk",
               "latitude" = "Latitude",
               "africa" = "Africa Dummy",
               "asia" = "Asia Dummy",
               "other" = "Other Continent Dummy"),
             stars = TRUE)

# Panel B: First Stage
fs1 <- feols(risk ~ logmort0, data = Data)
fs2 <- feols(risk ~ logmort0 + latitude, data = Data)
fs3 <- feols(risk ~ logmort0 + latitude + africa + asia + other, data = Data)

# table
modelsummary(list(fs1,fs2, fs3),
             coef_omit = "(Intercept)",
             coef_map = c(
               "logmort0"= "Log European settler mortality",
               "latitude" = "Latitude",
               "africa" = "Africa Dummy",
               "asia" = "Asia Dummy",
               "other" = "Other Continent Dummy"),
             stars = TRUE)

# Panel A: 2SLS (IV) regressions
iv1 <- feols(loggdp ~ 1 | risk ~ logmort0, data = Data)
iv2 <- feols(loggdp ~ latitude | risk ~ logmort0, data = Data)
iv3 <- feols(loggdp ~ latitude + africa + asia + other | risk ~ logmort0, data = Data)

# table
modelsummary(list(iv1,iv2, iv3),
             coef_omit = "(Intercept)",
             coef_map = c(
               "fit_risk"= "Expropriation risk",
               "latitude" = "Latitude",
               "africa" = "Africa Dummy",
               "asia" = "Asia Dummy",
               "other" = "Other Continent Dummy"),
             stars = TRUE)




#define controls 
controls <- c("campaign", 
              "source0",
              "slave",
              "neoeuro",
              "asia",
              "africa",
              "other",
              "campaignsj",
              "campaignsj2",
              "wandcafrica",
              "wacacontested")

#esimate correlation matrix, subset to low correlation covariates 
cor_table <- cor(Data[,controls],use='complete.obs')

cor_table<- apply(cor_table,c(1,2),function(x){ifelse(abs(x)>0.5,0,1)})

dim(unique(cor_table))
rowSums(cor_table[,controls])
length(controls)

#create all possible combinations 
vars <-  list()
for (i in 1: length(controls))
{
  pivot <- c(controls[i],"")
  vars <- append(vars, list(pivot))
}


#create all combinations 

all.vars<-expand.grid(vars) # all factorial combinations 
all.vars<-data.frame(all.vars)
head(all.vars)
dim(all.vars)


#estimate number of terms in models 
all.vars$N.vars<-rowSums(apply(all.vars,2,function(x){ifelse(x!="",1,0)}))
#subset to models with at least 2 vars
dim(all.vars) 
all.vars<-subset(all.vars,N.vars>=2)
dim(all.vars)
all.vars$id <-1:nrow(all.vars) 
ids <- all.vars[,c("N.vars","id")]
all.vars$id<-NULL
all.vars$N.vars<-NULL
all.vars<-apply(all.vars,2,function(x) {as.character(x)})
#explore object 
head(all.vars)


### lets build a function to evaliate our model 
performance_ols<-function(dependent.variable,treatment,controls,DataSet,id)
{
  #test code 
  #dependent.variable <- "earnq16" 
  #treatment <- "treatmnt"
  #controls <- all.vars[4,]
  #DataSet <- Data 
  model<-as.formula(paste(dependent.variable,"~",paste(controls,collapse="+"),"+",treatment,sep=""))
  linear.model<-lm(model, data=DataSet)
  my.SSR <- deviance(linear.model) #residual sum of squares
  my.LL <- logLik(linear.model) # log likelihood statistic
  my.RSquared <- summary(linear.model)$r.squared
  my.aic <- AIC(linear.model) # Akaike information criterion
  my.treatment.effect<-summary(linear.model)$coefficients[treatment,"Estimate"]
  my.treatment.StdEror<-summary(linear.model)$coefficients[treatment,"Std. Error"]
  #my.VIF<-length(which( vif(linear.model)>2.0 ) ) # number of covariates that exceed the 2 threshold as a proxy for multicollinarity
  #  my.cor.e.y<-cor(subset(DataSet[[dependent.variable]],complete.cases(Data[,c(dependent.variable,treatment,subset(controls,controls!=""))])),as.numeric(linear.model$residuals))
  #  my.cor.e.t<-cor(subset(DataSet[[treatment]],complete.cases(Data[,c(dependent.variable,treatment, subset(controls,controls!=""))])),as.numeric(linear.model$residuals))
  my.Percent.Coeff.Sig<-length(which(summary(linear.model)$coefficients[,"Pr(>|t|)"]<0.05))/length(summary(linear.model)$coefficients[,"Pr(>|t|)"])
  results<-data.frame(SSR=my.SSR,
                      LL=my.LL,
                      RSquared=my.RSquared,
                      AIC=my.aic,
                      Effect=my.treatment.effect, 
                      Std.Error.Effect=my.treatment.StdEror,
                      #Eps.T=my.cor.e.t,
                      #Eps.Y=my.cor.e.y,
                      #VIF=my.VIF,
                      Percent.Sig=my.Percent.Coeff.Sig ,
                      id = id,
                      #Controls=paste(controls,collapse="+"),
                      row.names=NULL)
  return(results)
}


#test the function 

summary(lm(loggdp ~ risk, data = Data))

i<- 1
performance_ols("loggdp","risk",as.character(all.vars[i,]),Data,i)


head(ids)
target_ids<-subset(ids,N.vars==3)$id
length(target_ids)
#for (i in 1:dim(all.vars)[1])
#for (i in 1:10)
results <- list()
for ( i in target_ids)
{
  out <- performance_ols("loggdp","risk",all.vars[i,],Data,i) 
  results <- append(results, list(out)) 
} 


#post-process 
#results
results <- do.call("rbind",results)
dim(results)
head(results)
summary(results)

#choose a model  
decision <- subset(results, Percent.Sig>=0.8)
decision <- subset(decision, AIC<as.numeric(quantile(results$AIC,0.25)))
decision <- decision[order(decision$Std.Error.Effect),]
decision <- decision[order(decision$AIC),]
head(decision)
dim(decision)


target_models <- c(72,74)
model1<-as.formula(paste("loggdp","~",paste(as.character(all.vars[target_models[1],]),collapse="+"),"+","risk",sep=""))
model2<-as.formula(paste("loggdp","~",paste(as.character(all.vars[target_models[2],]),collapse="+"),"+","risk",sep=""))
lm(model1,data=Data)
lm(model2,data=Data)


#######
#stepwise regresssion 
#######
controls

model <- lm(loggdp ~ risk + ., data = Data [,c("loggdp","risk",controls)])

#All Possible Regression
#All subset regression tests all possible subsets of the set of potential independent variables. 
#If there are K potential independent variables (besides the constant), then there are 2k #distinct subsets of them to be tested. 
#"For example, if you have 10 candidate independent variables, the number of subsets to be tested is 210 ,which is 1024, and if you have 20 candidate variables, the number is 220
#,which is more than one millio

all <- ols_step_all_possible(model)
head(all)

#Best Subset Regression
#select the subset of predictors that do the best at meeting some well-defined objective criterion, 
#such as having the largest R2 value or the smallest MSE, Mallow’s Cp or AIC.
#
best_subset <- ols_step_best_subset(model)
best_subset

#Stepwise Selection
#Stepwise regression is a method of fitting regression models that involves the iterative selection of independent variables to use in a model.
#It can be achieved through forward selection, backward elimination, or a combination of both methods.
#The forward selection approach starts with no variables and adds each new variable incrementally, testing for statistical significance,
#while the backward elimination method begins with a full model and then removes the least statistically significant variables one at a time.

# stepwise forward regression
step_forward <- ols_step_forward_p(model)
step_forward

# stepwise backward regression
ols_step_backward_p(model)

#The criteria for selecting variables may be one of the following:
#p value
#akaike information criterion (aic)
#schwarz bayesian criterion (sbc)
#sawa bayesian criterion (sbic)
#r-square
#adjusted r-square

#for example using AIC
ols_step_both_aic(model)


#show progress  
# adjusted r-square 
ols_step_forward_adj_r2(model, progress = TRUE)

#plot output
# adjusted r-square 
k <- ols_step_forward_adj_r2(model)
plot(k)

####
# Cross Validation using library caret  
#####
#install.packages("caret")
library(caret)
#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)

#fit a regression model and use k-fold CV to evaluate performance
model <- train(loggdp ~ ., 
               data = Data [complete.cases(Data),c("loggdp","risk",controls)],
               method = "rf")

#view summary of k-fold CV               
print(model)

#No pre-processing occured. That is, we didn’t scale the data in any way before fitting the models.
#The resampling method we used to evaluate the model was cross-validation with 5 folds.
#The sample size for each training set was 3954
#RMSE: The root mean squared error. This measures the average difference between the predictions made by the model and the actual observations. The lower the RMSE, the more closely a model can predict the actual observations.
#Rsquared: This is a measure of the correlation between the predictions made by the model and the actual observations. The higher the R-squared, the more closely a model can predict the actual observations.
#MAE: The mean absolute error. This is the average absolute difference between the predictions made by the model and the actual observations. The lower the MAE, the more closely a model can predict the actual observations.

#view final model
model$finalModel
mean(model$finalModel$mse)
names(model)


###
#Cross Validation using library cv
##
#install.packages("ISLR2")
library("ISLR2")
data("Auto", package="ISLR2")
head(Auto)
plot(mpg ~ horsepower, data=Auto)

#plot various alternatives 
plot(mpg ~ horsepower, data=Auto)
horsepower <- with(Auto, 
                   seq(min(horsepower), max(horsepower), 
                       length=1000))
for (p in 1:5){
  m <- lm(mpg ~ poly(horsepower,p), data=Auto)
  mpg <- predict(m, newdata=data.frame(horsepower=horsepower))
  lines(horsepower, mpg, col=p + 1, lty=p, lwd=2)
}
legend("topright", legend=1:5, col=2:6, lty=1:5, lwd=2,
       title="Degree", inset=0.02)

#cross/validation
#install.packages("cv")
library("cv") # for mse() and other functions

var <- mse <- numeric(10)
for (p in 1:10){
  m <- lm(mpg ~ poly(horsepower, p), data=Auto)
  mse[p] <- mse(Auto$mpg, fitted(m))
  var[p] <- summary(m)$sigma^2
}

plot(c(1, 10), range(mse, var), type="n",
     xlab="Degree of polynomial, p",
     ylab="Estimated Squared Error")
lines(1:10, mse, lwd=2, lty=1, col=2, pch=16, type="b")
lines(1:10, var, lwd=2, lty=2, col=3, pch=17, type="b")
legend("topright", inset=0.02,
       legend=c(expression(hat(sigma)^2), "MSE"),
       lwd=2, lty=2:1, col=3:2, pch=17:16)

#cross validation with OLS
m.auto <- lm(mpg ~ poly(horsepower, 2), data=Auto)
summary(m.auto)

#train and test set  
cv(m.auto)
as.numeric(cv(m.auto)["CV crit"])

#leave one out
cv(m.auto, k="loo")

#Comparing competing models
for (p in 1:10){
  assign(paste0("m.", p),
         lm(mpg ~ poly(horsepower, p), data=Auto))
}
objects(pattern="m\\.[0-9]")
summary(m.2)

cv.auto.10 <- cv(models(m.1, m.2, m.3, m.4, m.5,
                        m.6, m.7, m.8, m.9, m.10),
                 data=Auto, seed=2120)
cv.auto.10[1:2]
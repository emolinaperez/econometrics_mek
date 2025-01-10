#This is tutorial one
#set your working directory
dir.wd<- "/Users/edmun/Library/CloudStorage/OneDrive-Personal/Edmundo-ITESM/2.Cursos Impartidos/Econometrics MEK/MEK/econometrics_mek/Week 1/"
setwd(dir.wd)
#do you want to check if this was done correctly? No problem! use the following function:
getwd()
#did the directory you set using the function setwd() matches? if yes, well done! if not, try it again
#Now let's read some data in stata format,
#For this, you will install your first library to R,
# Go to "packages in the top menu"
# Select a CRAN mirror, these are servers in which all R stuff is provided
# Select package "foreign"
# Pay attention to the message displayed at the bottom, if it reads something like:
#The downloaded binary packages are in
#        C:\Users\emolina\AppData\Local\Temp\RtmpMr6plU\downloaded_packages
#Then your package is not where you want it to be,
#We need to save your package in the location in which your R library is, type the following function to know where your library lives:
.libPaths()
#when I execute this in my computer I get: "C:/Users/emolina/Documents/R/win-library/3.1", you should get something similar,
#ok, then let's solve the issue with the library, go to the directory "C:\Users\emolina\AppData\Local\Temp\RtmpMr6plU\downloaded_packages"
# unzip the foreging package, and move the folder "foreign" into your R library in this location: "C:/Users/emolina/Documents/R/win-library/3.1"
# done now you have the foreign library in R, let's check if this is the case, run the following function
install.packages("foreign")
library(foreign)
#if you get no message, everything worked fine, you may get some warning message about the version you are using in R, but that's no problem for now,
#loading libraries is important, as it allows you to use new functions created by other users,
# this particular library has the function we need to read .dta files, let's do this in the example file "example.dta":
# go to canvas and you'll find the dataset needed there
# now, let's loading into R and save is as an object named "data"
data.t1<-read.dta("example.dta",
                   convert.dates = TRUE, 
                   convert.factors = TRUE,
                   missing.type = FALSE,
                   convert.underscore = FALSE,
                   warn.missing.labels = TRUE)
#wait a minute!!, what if my data is in *.csv format??? can R read this stuff, yes it can, check it out:
#data.t1<-read.csv("example.csv")
#ok, let's move on,
#let's check which type of object data is, we can use the function class:
class(data.t1)
#let's see the first rows in this data.frame
head(data.t1)
#how many rows and columns my data has?
dim(data.t1)
#let's get summary statistics for all variables
summary(data.t1)
#lets see a histogram of these variables
hist(data.t1$offense)
hist(data.t1$policy)
hist(data.t1$caruse)
#what about scatter plots
pairs(~educ+age+caruse+fast+unsafe,data=data.t1, main="my test")
pairs(~educ+age+caruse+fast+unsafe+limit+offense+policy+contrib+limwar+accept,data=data.t1, main="Check out this scatter plot")
#ok, enough with the graphics let's get back to business,
#let's run a regression:
lm(accept~gender+educ+age+caruse+fast+unsafe+limit+offense,data = data.t1)
#cool, do you want to know how long it took to run this regression? let's check it out
system.time(lm(accept~gender+educ+age+caruse+fast+unsafe+limit+offense,data = data.t1))
#well it says this was almost zero!, system.time is a useful function to compare options when programming and design efficient code
# of coruse running lm by itself gives you litle information, let's get more information of this regression,
# first let's save the regression as an object,
my.first.Rregression<-lm(accept~gender+educ+age+caruse+fast+unsafe+limit+offense,data = data.t1)
# let's check which type of object this is:
class(my.first.Rregression)
#ok, give me more information:
summary(my.first.Rregression)
#ok, now let's access these data directly,
#first let's check which data is inside "my.first.Rregression"
names(my.first.Rregression)
#Nice, so let's save each independent piece in an object:
my.Coef<-my.first.Rregression$coefficients
my.Coef
class(my.Coef)
my.Coef["gender"]
#Ok, let's get all the other stuff we typically need:
  my.SSR <- deviance(my.first.Rregression) #residual sum of squares
  my.SSR
  my.LL <- logLik(my.first.Rregression) # log likelihood statistic
  my.LL
  my.DegreesOfFreedom <- my.first.Rregression$df # degrees of fredom
  my.DegreesOfFreedom
  my.Yhat <- my.first.Rregression$fitted.values # vector of fitted values
  my.Yhat
  my.Coef <- my.first.Rregression$coefficients # coefficients
  my.Coef
  my.Resid <- my.first.Rregression$residuals # vector of residuals
  my.Resid
  my.s <- summary(my.first.Rregression)$sigma
  my.s
  my.RSquared <- summary(my.first.Rregression)$r.squared
  my.RSquared
  my.CovMatrix <- my.s^2*summary(my.first.Rregression)$cov # variance-covariance matrix of
  my.CovMatrix
  my.aic <- AIC(my.first.Rregression) # Akaike information criterion
  my.aic

#probably you want to do this more automatically, let's write a function
#the function below does all the process above
#can you understand the syntax ?
my.first.function<-function(dependent.variable,independent.variables)
{
 paste(independent.variables,collapse = "+")
 model <- paste(dependent.variable,"~",paste(independent.variables,collapse = "+"),collapse="")
 linear.model<-lm(as.formula(model), data=data.t1)
  my.SSR <- deviance(linear.model) #residual sum of squares
  my.LL <- logLik(linear.model) # log likelihood statistic
  my.DegreesOfFreedom <- linear.model$df # degrees of fredom
  my.Yhat <- linear.model$fitted.values # vector of fitted values
  my.Coef <- linear.model$coefficients # coefficients
  my.Resid <- linear.model$residuals # vector of residuals
  my.s <- summary(linear.model)$sigma
  my.RSquared <- summary(linear.model)$r.squared
  my.CovMatrix <- my.s^2*summary(linear.model)$cov # variance-covariance matrix of
  my.aic <- AIC(linear.model) # Akaike information criterion
  results<-data.frame(SSR=my.SSR,LL=my.LL,DF=my.DegreesOfFreedom,RSquared=my.RSquared,AIC=my.aic)
  return(results)
}

#since we have defined this function, now we can use it:
# the function has two arguments:
# the first argument indicates the dependent variable
# the second arugment indicates the independent variables
#let's try this example:
 my.first.function("accept",c("gender","educ","age","caruse","fast","unsafe","limit","offense"))
#let's try this other example
 my.first.function("accept",c("gender","educ","age","caruse"))

#interesting, can we do this over all possible specifications for the data set we have?
#let's generate a table with all possible combinations
vars<-list(var1=c(colnames(data.t1)[1],""),
           var2=c(colnames(data.t1)[2],""),
	         var3=c(colnames(data.t1)[3],""),
	         var4=c(colnames(data.t1)[4],""),
	         var5=c(colnames(data.t1)[5],""),
	         var6=c(colnames(data.t1)[6],""),
	         var7=c(colnames(data.t1)[7],""),
	         var8=c(colnames(data.t1)[8],""),
	         var9=c(colnames(data.t1)[9],""),
	         var10=c(colnames(data.t1)[10],""),
	         var11=c(colnames(data.t1)[11],""),
	         var13=c(colnames(data.t1)[13],""))
all.vars<-expand.grid(vars)

#now let's estimate OLS for all possible combinations 

#first let's try one case
my.first.function("accept",c(
                             as.character(all.vars[1,'var1']),
                             as.character(all.vars[1,'var2']),
                             as.character(all.vars[1,'var3']),
                             as.character(all.vars[1,'var4']),
                             as.character(all.vars[1,'var5']),
                             as.character(all.vars[1,'var6']),
                             as.character(all.vars[1,'var7']),
                             as.character(all.vars[1,'var8']),
                             as.character(all.vars[1,'var9']),
                             as.character(all.vars[1,'var10'])
                             )
                  )


#now let's use the apply function to this over the entire possible combinations 


regress<-apply(all.vars[1:20,],1,function(x){
  
                            my.first.function("accept",c(
                                                         as.character(x['var1']),
                                                         as.character(x['var2']),
                                                         as.character(x['var3']),
                                                         as.character(x['var4']),
                                                         as.character(x['var5']),
                                                         as.character(x['var6']),
                                                         as.character(x['var7']),
                                                         as.character(x['var8']),
                                                         as.character(x['var9']),
                                                         as.character(x['var10'])
                                                        )
                                              )
                               }
                 )

#regress is a list, you can check it out
class(regress)

#you can put everything on a table with do.call 

regress<-do.call('rbind',regress)

subset(regress,RSquared==max(regress$RSquared))
#find the model with the highes R squared 




#Lab 2
#set your working directory
 dir.wd<-r'(/Users/fabianfuentes/Library/CloudStorage/OneDrive-InstitutoTecnologicoydeEstudiosSuperioresdeMonterrey/cursos/Econometrics/econometrics_mek/Week 2/)'
#dir.wd<-
 setwd(dir.wd)
#load library for importing database into R (from stata format)
 library(foreign)
#load data
 jobcorpsfile<-"data/jobcorps.dta"
 JobCorps<-read.dta(jobcorpsfile, convert.dates = TRUE, convert.factors = TRUE,missing.type = FALSE,convert.underscore = FALSE, warn.missing.labels = TRUE)
#Explore data file
 head(JobCorps)
 colnames(JobCorps)
 dim(JobCorps)
 attributes(JobCorps)
 str(JobCorps)
 summary(JobCorps)

#Explore in class characteristics of differnet variables 

#compare hispanics in control and treatment group
#first compare means
 # in control group
  mean(JobCorps[JobCorps$treatmnt==0,"hispanic"] ,na.rm=TRUE)
  sum(JobCorps[JobCorps$treatmnt==0,"hispanic"] ,na.rm=TRUE)
 # in treatment group
  mean(JobCorps[JobCorps$treatmnt==1,"hispanic"],na.rm=TRUE)
  sum(JobCorps[JobCorps$treatmnt==1,"hispanic"] ,na.rm=TRUE)


#Difference in means test
  mytest <- with( JobCorps, t.test(hispanic~treatmnt,alternative = c("two.sided"),conf.level = 0.95))
  t.test(JobCorps$hispanic~JobCorps$treatmnt,alternative = c("two.sided"),conf.level = 0.95)
  t.test(JobCorps$male~JobCorps$treatmnt,alternative = c("two.sided"),conf.level = 0.95)

#conclusion is not possible to reject null hypothesis that hispanic in both groups are the same

#use OLS model to see of randomization was sucessful
 #first define vector of variables of interest
   target.vars<-colnames(JobCorps)[19:36]
   target.vars<-subset(colnames(JobCorps), grepl("earnq",colnames(JobCorps))==FALSE)
 #run the regression, first a simple example with 3 observable caracteristics
   model1<-lm(treatmnt~ white+black+hispanic, data=JobCorps)
   summary(model1)
   plot(model1)
 #now all variables
  #first save the model
   formula.model2<-as.formula(paste("treatmnt","~",paste(target.vars,collapse="+"),sep=""))
  #second run regression
   model2<-lm(formula.model2,data=JobCorps)
   summary(model2)

#selection on observables
  formula.model3<-as.formula(paste("attrit","~",paste(target.vars,collapse="+"),sep=""))
#second run regression
  model3<-lm(attrit~ white+black+hispanic, data=JobCorps)
  model3<-lm(formula.model3,data=JobCorps)
  summary(model3)
   dim(JobCorps)
   test<-subset(JobCorps,complete.cases(JobCorps[,target.vars])==TRUE)
   dim(test)
   cor(test[,target.vars])


#Average Treatment Effect  
  formula.model_ATE<-as.formula(earnq16~treatmnt)
  model_ATE<-lm(formula.model_ATE,data=JobCorps)
  summary(model_ATE) 

#Is this a really ATE ? 
#Effect on the Treated 
  summary(subset(JobCorps,treatmnt==1)$earnq16)
  ToT<-mean(subset(JobCorps,treatmnt==1)$earnq16,na.rm=TRUE)  # ToT: Average Effect of Treatment on the Treated
  ToT

#Effect on the non-treated 
  summary(subset(JobCorps,treatmnt==0)$earnq16)
  NT<-mean(subset(JobCorps,treatmnt==0)$earnq16,na.rm=TRUE)  # ToT: Average Effect of Treatment on the Treated
  NT

#ATE , maybe 
  ToT-NT

#compare this to regression coefficient 
 model_ATE$$coefficients
 
#Randomization seems to be working  
 summary(model_ATE$residuals) #what is the expected value? 
 #is this correlated with the treatment 
 cor(model_ATE$residuals,subset(JobCorps,complete.cases(JobCorps[,c("earnq16","treatmnt")])==TRUE)$treatmnt)
 #no correlation at all, what does this mean? 

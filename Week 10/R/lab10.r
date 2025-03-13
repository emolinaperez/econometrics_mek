#set your working directory
# dir.wd<-r"()"
# setwd(dir.wd)

#################################################################
# Lab 10: Resampling methods
################################################################

# Read your data 
data <- read.csv("Week 10/data/cps09mar.csv") 
head(data)


model_1 <- lm(log(earnings) ~ education, data = data)
summary(model_1)

# Subset data 
data_subset <-  subset(data,marital==1 & race==2 & female==1 & union==1 & education>=12)
dim(data_subset)

#normal regression 
lm1 <- lm(log(earnings) ~ education, data = data_subset)
summary(lm1)

#remove one row randonmly 
set.seed(14252)
jk_sample <- sample(1:nrow(data_subset),nrow(data_subset)-1)
length(jk_sample)
nrow(data_subset)
lmjk <- lm(log(earnings) ~ education, data = data_subset[jk_sample,])
summary(lmjk)


# Regression with jackknife variance estimation
#install.packages("bootstrap")
library(bootstrap)

#thetha
regression_func <- function(x,data) {
  model <- lm(log(earnings) ~ education, data = data_subset[as.numeric(x),])
  as.numeric(coef(model)["education"])
}

# Perform jackknife
set.seed(1234)
jackknife_results <- jackknife(1:18,regression_func,data_subset)

# Display jackknife results
print(jackknife_results)
mean(jackknife_results$jack.values)

#Bootstrap 

# Load required packages
library(boot)

#
regression_func <- function(data,x) {
  model <- lm(log(earnings) ~ education, data = data[as.numeric(x),])
  as.numeric(coef(model)["education"])
}

# Bootstrap analysis with 10000 replications
#boot_result <- boot(data = data_subset, statistic = boot_func, R = 10000)
boot_result <- boot(data = data_subset, statistic = regression_func, R = 10000)
boot_result
boot.ci(boot_result, conf=0.95, type="bca")


#Exploring more, bootstrap, step-by-step 

# Generating data
set.seed(555555)
n <- 1000
x <- rnorm(n)
y <- x + rnorm(n)


#Models of population and sample
population.data <- as.data.frame(cbind(x, y))
population.model <- lm(y ~ x, population.data)
summary(population.model)


#Sampling the data
sample.data <- population.data[sample(nrow(population.data), 20, replace = TRUE), ]
sample.model <- lm(y ~ x, data = sample.data)
summary(sample.model)


#Plotting the models
plot(y ~ x, col = "gray", main = 'Population and Sample Regressions')
abline(coef(population.model)[1], coef(population.model)[2], col = "red")
abline(coef(sample.model)[1],
       coef(sample.model)[2],
       col = "blue",
       lty = 2)
legend(
  "topleft",
  legend = c("Sample", "Population"),
  col = c("red", "blue"),
  lty = 1:2,
  cex = 0.8)


#The bootstrap regression
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL

for (i in 1:1000) {
  sample_d <- sample.data[sample(1:nrow(sample.data), nrow(sample.data), replace = TRUE), ]  
  model_bootstrap <- lm(y ~ x, data = sample_d)  
  sample_coef_intercept <- c(sample_coef_intercept, model_bootstrap$coefficients[1])
  sample_coef_x1 <- c(sample_coef_x1, model_bootstrap$coefficients[2])
}

coefs <- rbind(sample_coef_intercept, sample_coef_x1)


# Combining the results in a table
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1))
knitr::kable(round(
  cbind(
    population = coef(summary(population.model))[, 1],
    sample = coef(summary(sample.model))[, 1],
    bootstrap = means.boot),4), 
  "simple", caption = "Coefficients in different models")

confint(population.model)
confint(sample.model)
a <-cbind(
    quantile(sample_coef_intercept, prob = 0.025),
    quantile(sample_coef_intercept, prob = 0.975))
b <-cbind(
     quantile(sample_coef_x1, prob = 0.025),
     quantile(sample_coef_x1, prob = 0.975))

c <-
  round(cbind(
    population = confint(population.model),
    sample = confint(sample.model),
    boot = rbind(a, b)), 4)
colnames(c) <- c("2.5 %", "97.5 %",
                 "2.5 %", "97.5 %",
                 "2.5 %", "97.5 %")
knitr::kable(rbind(
  c('population',
    'population',
    'sample',
    'sample',
    'bootstrap',
    'bootstrap'),c))




#Predicting on new data
new.data <- seq(min(x), max(x), by = 0.05)
conf_interval <-
  predict(
    sample.model,
    newdata = data.frame(x = new.data),
    interval = "confidence",
    level = 0.95)


#Plotting the results on the project step-by-spet
plot(
  y ~ x,
  col = "gray",
  xlab = "x",
  ylab = "y",
  main = "Compare regressions")
apply(coefs, 2, abline, col = rgb(1, 0, 0, 0.03))
abline(coef(population.model)[1], coef(population.model)[2], col = "blue")
abline(coef(sample.model)[1],
       coef(sample.model)[2],
       col = "black",
       lty = 2, lwd=3)
abline(mean(sample_coef_intercept),
       mean(sample_coef_x1),
       col = "green",
       lty = 4, lwd=3)
lines(new.data, conf_interval[, 2], col = "black", lty = 3, lwd=3)
lines(new.data, conf_interval[, 3], col = "black", lty = 3, lwd=3)
legend("topleft",
       legend = c("Bootstrap", "Population", 'Sample'),
       col = c("red", "blue", 'green'),
       lty = 1:3,
       cex = 0.8)
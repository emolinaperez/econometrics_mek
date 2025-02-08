rm(list=ls())

# load packages
if (!require(pacman)) install.packages("pacman") 
p_load('dplyr','data.table','haven','ggplot2', 'stargazer','sandwich','lmtest', 'estimatr', 'clubSandwich', 'modelsummary') # clubSandwich: for clustered robust standard errors

# load data
df <- read_dta("Week 5/data/forpeerpapers.dta") %>% as.data.table()

# Figure 1. Distribution of Initial Test Scores (all schools)

# Create the trackinggroup variable
df$trackinggroup <- ifelse(df$tracking == 0, "Non-Tracking Schools", "Tracking Schools")

# Create the histogram
ggplot(df, aes(x = std_mark)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black")  +
  facet_wrap(~ trackinggroup) +
  xlab("") +
  theme_minimal()


# Table 2—Overall Effect of Tracking
# Panel A. Short-run effects (after 18 months in program)

df[, girl_tracking := girl * tracking]

names <- c("bottom", "second", "third", "top")  

for (name in names) {
  df[, paste0(name, "quarter_tracking") := get(paste0(name, "quarter")) * tracking]
}

# Total score

df[, mean_total := df[tracking == 0, mean(stdR_totalscore, na.rm = TRUE)]]
df[, sd_total := df[tracking == 0, sd(stdR_totalscore, na.rm = TRUE)]]

ind_controls <- c('girl', 'percentile', 'agetest', 'etpteacher')
bottomhalf <- c('bottomhalf', 'bottomhalf_tracking')
bottomquarter <- c('bottomquarter', 'bottomquarter_tracking')
secondquarter <- c('secondquarter', 'secondquarter_tracking')
topquarter <- c('topquarter', 'topquarter_tracking')

fm0 <- as.formula(paste("stdR_totalscore ~ tracking +", 
                       paste(bottomhalf, collapse = " + "),'+',
                       paste(bottomquarter, collapse = " + "),'+',
                       paste(secondquarter, collapse = " + "),'+',
                       paste(topquarter, collapse = " + "),'+',
                       paste(ind_controls, collapse = " + ")))

m0 <- lm(fm0, data = df)
stargazer(m0, type = 'text', digits = 4)


# model 1
m1 <- lm(stdR_totalscore ~ tracking, data=df)
stargazer(m1, type = 'text', digits = 4)

par(mfrow=c(2,2))
plot(m1)


cl_se <- vcovCL(m1, cluster = df$schoolid, type = "HC1")
m1rce  <- coeftest(m1, vcov = cl_se)

stargazer(m1, m1rce, type = 'text', digits = 4)

# model 2
fm2 <-as.formula(paste("stdR_totalscore ~ tracking +", 
                       paste(ind_controls, collapse = " + ")))

m2 <- lm(fm2, data = df)

cl_se <- vcovCL(m2, cluster = df$schoolid, type = "HC1")
m2rce  <- coeftest(m2, vcov = cl_se)

stargazer(m1rce, m2rce, type = 'text', digits = 4)

# model 3
fm3 <-as.formula(paste("stdR_totalscore ~ tracking +", 
                       paste(bottomhalf, collapse = " + "),'+',
                       paste(ind_controls, collapse = " + ")))

m3 <- lm(fm3, data = df)

cl_se <- vcovCL(m3, cluster = df$schoolid, type = "HC1")
m3rce  <- coeftest(m3, vcov = cl_se)


stargazer(m0, m1rce, m2rce, m3rce, type = 'text', digits = 4)

# model 4
fm4 <- as.formula(paste("stdR_totalscore ~ tracking +", 
                       
                       paste(bottomquarter, collapse = " + "),'+',
                       paste(secondquarter, collapse = " + "),'+',
                       paste(topquarter, collapse = " + "),'+',
                       
                       paste(ind_controls, collapse = " + ")))
fm4

m4 <- lm(fm4, data = df)

cl_se <- vcovCL(m4, cluster = df$schoolid, type = "HC1")
m4rce  <- coeftest(m4, vcov = cl_se)

stargazer(m1rce, m2rce, m3rce, m4rce, type = 'text', digits = 4)


fm5 <- as.formula(paste("stdR_totalscore ~ tracking +", 
                       paste(topquarter, collapse = " + "),'+',
                       paste(ind_controls, collapse = " + ")))
fm5

# model 5
m5 <- lm(fm5, data = df)

cl_se <- vcovCL(m5, cluster = df$schoolid, type = "HC1")
m5rce  <- coeftest(m5, vcov = cl_se)

stargazer(m1rce, m2rce, m3rce, m4rce,m5rce, type = 'text', digits = 4)

# Function
models <- function(dep_var) {
  
  # Controls
  ind_controls <- c('girl', 'percentile', 'agetest', 'etpteacher')
  bottomhalf <- c('bottomhalf', 'bottomhalf_tracking')
  bottomquarter <- c('bottomquarter', 'bottomquarter_tracking')
  secondquarter <- c('secondquarter', 'secondquarter_tracking')
  topquarter <- c('topquarter', 'topquarter_tracking')
  
  # Summary statistics
  df[, mean_total := df[tracking == 0, mean(get(dep_var), na.rm = TRUE)]]
  df[, sd_total := df[tracking == 0, sd(get(dep_var), na.rm = TRUE)]]
  
  # Initialize an empty list to store models
  model_list <- list()
  
  # If `dep_var` is "stdR_totalscore", run all 4 models
  if (dep_var == "stdR_totalscore") {
    
    # Model 1
    m1 <- lm(as.formula(paste(dep_var, "~ tracking")), data = df)
    cl_se_m1 <- vcovCL(m1, cluster = df$schoolid, type = "HC1")
    model_list$m1rce <- coeftest(m1, vcov = cl_se_m1)
    
    # Model 2
    fm2 <- as.formula(paste(dep_var, "~ tracking +", paste(ind_controls, collapse = " + ")))
    m2 <- lm(fm2, data = df)
    cl_se_m2 <- vcovCL(m2, cluster = df$schoolid, type = "HC1")
    model_list$m2rce <- coeftest(m2, vcov = cl_se_m2)
  }
  
  # Model 3 (always runs)
  fm3 <- as.formula(paste(dep_var, "~ tracking +", 
                          paste(bottomhalf, collapse = " + "), "+",
                          paste(ind_controls, collapse = " + ")))
  m3 <- lm(fm3, data = df)
  cl_se_m3 <- vcovCL(m3, cluster = df$schoolid, type = "HC1")
  model_list$m3rce <- coeftest(m3, vcov = cl_se_m3)
  
  # Model 4 (always runs)
  fm4 <- as.formula(paste(dep_var, "~ tracking +", 
                          paste(bottomquarter, collapse = " + "), "+",
                          paste(secondquarter, collapse = " + "), "+",
                          paste(topquarter, collapse = " + "), "+",
                          paste(ind_controls, collapse = " + ")))
  m4 <- lm(fm4, data = df)
  cl_se_m4 <- vcovCL(m4, cluster = df$schoolid, type = "HC1")
  model_list$m4rce <- coeftest(m4, vcov = cl_se_m4)
  
  # Output the appropriate models
  stargazer(model_list, type = "text", digits = 4, 
            title = paste("Regression Results for", dep_var))
}


# Total score
models("stdR_totalscore")

# Math score
models("stdR_mathscoreraw")

# Literacy score
models("stdR_litscore")
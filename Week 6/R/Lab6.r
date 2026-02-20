rm(list=ls())

### Replication of Papageorgiou et al. (2017)

# Load necessary packages
#install.packages(c("lmtest", "sandwich","modelsummary"))
library(stats)
library(nlme)
library(lmtest)
library(sandwich)
library(boot)

# Load data
data <- read.csv("https://raw.githubusercontent.com/emolinaperez/econometrics_mek/main/Week%206/data/electricity_sector.csv")

# Generate country dummies
data$country <- as.factor(data$country)
dummies <- model.matrix(~ country - 1, data = data)
colnames(dummies) <- paste0("c", 1:ncol(dummies))
data <- cbind(data, dummies)

# Generate country ID
data$id <- as.numeric(factor(data$country))

#####################################################
# Table 3: Nonlinear Estimation of CES - Electricity Sector
#####################################################

# Column 1: Nonlinear Estimation of CES in Levels - Electricity Sector

# Nonlinear Least Squares (NLS) Estimation of CES Model
ces_model <- nls(
  ln_eg ~ a + d*year + 
    b_c1*c1 + b_c2*c2 + b_c3*c3 + b_c4*c4 + b_c5*c5 + 
    b_c6*c6 + b_c7*c7 + b_c8*c8 + b_c9*c9 + b_c10*c10 +
    b_c11*c11 + b_c12*c12 + b_c13*c13 + b_c14*c14 + b_c15*c15 +
    b_c16*c16 + b_c17*c17 + b_c18*c18 + b_c19*c19 + b_c20*c20 +
    b_c21*c21 + b_c22*c22 + b_c23*c23 + b_c24*c24 + b_c25*c25 +
    (1/psi)*log(omega*EC_c^psi + (1-omega)*EC_d^psi),
  data = data,
  start = list(a = 20,d = 0.01, psi = -0.5, omega = 0.5,
    b_c1 = 0, b_c2 = 0, b_c3 = 0, b_c4 = 0, b_c5 = 0,
    b_c6 = 0, b_c7 = 0, b_c8 = 0, b_c9 = 0, b_c10 = 0,
    b_c11 = 0, b_c12 = 0, b_c13 = 0, b_c14 = 0, b_c15 = 0,
    b_c16 = 0, b_c17 = 0, b_c18 = 0, b_c19 = 0, b_c20 = 0,
    b_c21 = 0, b_c22 = 0, b_c23 = 0, b_c24 = 0, b_c25 = 0
  ),
  control = nls.control(maxiter = 100)
)

summary(ces_model)


# Model Summary
library(modelsummary)
modelsummary(ces_model,
             coef_omit = "^b_c",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             fmt = fmt_decimal(digits = 4),
             gof_map = c(
               "nobs ~ N",
               "r.squared ~ Adjusted R²",
               "sigma ~ σ"
             ),
             coef_map = c(
               "d" = "d",
               "omega" = "ω",
               "psi" = "ψ"
             ),
             title = "Table 3: Nonlinear Estimation of CES - Electricity Sector"
             )


# Bootstrapping CES Model with Error Handling

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = 400, style = 3)

# Create a counter variable outside of the function
iteration_counter <- 0  

# Define bootstrap function
ces_boot <- boot(data, function(data, indices) {
  
  # Update progress bar safely
  iteration_counter <<- iteration_counter + 1  # Update the counter
  setTxtProgressBar(pb, iteration_counter)  # Update progress bar
  
  # Sample data
  data_sample <- data[indices, ]
  
  # Fit the model using tryCatch to handle errors
  fit <- tryCatch({
    suppressWarnings(
      nls(ln_eg ~ a + d*year + 
            b_c1*c1 + b_c2*c2 + b_c3*c3 + b_c4*c4 + b_c5*c5 + 
            b_c6*c6 + b_c7*c7 + b_c8*c8 + b_c9*c9 + b_c10*c10 +
            b_c11*c11 + b_c12*c12 + b_c13*c13 + b_c14*c14 + b_c15*c15 +
            b_c16*c16 + b_c17*c17 + b_c18*c18 + b_c19*c19 + b_c20*c20 +
            b_c21*c21 + b_c22*c22 + b_c23*c23 + b_c24*c24 + b_c25*c25 +
            (1/psi)*log(omega*EC_c^psi + (1-omega)*EC_d^psi),
          data = data_sample,
          start = coef(ces_model),
          control = nls.control(maxiter = 100, warnOnly = TRUE))
    )
  }, error = function(e) return(rep(NA, length(coef(ces_model)))))

  if (is.numeric(fit)) return(fit)  # Return NA if failed
  return(coef(fit))
}, R = 400)

# Close the progress bar when finished
close(pb)


# Summary results
boot_summary <- data.frame(
  estimate = coef(ces_model),
  std.error = apply(ces_boot$t, 2, sd, na.rm = TRUE),
  z_stat = coef(ces_model) / apply(ces_boot$t, 2, sd, na.rm = TRUE),
  p_value = 2 * (1 - pnorm(abs(coef(ces_model) / apply(ces_boot$t, 2, sd, na.rm = TRUE)))),
  conf.low = apply(ces_boot$t, 2, quantile, probs = 0.025, na.rm = TRUE),
  conf.high = apply(ces_boot$t, 2, quantile, probs = 0.975, na.rm = TRUE)
)

# Print results
boot_summary 

# Column 3: Linear Estimation of Kmenta Approximation in Levels - Electricity Sector

# Define model
model3 <- lm(ln_egecd ~ c1 + c2 + c3 + c4 + c5 + 
               c6 + c7 + c8 + c9 + c10 +
               c11 + c12 + c13 + c14 + c15 +
               c16 + c17 + c18 + c19 + c20 +
               c21 + c22 + c23 + c24 + c25 +
               year + ln_eccd + ln_eccd_2, 
             data = data)

# Get clustered standard errors at the country level
cl_vcov <- vcovCL(model3, cluster = ~country, type = "HC1")

# Display robust summary
coeftest(model3, vcov = cl_vcov)

# Extract coefficients
coef_values <- coef(model3)

# Define nonlinear transformations
a <- coef_values["(Intercept)"]
d <- coef_values["year"]
omega <- coef_values["ln_eccd"]

psi_formula <- ((coef_values["ln_eccd"] * (1 - coef_values["ln_eccd"]) / 
                (coef_values["ln_eccd"] * (1 - coef_values["ln_eccd"]) - coef_values["ln_eccd_2"])) - 1) / 
                (coef_values["ln_eccd"] * (1 - coef_values["ln_eccd"]) / 
                (coef_values["ln_eccd"] * (1 - coef_values["ln_eccd"]) - coef_values["ln_eccd_2"]))

# Print the transformed coefficients
print(paste("d:", d))
print(paste("ω:", omega))
print(paste("ψ:", psi_formula))


#####################################################
# Table 4: Alternative Capital Proxy - Electricity Sector
#####################################################

# Alternative Capital Proxy Model
ces_alt_model <- nls(ln_eg ~ a + d * year + (1 / psi) * log(omega * EC_c_alt^psi + (1 - omega) * EC_d_alt^psi),
                      data = data,
                      start = list(a = 20, d = 0.01, psi = -0.5, omega = 0.5),
                      control = nls.control(maxiter = 100, warnOnly = TRUE))
summary(ces_alt_model)

# Bootstrapping CES Alternative Model with Error Handling

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = 400, style = 3)

# Create a counter variable outside of the function
iteration_counter <- 0  

# Define bootstrap function with progress tracking
ces_alt_boot <- boot(data, function(data, indices) {
  
  # Update progress bar safely
  iteration_counter <<- iteration_counter + 1  # Increment counter
  setTxtProgressBar(pb, iteration_counter)  # Update progress bar
  
  # Sample data
  data_sample <- data[indices, ]
  
  # Fit the model using tryCatch to handle errors
  fit <- tryCatch({
    suppressWarnings(
      nls(ln_eg ~ a + d * year + (1 / psi) * log(omega * EC_c_alt^psi + (1 - omega) * EC_d_alt^psi),
          data = data_sample,
          start = coef(ces_alt_model),
          control = nls.control(maxiter = 100, warnOnly = TRUE))
    )
  }, error = function(e) return(rep(NA, length(coef(ces_alt_model)))))

  if (is.numeric(fit)) return(fit)  # Return NA if failed
  return(coef(fit))
}, R = 400)

# Close the progress bar when finished
close(pb)

# Summary results
boot_summary <- data.frame(
  estimate = coef(ces_alt_model),
  std.error = apply(ces_alt_boot$t, 2, sd, na.rm = TRUE),
  z_stat = coef(ces_alt_model) / apply(ces_alt_boot$t, 2, sd, na.rm = TRUE),
  p_value = 2 * (1 - pnorm(abs(coef(ces_alt_model) / apply(ces_alt_boot$t, 2, sd, na.rm = TRUE)))),
  conf.low = apply(ces_alt_boot$t, 2, quantile, probs = 0.025, na.rm = TRUE),
  conf.high = apply(ces_alt_boot$t, 2, quantile, probs = 0.975, na.rm = TRUE)
)

# Print results
boot_summary 

#####################################################
# Table 5: Nonlinear Estimation of Cobb-Douglas in CES - Electricity Sector
#####################################################

# Cobb-Douglas Model
cd_model <- nls(ln_eg ~ a + d * year + (1 / psi) * log(omega * EC_c^psi + (1 - omega) * (EC_d^alpha * FU_d^(1 - alpha))^psi),
                 data = data,
                 start = list(a = 0, d = 0.01, psi = -0.2, omega = 0.5, alpha = 0.7),
                 control = nls.control(maxiter = 100, warnOnly = TRUE))
summary(cd_model)

# Bootstrapping Cobb-Douglas Model with Error Handling

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = 400, style = 3)

# Create a counter variable outside of the function
iteration_counter <- 0  

# Define bootstrap function with progress tracking
cd_boot <- boot(data, function(data, indices) {
  
  # Update progress bar safely
  iteration_counter <<- iteration_counter + 1  # Increment counter
  setTxtProgressBar(pb, iteration_counter)  # Update progress bar
  
  # Sample data
  data_sample <- data[indices, ]
  
  # Fit the model using tryCatch to handle errors
  fit <- tryCatch({
    suppressWarnings(
      nls(ln_eg ~ a + d * year + 
            (1 / psi) * log(omega * EC_c^psi + (1 - omega) * (EC_d^alpha * FU_d^(1 - alpha))^psi),
          data = data_sample,
          start = coef(cd_model),
          control = nls.control(maxiter = 100, warnOnly = TRUE))
    )
  }, error = function(e) return(rep(NA, length(coef(cd_model)))))

  if (is.numeric(fit)) return(fit)  # Return NA if failed
  return(coef(fit))
}, R = 400)

# Close the progress bar when finished
close(pb)

# Summary results
boot_summary <- data.frame(
  estimate = coef(cd_model),
  std.error = apply(cd_boot$t, 2, sd, na.rm = TRUE),
  z_stat = coef(cd_model) / apply(cd_boot$t, 2, sd, na.rm = TRUE),
  p_value = 2 * (1 - pnorm(abs(coef(cd_model) / apply(cd_boot$t, 2, sd, na.rm = TRUE)))),
  conf.low = apply(cd_boot$t, 2, quantile, probs = 0.025, na.rm = TRUE),
  conf.high = apply(cd_boot$t, 2, quantile, probs = 0.975, na.rm = TRUE)
)

# Print results
boot_summary 

#####################################################
# Table 6: CES in Cobb-Douglas - Non-Energy Industries
#####################################################

# Load data
data <- read.csv("https://raw.githubusercontent.com/emolinaperez/econometrics_mek/main/Week%206/data/nonenergy_industries.csv")

# Generate country dummies
data$country <- as.factor(data$country)
dummies <- model.matrix(~ country - 1, data = data)
colnames(dummies) <- paste0("c", 1:ncol(dummies))
data <- cbind(data, dummies)

# Generate industry dummies
data$industry <- as.factor(data$industry)
dummies <-model.matrix(~ factor(industry) - 1, data = data)
colnames(dummies) <- paste0("i", 1:ncol(dummies))
data <- cbind(data, dummies)

# Generate country ID
data$id <- as.numeric(factor(data$country))

# CES Model for Non-Energy Industries
ces_ne_model <- nls(
  ln_vaxiie ~ a + d * year + 
    b_c1*c1 + b_c2*c2 + b_c3*c3 + b_c4*c4 + b_c5*c5 + 
    b_c6*c6 + b_c7*c7 + b_c8*c8 + b_c9*c9 + b_c10*c10 +
    b_c11*c11 + b_c12*c12 + b_c13*c13 + b_c14*c14 + b_c15*c15 +
    b_c16*c16 + b_c17*c17 + b_c18*c18 +
    b_i1*i1 + b_i2*i2 + b_i3*i3 + b_i4*i4 + b_i5*i5 +
    b_i6*i6 + b_i7*i7 + b_i8*i8 + b_i9*i9 + b_i10*i10 +
    b_i11*i11 + b_i12*i12 + b_i13*i13 + b_i14*i14 + b_i15*i15 +
    b_i16*i16 + b_i17*i17 + b_i18*i18 + b_i19*i19 + b_i20*i20 +
    b_i21*i21 + b_i22*i22 + b_i23*i23 + b_i24*i24 + b_i25*i25 + b_i26*i26 + b_i27*i27 +
    (1 - alph - gamm) * ln_xl + alph * ln_xk + gamm * (1 / psi) * log(xd^psi + xc^psi),
  
      data = data,
      start = list(a = 0, d = 0.01, alph = 0.3,gamm = 0.1,psi  = 0.2,
                   b_c1 = 0, b_c2 = 0, b_c3 = 0, b_c4 = 0, b_c5 = 0,
                   b_c6 = 0, b_c7 = 0, b_c8 = 0, b_c9 = 0, b_c10 = 0,
                   b_c11 = 0, b_c12 = 0, b_c13 = 0, b_c14 = 0, b_c15 = 0,
                   b_c16 = 0, b_c17 = 0, b_c18 = 0,
                   b_i1 = 0, b_i2 = 0, b_i3 = 0, b_i4 = 0, b_i5 = 0,
                   b_i6 = 0, b_i7 = 0, b_i8 = 0, b_i9 = 0, b_i10 = 0,
                   b_i11 = 0, b_i12 = 0, b_i13 = 0, b_i14 = 0, b_i15 = 0,
                   b_i16 = 0, b_i17 = 0, b_i18 = 0, b_i19 = 0, b_i20 = 0,
                   b_i21 = 0, b_i22 = 0, b_i23 = 0, b_i24 = 0, b_i25 = 0, b_i26 = 0, b_i27 = 0),
      control = nls.control(maxiter = 100, warnOnly = TRUE))

summary(ces_ne_model)


# Model Summary
modelsummary(ces_ne_model,
             coef_omit = "^b_c|^b_i",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             fmt = fmt_decimal(digits = 4),
             gof_map = c(
               "nobs ~ N",
               "r.squared ~ Adjusted R²",
               "sigma ~ σ"
             ),
             coef_map = c(
               "d" = "d",
               "alph"="α",
               "gamm" = "γ",
               "psi" = "ψ"
             ),
             title = "Table 3: Nonlinear Estimation of CES - Electricity Sector"
)

# Bootstrapping Non-Energy CES Model

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = 10, style = 3)  # Adjusted for R = 10

# Create a counter variable outside of the function
iteration_counter <- 0

ces_ne_boot <- boot(data, function(data, indices) {
  
  # Update progress bar safely
  iteration_counter <<- iteration_counter + 1  # Increment counter
  setTxtProgressBar(pb, iteration_counter)  # Update progress bar

  data_sample <- data[indices, ]
  fit <- tryCatch({
    suppressWarnings(
    nls(ln_vaxiie ~ a + d * year + 
          b_c1*c1 + b_c2*c2 + b_c3*c3 + b_c4*c4 + b_c5*c5 + 
          b_c6*c6 + b_c7*c7 + b_c8*c8 + b_c9*c9 + b_c10*c10 +
          b_c11*c11 + b_c12*c12 + b_c13*c13 + b_c14*c14 + b_c15*c15 +
          b_c16*c16 + b_c17*c17 + b_c18*c18 +
          b_i1*i1 + b_i2*i2 + b_i3*i3 + b_i4*i4 + b_i5*i5 +
          b_i6*i6 + b_i7*i7 + b_i8*i8 + b_i9*i9 + b_i10*i10 +
          b_i11*i11 + b_i12*i12 + b_i13*i13 + b_i14*i14 + b_i15*i15 +
          b_i16*i16 + b_i17*i17 + b_i18*i18 + b_i19*i19 + b_i20*i20 +
          b_i21*i21 + b_i22*i22 + b_i23*i23 + b_i24*i24 + b_i25*i25 + b_i26*i26 + b_i27*i27 +
          (1 - alph - gamm) * ln_xl + alph * ln_xk + gamm * (1 / psi) * log(xd^psi + xc^psi),
        data = data_sample,
        start = coef(ces_ne_model),
        control = nls.control(maxiter = 100, warnOnly = TRUE))
    )
  }, error = function(e) return(rep(NA, length(coef(ces_ne_model)))))
  
  if (is.numeric(fit)) return(fit)  # Return NA if failed
  return(coef(fit))
}, R = 10)

# Close the progress bar when finished
close(pb)

# Summary results
boot_summary <- data.frame(
  estimate = coef(ces_ne_model),
  std.error = apply(ces_ne_boot$t, 2, sd, na.rm = TRUE),
  z_stat = coef(ces_ne_model) / apply(ces_ne_boot$t, 2, sd, na.rm = TRUE),
  p_value = 2 * (1 - pnorm(abs(coef(cd_model) / apply(ces_ne_boot$t, 2, sd, na.rm = TRUE)))),
  conf.low = apply(ces_ne_boot$t, 2, quantile, probs = 0.025, na.rm = TRUE),
  conf.high = apply(ces_ne_boot$t, 2, quantile, probs = 0.975, na.rm = TRUE)
)

# Print results
boot_summary 
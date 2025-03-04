library(haven)
library(dplyr)
library(lme4)
library(plm)
library(plotly)
library(patchwork)
library(ggplot2)
library(lmtest)
library(lfe)
library(clubSandwich)
library(biglmm)
library(quantreg)
library(msm)
library(boot)

# Data preparation

gc()

rm(cleaned_data, full_data, lagged_firm_prod, lagged_revenue)

bartik_full_postdo <- read_dta("C:/Users/a7654/OneDrive/Ambiente de Trabalho/Heriot-Watt/Dissertation/3rd paper material/Motivation/data/bartik_full_postdo.dta")

#bartik_full_postdo <- bartik_full_postdo %>% filter(i_wages>0)

labour_intensive = subset(bartik_full_postdo, ind2d %in% c(1, 2, 3, 46, 47, 53, 55, 56, 61, 64, 65, 66, 68, 69, 70, 74, 79, 80))

bartik_full_postdo = labour_intensive

# Lagged productivity variable
bartik_full_postdo <- bartik_full_postdo %>%
  group_by(j) %>%
  mutate(firm_productivity = lag(firm_productivity), na.rm = TRUE) %>%
  ungroup()

bartik_full_postdo <- na.omit(bartik_full_postdo)

# OLS
ols_model <- lm(r_i_wages ~ firm_productivity, data = bartik_full_postdo)
summary(ols_model)
# Extract the residuals from the OLS model
bartik_full_postdo$u_hat <- residuals(ols_model)

# rho
#Estimate AR(1) coefficient for x
ar_x_model <- lm(firm_productivity ~ firm_productivity_lag, data = bartik_full_postdo)
rho_hat <- coef(ar_x_model)[2]
rho_se <- summary(ar_x_model)$coefficients[2, "Std. Error"] # SE from summary table
print(paste("Estimated rho:", rho_hat, ", SE:", rho_se))

# varphi
#Estimate AR(1) coefficient for residuals
ar_u_model <- lm(u_hat ~ lag(lag(u_hat)), data = bartik_full_postdo)
phi_hat <- coef(ar_u_model)[2]
phi_se <- summary(ar_u_model)$coefficients[2, "Std. Error"] # SE from summary table
print(paste("Estimated phi:", phi_hat, ", SE:", phi_se))

cov_matrix <- matrix(0, nrow = 2, ncol = 2)
cov_matrix[1, 1] <- vcov(ar_x_model)[2, 2]  # Variance of rho
cov_matrix[2, 2] <- vcov(ar_u_model)[2, 2]  # Variance of phi

# rho/phi ratio
rho_phi_ratio = rho_hat/phi_hat
rho_phi_se <- deltamethod(
  g = ~ x1/x2, # Formula: rho / phi
  mean = c(rho_hat, phi_hat), # Point estimates
  cov = cov_matrix
)

# Delta

#Estimate the parameter for the effect of u on y
delta_hat <- coef(ols_model)[1] #Intercept
delta_se <- summary(ols_model)$coefficients[1, "Std. Error"]

# Kappa
kappa_model <- lm(firm_productivity ~ u_hat + firm_productivity_lag, data = bartik_full_postdo)
kappa_hat <- coef(kappa_model)[2]
kappa_se <- summary(kappa_model)$coefficients[2, "Std. Error"]
print(paste("Estimated kappa:", kappa_hat, ", SE:", kappa_se))

# Estimated Parameters for Bias Calculations

ols_bias_sample <- (delta_hat*kappa_hat*var(bartik_full_postdo$u_hat))/((1-phi_hat*rho_hat)*var(bartik_full_postdo$firm_productivity))

#Lagged IV Bias

iv_bias_sample <- (delta_hat*kappa_hat*var(bartik_full_postdo$u_hat))/((rho_hat/phi_hat)*(1-phi_hat*rho_hat)*var(bartik_full_postdo$firm_productivity)+kappa_hat^2*var(bartik_full_postdo$u_hat))

# Function to Calculate Biases
calculate_biases <- function(data, indices) {
  # Use only the resampled data
  data_resample <- data[indices, ]
  
  ols_model <- lm(r_i_wages ~ firm_productivity, data = data_resample)
  
  data_resample$u_hat <- residuals(ols_model)
  
  ar_x_model <- lm(firm_productivity ~ firm_productivity_lag2, data = data_resample)
  rho_hat <- coef(ar_x_model)[2]
  
  ar_u_model <- lm(u_hat ~ lag(lag(u_hat)), data = data_resample)
  phi_hat <- coef(ar_u_model)[2]
  
  delta_hat <- coef(ols_model)[1]
  
  kappa_model <- lm(firm_productivity ~ u_hat + firm_productivity_lag2, data = data_resample)
  kappa_hat <- coef(kappa_model)[2]
  
  ols_bias_sample <- (delta_hat * kappa_hat * var(data_resample$u_hat)) / ((1 - phi_hat * rho_hat) * var(data_resample$firm_productivity))
  iv_bias_sample <- (delta_hat * kappa_hat * var(data_resample$u_hat)) / ((rho_hat / phi_hat) * (1 - phi_hat * rho_hat) * var(data_resample$firm_productivity) + kappa_hat^2 * var(data_resample$u_hat))
  
  return(c(ols_bias_sample, iv_bias_sample))
}

# Bootstrap
set.seed(123)
boot_results <- boot(data = bartik_full_postdo, statistic = calculate_biases, R = 500) # R = number of bootstrap replicates

# Extracting SEs
ols_bias_se <- sd(boot_results$t[, 1]) # SE of OLS bias
iv_bias_se <- sd(boot_results$t[, 2]) # SE of IV bias

print(paste("Sample OLS Bias: ",ols_bias_sample))
print(paste("Bootstrapped OLS Bias SE:", ols_bias_se))

print(paste("Sample IV Bias: ",iv_bias_sample))
print(paste("Bootstrapped IV Bias SE:", iv_bias_se))

print(paste("Estimated rho/phi Ratio:", rho_phi_ratio, ", SE:", rho_phi_se))
print(paste("Estimated kappa:", kappa_hat, ", SE:", kappa_se))
print(paste("Estimated delta:", delta_hat, ", SE:", delta_se))

gc()

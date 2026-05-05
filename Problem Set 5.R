# Q1 ----------------------------------------------------------------------
#Read the dataset 
natality <- read.csv("data/Problem Set 5/natality.csv")
View(natality)


# 1a ----------------------------------------------------------------------
# Load necessary library
library(dplyr)
# Overall Count (N)
n_cases <- sum(natality$NICU == 1)
n_controls <- sum(natality$NICU == 0)
# Maternal Education: N (%)
# Ensure MEDUC is a factor with the correct levels order
natality$MEDUC <- factor(natality$MEDUC, levels = c("<HS", "HS", "Some college", "Bachelor", "Adv Degree"))
edu_table <- natality %>%
  group_by(NICU, MEDUC) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(NICU) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  arrange(NICU, MEDUC)
# Maternal Age: Mean (SD)
age_stats <- natality %>%
  group_by(NICU) %>%
  summarise(
    mean_age = mean(MAGER, na.rm = TRUE),
    sd_age = sd(MAGER, na.rm = TRUE)
  )
# P-values
# Education: Chi-squared test
edu_p_value <- chisq.test(table(natality$MEDUC, natality$NICU))$p.value
# Age: Independent t-test
age_p_value <- t.test(MAGER ~ NICU, data = natality)$p.value
# Print Results
cat("--- Sample Size --- \n")
print(paste("Cases:", n_cases, "| Controls:", n_controls))
cat("\n--- Education Distribution --- \n")
print(edu_table)
print(paste("Education P-value (Chi-sq):", round(edu_p_value, 4)))
cat("\n--- Age Distribution --- \n")
print(age_stats)
print(paste("Age P-value (t-test):", round(age_p_value, 4)))


# 1b ----------------------------------------------------------------------
# Load the necessary library
library(survival)
# Fit the conditional logistic regression model
# NICU is the outcome, RF_PPTERM is the predictor
# strata(matchid) accounts for the matching
cond_model <- clogit(NICU ~ RF_PPTERM + strata(matchid), data = natality)
# Summary of the model
summary(cond_model)
# To get the exponentiated coefficients (Odds Ratio) and 95% CI
exp_coef <- exp(coef(cond_model))
conf_interval <- exp(confint(cond_model))
# Print the results
cat("--- Odds Ratio for Previous Preterm Birth --- \n")
print(exp_coef)
cat("\n--- 95% Confidence Interval --- \n")
print(conf_interval)


# Q2 ----------------------------------------------------------------------

# 2c ----------------------------------------------------------------------
# Load necessary library
# install.packages("survival") # Uncomment if you haven't installed it
# install.packages("survminer") # Optional, for better plotting
library(survival)
# Load the dataset
pfs_df <- read.csv("data/Problem Set 5/pfs_data.csv")
# Show the dataset
cat("--- Dataset --- \n")
print(pfs_df)

#  Create a survival object
# time: the survival time
# status: 1 for event (death/progression), 0 for censored
surv_obj <- Surv(time = pfs_df$time, event = pfs_df$status)

# Generate the Kaplan-Meier curve (fit the model)
km_fit <- survfit(surv_obj ~ 1)
# Summary of the KM fit (includes 95% CI values)
summary(km_fit)
#  Plot the KM curve with 95% Confidence Intervals
plot(km_fit, 
     conf.int = TRUE, 
     main = "Kaplan-Meier Curve with 95% CI", 
     xlab = "Time (Months)", 
     ylab = "Progression-Free Survival Probability",
     col = "blue", 
     lwd = 2,
     mark.time = TRUE) # Shows "+" for censored data




# Q3 -----------------------------------------------------------------------
#Read the dataset 
phsvte <- read.csv("data/Problem Set 5/phsvte.csv")
View(phsvte)
phsvte_codebook <- read.csv("data/Problem Set 5/phsvte_codebook.csv")
View(phsvte_codebook)


# 3a ----------------------------------------------------------------------
# Create the indicator variable
# If hip >= 40, hip_group = 1, otherwise 0
phsvte$hip_high <- ifelse(phsvte$hip >= 40, 1, 0)
# Fit the Poisson regression model
# We use log(fup) as an offset to model the RATE (events / person-years)
poisson_model <- glm(status ~ hip_high + offset(log(fup)), 
                     family = poisson(link = "log"), 
                     data = phsvte)

# Calculate point estimates and 95% CIs for each group
# We use the 'predict' function or look at the model coefficients
# Group 0 (Hip < 40)
pred_low <- predict(poisson_model, newdata = data.frame(hip_high = 0, fup = 1), 
                    type = "link", se.fit = TRUE)

# Group 1 (Hip >= 40)
pred_high <- predict(poisson_model, newdata = data.frame(hip_high = 1, fup = 1), 
                     type = "link", se.fit = TRUE)

# Function to transform log-rate to rate and get 95% CI
get_rate_ci <- function(pred) {
  rate <- exp(pred$fit)
  lower <- exp(pred$fit - 1.96 * pred$se.fit)
  upper <- exp(pred$fit + 1.96 * pred$se.fit)
  return(c(Rate = rate, Lower = lower, Upper = upper))
}

# Final Estimates
rates_low <- get_rate_ci(pred_low)
rates_high <- get_rate_ci(pred_high)
cat("--- Incidence Rate (VTE per Person-Year) --- \n")
cat("Hip < 40: ", round(rates_low, 6), "\n")
cat("Hip >= 40:", round(rates_high, 6), "\n")



# 3b ----------------------------------------------------------------------
# Extract the incidence rates from Part A
# Assuming 'rates_low' and 'rates_high' are the vectors from your previous step
lambda_low  <- rates_low[1]   # Rate for Hip < 40
lambda_high <- rates_high[1]  # Rate for Hip >= 40
# Define the time horizon (10 years)
t <- 10

# Calculate 10-year Cumulative Incidence (CI)
# Formula: CI = 1 - exp(-lambda * t)
ci_10_low  <- 1 - exp(-lambda_low * t)
ci_10_high <- 1 - exp(-lambda_high * t)

# Display the results
cat("--- 10-Year Cumulative Incidence Estimates --- \n")
cat("Group: Hip < 40 inches \n")
cat("Cumulative Incidence:", round(ci_10_low, 5), "(or", round(ci_10_low * 100, 2), "%) \n\n")
cat("Group: Hip >= 40 inches \n")
cat("Cumulative Incidence:", round(ci_10_high, 5), "(or", round(ci_10_high * 100, 2), "%) \n")


# 3c ----------------------------------------------------------------------
# Load the survival library
library(survival)
# Create the indicator variable (if not already done)
phsvte$hip_high <- ifelse(phsvte$hip >= 40, 1, 0)

# Fit the Kaplan-Meier model stratified by the hip group
km_fit <- survfit(Surv(fup, status) ~ hip_high, data = phsvte)

# Extract estimates at exactly 10 years
# 'times = 10' tells R to provide the values for the 10-year mark
km_summary <- summary(km_fit, times = 10)

# Extract Survival Estimates and 95% CIs
# Group 0: Hip < 40 | Group 1: Hip >= 40
surv_estimates <- km_summary$surv
surv_lower <- km_summary$lower
surv_upper <- km_summary$upper

# Calculate Cumulative Incidence (Risk) and 95% CIs
# Risk = 1 - Survival
risk_estimates <- 1 - surv_estimates
risk_lower <- 1 - surv_upper  # Lower bound of risk uses Upper bound of survival
risk_upper <- 1 - surv_lower  # Upper bound of risk uses Lower bound of survival

# Print the Results
cat("--- 10-Year Kaplan-Meier Survival Estimates --- \n")
results_surv <- data.frame(
  Group = c("Hip < 40", "Hip >= 40"),
  Survival = round(surv_estimates, 5),
  Lower_CI = round(surv_lower, 5),
  Upper_CI = round(surv_upper, 5)
)
print(results_surv)

cat("\n--- 10-Year Cumulative Incidence (Risk) Estimates --- \n")
results_risk <- data.frame(
  Group = c("Hip < 40", "Hip >= 40"),
  Risk = round(risk_estimates, 5),
  Lower_CI = round(risk_lower, 5),
  Upper_CI = round(risk_upper, 5)
)
print(results_risk)


# 3d ----------------------------------------------------------------------
# Load the survival library
library(survival)
#  Fit the Kaplan-Meier model
km_fit <- survfit(Surv(fup, status) ~ hip_high, data = phsvte)
# Plot the Cumulative Hazard
# fun = "cumhaz" transforms the survival probability into cumulative hazard
plot(km_fit, 
     fun = "cumhaz", 
     col = c("black", "red"), 
     lty = c(1, 2),
     lwd = 2,
     xlab = "Time", 
     ylab = "Cumulative hazard",
     main = "Cumulative hazard by hip circumference",
     bty = "l") # Creates the L-shaped box frame
# Add a legend to match the screenshot
legend("topleft", 
       legend = c("Hip < 40", "Hip >= 40"), 
       col = c("black", "red"), 
       lty = c(1, 2),
       bty = "n") # Removes the legend border


# 3e ----------------------------------------------------------------------
# (1) Poisson Test: Check the p-value of the coefficient
summary(poisson_model) 
# (2) Log-rank Test
survdiff(Surv(fup, status) ~ hip_high, data = phsvte)


# 3f ----------------------------------------------------------------------
# Partition height into 4 categories using quartiles
# We use the cut function with quantile to create the quartiles
height_quartiles <- quantile(phsvte$height, probs = seq(0, 1, 0.25), na.rm = TRUE)
phsvte$height_cat <- cut(phsvte$height, 
                         breaks = height_quartiles, 
                         include.lowest = TRUE, 
                         labels = c("Q1", "Q2", "Q3", "Q4"))

# Perform the stratified log-rank test
# strata(height_cat) tells R to perform the test within height groups
strat_test <- survdiff(Surv(fup, status) ~ hip_high + strata(height_cat), data = phsvte)
# Print the results
print(strat_test)


# Q4 ----------------------------------------------------------------------

# 4a ----------------------------------------------------------------------
library(survival)
# Create hip circumference quartiles
hip_quartiles <- quantile(phsvte$hip, probs = seq(0, 1, 0.25), na.rm = TRUE)
phsvte$hip_q <- cut(phsvte$hip, 
                    breaks = hip_quartiles, 
                    include.lowest = TRUE, 
                    labels = c("Q1", "Q2", "Q3", "Q4"))

# Fit the Cox proportional hazards model
# Q1 will be the reference group by default
cox_model <- coxph(Surv(fup, status) ~ hip_q + age + exer + diab + hyper + 
                     hichol + current_smok + past_smok, data = phsvte)
# View the summary
summary(cox_model)


# 4b ----------------------------------------------------------------------
# Test the proportional hazards assumption
# This function calculates the correlation between Schoenfeld residuals and time
ph_check <- cox.zph(cox_model)
# Print the test results
print(ph_check)


# EXTRA CREDIT ------------------------------------------------------------
# Load required libraries
library(survival)
library(ggplot2)
library(ggfortify)

# Define the incidence rates from Question 3a
# Replace these with your actual point estimates if they differ
lambda_low <- 0.000898
lambda_high <- 0.001537

# Create the data for the Poisson Survival Curves (Smooth Exponential)
time_seq <- seq(0, 12, by = 0.1)
poisson_df <- data.frame(
  time = rep(time_seq, 2),
  surv = c(exp(-lambda_low * time_seq), exp(-lambda_high * time_seq)),
  group = rep(c("Hip < 40", "Hip >= 40"), each = length(time_seq))
)

# Create the data for KM Curves using fortify
# Assuming km_fit is your survfit object from Question 3c:
# km_fit <- survfit(Surv(fup, status) ~ hip_high, data = phsvte)
km_data <- fortify(km_fit)

# Generate the Plot
ggplot() +
  # Plot Poisson estimates (Dashed Lines)
  geom_line(data = poisson_df, aes(x = time, y = surv, color = group), 
            linetype = "dashed", size = 1) +
  # Plot KM estimates (Solid Step Lines)
  geom_step(data = km_data, aes(x = time, y = surv, color = strata), 
            size = 1) +
  # Restrict x-axis to 0-12
  coord_cartesian(xlim = c(0, 12), ylim = c(0.98, 1)) +
  # Labels and Theme
  labs(
    title = "Comparison of Survival Curves: Poisson vs. Kaplan-Meier",
    subtitle = "Dashed = Poisson (Constant Hazard) | Solid = Kaplan-Meier (Non-parametric)",
    x = "Years of Follow-up",
    y = "Survival Probability",
    color = "Hip Circumference Group"
  ) +
  scale_color_manual(
    # Provide 4 colors (2 for Poisson, 2 for KM)
    values = c("blue", "red", "blue", "red"),
    # Provide 4 labels to match the 4 levels in your plot legend
    labels = c("Hip < 40 (Poisson)", "Hip >= 40 (Poisson)", 
               "Hip < 40 (KM)", "Hip >= 40 (KM)")
  ) +
  theme_minimal()



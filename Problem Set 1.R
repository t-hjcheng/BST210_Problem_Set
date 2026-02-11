#Read the dataset 
data <- read.csv("data/Problem Set 1/jptr1yr_ps01.csv")
View(data)

# 1a -----------------------------------------------------------------------
# 1. Create the log-transformed CRP variable if not already in the dataset
data$log_crp0 <- log(data$crp0)
View(data)
# 2. Fit the multiple linear regression model
# Note: Ensure 'sex' is treated as a factor if it isn't already
sbp_model <- lm(sbp ~ male + age + bmi + ldl0 + log_crp0, data = data)
# 3. Display the results
summary(sbp_model)

# 1c  --------------------------------------------------------------------
# Calculate the sum of the residuals from your sbp_model
sum_resid <- sum(residuals(sbp_model))
# Print the result
print(sum_resid)

# 1d  --------------------------------------------------------------------
# Calculate the sum of the absolute value of the residuals
sum_abs_resid <- sum(abs(residuals(sbp_model)))
# Print the result
print(sum_abs_resid)
# Round to the nearest integer as requested
round(sum_abs_resid)

# 1e  --------------------------------------------------------------------
rss_value <- sum(residuals(sbp_model)^2)
round(rss_value)

# 1f  --------------------------------------------------------------------
tss_value <- sum((data$sbp - mean(data$sbp))^2)
round(tss_value)


# 1 g  --------------------------------------------------------------------
mss_value <- sum((predict(sbp_model) - mean(data$sbp))^2)
round(mss_value)


# 1h  --------------------------------------------------------------------
r_squared <- summary(sbp_model)$r.squared
round(r_squared, 4)


# 1j  --------------------------------------------------------------------
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)
# Robust Covariance Matrix)
robust_cov <- vcovHC(sbp_model, type = "HC1")
# 95% CI 
robust_ci <- coefci(sbp_model, vcov = robust_cov)
print(robust_ci)
# For BMI 
bmi_ci <- robust_ci["bmi", ]
print(bmi_ci)


# 3a  --------------------------------------------------------------------
# Build age catagories 
data$age_cat <- cut(data$age, 
             breaks = c(-Inf, 65, 75, Inf), 
             labels = c("65_and_under", "66_to_75", "over_75"))
# Reference group: 65_and_under 
data$age_cat <- relevel(data$age_cat, ref = "65_and_under")
# New model 
model_cat <- lm(sbp ~ male + age_cat + bmi + ldl0 + log_crp0, data = data)
# show results 
summary(model_cat)
confint(model_cat)


# 3b ---------------------------------------------------------------------
# Change reference to 66-75
data$age_cat <- relevel(data$age_cat, ref = "66_to_75")
# Refit the model
model_ref_66 <- lm(sbp ~ male + age_cat + bmi + ldl0 + log_crp0, data = data)
summary(model_ref_66)
confint(model_ref_66)


# 3c ---------------------------------------------------------------------
# Create the numeric age category variable
# 0: age <= 65
# 1: 66 <= age <= 75
# 2: age > 75
data$age_cat_num <- ifelse(data$age <= 65, 0, 
                           ifelse(data$age <= 75, 1, 2))
# Fit the model treating age_cat_num as a continuous variable
# (Assuming your original predictors were male, bmi, ldl0, and log_crp0)
model_numeric <- lm(sbp ~ male + age_cat_num + bmi + ldl0 + log_crp0, data = data)
# View the summary
summary(model_numeric)

# 3d ---------------------------------------------------------------------
summary(model_numeric)
confint(model_numeric, level = 0.95)


# 4a  --------------------------------------------------------------------
# Generate log(trig0) & log(trig1)
data$log_trig0 <- log(data$trig0)
data$log_trig1 <- log(data$trig1)
# Generate the indicator variable
# Returns 1 if log_trig0 >= 5, otherwise 0
data$hi_logtrig0 <- ifelse(data$log_trig0 >= 5, 1, 0)
# Fit the multiple linear regression with interaction
# The '*' operator in R automatically includes main effects and the interaction
model_trig <- lm(log_trig1 ~ trt * hi_logtrig0, data = data)
# View the results
summary(model_trig)


# 4b  --------------------------------------------------------------------
confint(model_trig, level = 0.95)


# 4c ----------------------------------------------------------------------
# Create the four group indicators (1 if in group, 0 otherwise)
data$G1 <- ifelse(data$trt == 0 & data$hi_logtrig0 == 0, 1, 0) # Placebo + Low Baseline (Reference)
data$G2 <- ifelse(data$trt == 0 & data$hi_logtrig0 == 1, 1, 0) # Placebo + High Baseline
data$G3 <- ifelse(data$trt == 1 & data$hi_logtrig0 == 0, 1, 0) # Treatment + Low Baseline
data$G4 <- ifelse(data$trt == 1 & data$hi_logtrig0 == 1, 1, 0) # Treatment + High Baseline
# Fit the model using groups G2, G3, and G4 (omitting the reference group G1)
model_no_interaction <- lm(log_trig1 ~ G2 + G3 + G4, data = data)
summary(model_no_interaction)


# 5 a ---------------------------------------------------------------------
# Create the centered baseline variable
# Centering at 5 means subtracting 5 from the original log_trig0
data$log_trig0_centered <- data$log_trig0 - 5
# Fit the model with interaction
# Using the centered variable as a continuous predictor
model_centered <- lm(log_trig1 ~ trt * log_trig0_centered, data = data)
# View the results
summary(model_centered)


# EXTRA CREDIT 1  ---------------------------------------------------------
# Use existing model
sbp_model <- lm(sbp ~ male + age + bmi + ldl0 + log_crp0, data = data)
# Filter data for males (assuming male == 1 means Male)
male_data <- subset(data, male == 1)
# Create a sequence of log CRP values (per Hint)
log_crp_seq <- seq(min(data$log_crp0, na.rm = TRUE), 
                   max(data$log_crp0, na.rm = TRUE), 
                   by = 0.01)
# Create prediction data frame matching your model's variable names
# Fixed constraints: Male (male=1), Age 65, BMI 32, LDL 95
pred_df <- data.frame(
  male = 1,
  age = 65,
  bmi = 32,
  ldl0 = 95,
  log_crp0 = log_crp_seq
)
# Predict SBP values
pred_df$predicted_sbp <- predict(sbp_model, newdata = pred_df)
# Generate the Scatter Plot
plot(male_data$log_crp0, male_data$sbp, 
     col = "lightgray", pch = 16, cex = 0.6,
     xlab = "Baseline log CRP", 
     ylab = "Systolic Blood Pressure (SBP)",
     main = "SBP vs log CRP for Males (Age 65, BMI 32, LDL 95)")
# Superimpose the Regression Line
lines(pred_df$log_crp0, pred_df$predicted_sbp, col = "red", lwd = 3)


# EXTRA CREDIT 2 ----------------------------------------------------------
# Load the splines library
library(splines)
# Fit the model with natural cubic splines (df=6) for age
spline_model <- lm(sbp ~ male + ns(age, df = 6) + bmi + ldl0 + log_crp0, data = data)
# Filter data for male participants (male == 1)
male_data <- subset(data, male == 1)
# Create a sequence of ages for the prediction line (0.1 units apart)
age_seq <- seq(min(data$age, na.rm = TRUE), 
               max(data$age, na.rm = TRUE), 
               by = 0.1)
# Create prediction data frame with specific constraints:
# male=1, LDL=108, BMI=32, log_crp0=1.5
pred_df <- data.frame(
  male = 1,
  age = age_seq,
  bmi = 32,
  ldl0 = 108,
  log_crp0 = 1.5
)
# Predict SBP values using the spline model
pred_df$predicted_sbp <- predict(spline_model, newdata = pred_df)
# Generate the Scatter Plot
plot(male_data$age, male_data$sbp, 
     col = "lightgray", pch = 16, cex = 0.5,
     xlab = "Age (Years)", 
     ylab = "Systolic Blood Pressure (SBP)",
     main = "SBP vs Age for Males (Spline Model: df=6)")
# Superimpose the Spline Regression Line
lines(pred_df$age, pred_df$predicted_sbp, col = "blue", lwd = 3)





#Read the dataset 
data <- read.csv("data/Problem Set 1/jptr1yr_ps01.csv")
View(data)

# 1 a -----------------------------------------------------------------------
# 1. Create the log-transformed CRP variable if not already in the dataset
data$log_crp0 <- log(data$crp0)
View(data)
# 2. Fit the multiple linear regression model
# Note: Ensure 'sex' is treated as a factor if it isn't already
sbp_model <- lm(sbp ~ male + age + bmi + ldl0 + log_crp0, data = data)
# 3. Display the results
summary(sbp_model)

# 1 c  --------------------------------------------------------------------
# Calculate the sum of the residuals from your sbp_model
sum_resid <- sum(residuals(sbp_model))
# Print the result
print(sum_resid)

# 1 d  --------------------------------------------------------------------
# Calculate the sum of the absolute value of the residuals
sum_abs_resid <- sum(abs(residuals(sbp_model)))
# Print the result
print(sum_abs_resid)
# Round to the nearest integer as requested
round(sum_abs_resid)

# 1 e  --------------------------------------------------------------------
rss_value <- sum(residuals(sbp_model)^2)
round(rss_value)

# 1 f  --------------------------------------------------------------------
tss_value <- sum((data$sbp - mean(data$sbp))^2)
round(tss_value)


# 1 g  --------------------------------------------------------------------
mss_value <- sum((predict(sbp_model) - mean(data$sbp))^2)
round(mss_value)


# 1 h  --------------------------------------------------------------------
r_squared <- summary(sbp_model)$r.squared
round(r_squared, 4)










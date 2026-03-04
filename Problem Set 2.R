#Read the dataset 
data <- read.csv("data/Problem Set 2/wells.csv")
View(data)

# 1a ----------------------------------------------------------------------
# Create a binary variable for arsenic (1 if >= 1.5, 0 otherwise)
data$arsenic_bin <- ifelse(data$arsenic >= 1.5, 1, 0)
# Generate a 2x2 contingency table
# Exposure: arsenic_bin (Row), Outcome: switch (Column)
arsenic_switch_table <- table(Arsenic_Level = data$arsenic_bin, Switched = data$switch)
print(arsenic_switch_table)
# Perform the Chi-square test (without continuity correction for standard 2x2 analysis)
chi_test_result <- chisq.test(arsenic_switch_table, correct = FALSE)
print(chi_test_result)


# 1b ----------------------------------------------------------------------
# Calculate the proportions (risks) for each group
# Risk in High Arsenic group (arsenic_bin = 1)
risk_high <- 857 / (420 + 857)
# Risk in Low Arsenic group (arsenic_bin = 0)
risk_low <- 880 / (863 + 880)
# Calculate the Risk Difference
risk_diff <- risk_high - risk_low
#Print results rounded to 3 decimal places
cat("Risk (High Arsenic):", round(risk_high, 3), "\n")
cat("Risk (Low Arsenic):", round(risk_low, 3), "\n")
cat("Risk Difference:", round(risk_diff, 3), "\n")


# 1c ----------------------------------------------------------------------
risk_ratio <- risk_high / risk_low
# Print results rounded to 3 decimal places
cat("Risk Ratio:", round(risk_ratio, 3), "\n")


# 1d ----------------------------------------------------------------------
# Odds in High Arsenic group (arsenic_bin = 1)
odds_high <- 857 / 420
# Odds in Low Arsenic group (arsenic_bin = 0)
odds_low <- 880 / 863
# Calculate the Odds Ratio (OR)
odds_ratio <- odds_high / odds_low
# Print results rounded to 3 decimal places
cat("Odds (High Arsenic):", round(odds_high, 3), "\n")
cat("Odds (Low Arsenic):", round(odds_low, 3), "\n")
cat("Odds Ratio:", round(odds_ratio, 3), "\n")


# 2a ----------------------------------------------------------------------
# Fit the Linear Probability Model (LPM) using lm()
# Outcome: switch (0/1)
# Predictors: dist, arsenic, arsenic squared, and educ
model_lpm <- lm(switch ~ dist + arsenic + I(arsenic^2) + educ, data = data)
# Extract the coefficient (Risk Difference for 1 meter)
beta_dist <- coef(model_lpm)["dist"]
#  Extract the 95% Confidence Interval for 1 meter
ci_dist <- confint(model_lpm)["dist", ]
# Calculate the effect for a 50-meter increase
rd_50m <- beta_dist * 50
ci_50m <- ci_dist * 50
# Convert to percentages and round to 1 decimal place
rd_pct <- round(rd_50m * 100, 1)
ci_low_pct <- round(ci_50m[1] * 100, 1)
ci_high_pct <- round(ci_50m[2] * 100, 1)
# Display the results
cat("Risk Difference for 50m increase (LPM):", rd_pct, "%\n")
cat("95% CI:", "[", ci_low_pct, "%,", ci_high_pct, "%]\n")

# 3a ----------------------------------------------------------------------
#Using a Poisson model with log link to estimate Risk Ratio
model_rr <- glm(switch ~ dist + arsenic + I(arsenic^2) + educ, 
                family = poisson(link = "log"), 
                data = data)
# Extract the coefficient for 'dist' (Log-RR for 1 meter)
beta_dist <- coef(model_rr)["dist"]
# Extract the 95% Confidence Interval for 'dist' (Log-RR)
ci_dist <- confint(model_rr)["dist", ]
#  Calculate the Risk Ratio for a 50-meter increase
# RR_50m = exp(beta * 50)
rr_50m <- exp(beta_dist * 50)
ci_50m_low <- exp(ci_dist[1] * 50)
ci_50m_high <- exp(ci_dist[2] * 50)
# Convert to relative percentage change: (RR - 1) * 100
rr_pct <- round((rr_50m - 1) * 100, 1)
ci_low_pct <- round((ci_50m_low - 1) * 100, 1)
ci_high_pct <- round((ci_50m_high - 1) * 100, 1)
# Display results
cat("Relative change in risk for 50m increase:", rr_pct, "%\n")
cat("95% CI:", "[", ci_low_pct, "%,", ci_high_pct, "%]\n")

# 4a ----------------------------------------------------------------------
# Fit the Logistic Regression model
# Predictors: arsenic, arsenic^2, dist, educ, assoc &　Interaction: assoc * dist
model_logit <- glm(switch ~ arsenic + I(arsenic^2) + dist + educ + assoc + assoc:dist, 
                   family = binomial(link = "logit"), 
                   data = data)
# Extract the coefficient for 'dist'
# In a model with (assoc * dist), the coefficient for 'dist' alone 
# represents the effect of distance when assoc = 0.
log_or_dist_assoc0 <- coef(model_logit)["dist"]
# Get the 95% Confidence Interval for the log-odds
ci_log_dist <- confint(model_logit)["dist", ]
# Calculate the Odds Ratio and 95% CI
or_dist_assoc0 <- exp(log_or_dist_assoc0)
ci_or_low <- exp(ci_log_dist[1])
ci_or_high <- exp(ci_log_dist[2])
# Print results rounded to 3 decimal points
cat("Odds Ratio for 1m increase (assoc=0):", round(or_dist_assoc0, 3), "\n")
cat("95% CI:", "[", round(ci_or_low, 3), ",", round(ci_or_high, 3), "]\n")


# 4b  ---------------------------------------------------------------------
# Use the correct interaction name found in your vcov_mat: "dist:assoc"
inter_name <- "dist:assoc"
# Calculate log-odds for dist when assoc = 1
log_or_assoc1 <- coeffs["dist"] + coeffs[inter_name]
# Calculate the SE for this combination
se_assoc1 <- sqrt(vcov_mat["dist", "dist"] + 
                    vcov_mat[inter_name, inter_name] + 
                    2 * vcov_mat["dist", inter_name])

# Calculate Odds Ratio and 95% CI
or_assoc1 <- exp(log_or_assoc1)
ci_low_assoc1 <- exp(log_or_assoc1 - 1.96 * se_assoc1)
ci_high_assoc1 <- exp(log_or_assoc1 + 1.96 * se_assoc1)
# Print results (rounded to 3 decimal points)
cat("Odds Ratio for 1m increase (assoc=1):", round(or_assoc1, 3), "\n")
cat("95% CI: [", round(ci_low_assoc1, 3), ", ", round(ci_high_assoc1, 3), "]\n")

# 5a ----------------------------------------------------------------------
# Fit the model without the interaction term
model_no_int <- glm(switch ~ dist + arsenic + I(arsenic^2) + educ + assoc, 
                    family = binomial(link = "logit"), 
                    data = data)
# Calculate the Odds Ratio (OR) for 1 meter
or_dist <- exp(coef(model_no_int)["dist"])
# Calculate the 95% Confidence Interval
ci_dist <- exp(confint(model_no_int)["dist", ])
# Print results (rounded to 3 decimal points)
cat("Odds Ratio for 1m increase:", round(or_dist, 3), "\n")
cat("95% CI: [", round(ci_dist[1], 3), ", ", round(ci_dist[2], 3), "]\n")


# 5b ----------------------------------------------------------------------
# Compare the model without interaction (model_no_int) 
# to the model with interaction (model_logit)
lrt_result <- anova(model_no_int, model_logit, test = "Chisq")
# Display the results to find the p-value (Pr(>Chi))
print(lrt_result)


# 6a ----------------------------------------------------------------------
# Fit the Linear Probability Model (LPM) with interaction
# The '*' operator includes main effects and the interaction
model_rd <- lm(switch ~ arsenic_bin * assoc, data = data)
# View the results
summary(model_rd)


# 6b ----------------------------------------------------------------------
# Fit the Linear Probability Model without interaction
model_not_saturated <- lm(switch ~ arsenic_bin + assoc, data = data)
# Display results
summary(model_not_saturated)



# 6f ----------------------------------------------------------------------
#  Fit the logistic regression model with interaction
model_logistic_int <- glm(switch ~ arsenic_bin * assoc, 
                          family = binomial(link = "logit"), 
                          data = data)
#  Create a new data frame for prediction
new_data <- expand.grid(arsenic_bin = c(0, 1), assoc = c(0, 1))
#  Get predicted probabilities
new_data$prob <- predict(model_logistic_int, newdata = new_data, type = "response")
#  View results
print(new_data)


# 7a  ---------------------------------------------------------------------
# Load the necessary library
library(pROC)
# Split the data based on the 'subset' variable
train_data <- subset(data, subset == "training")
# Fit the model from question #4 on the training set
# (Assuming the model was: switch ~ dist * assoc + arsenic + I(arsenic^2) + educ)
model_train <- glm(switch ~ arsenic + I(arsenic^2) + dist + educ + assoc + assoc:dist, 
                   family = binomial(link = "logit"), 
                   data = train_data)
# Get predicted probabilities for the training set
train_probs <- predict(model_train, type = "response")
# Calculate the ROC curve and AUC
roc_train <- roc(train_data$switch, train_probs)
auc_value <- auc(roc_train)
#  Calculate the 95% Confidence Interval for the AUC
auc_ci <- ci.auc(roc_train)
# Print results rounded to 3 decimal places
cat("Training Set AUC:", round(auc_value, 3), "\n")
cat("95% CI: [", round(auc_ci[1], 3), ", ", round(auc_ci[3], 3), "]\n")

# 7b ----------------------------------------------------------------------
# Plot the ROC curve for the training set
plot(roc_train, 
     main = "ROC Curve for Training Set (Question 7)", 
     col = "blue",        # Line color
     lwd = 3,             # Line width
     legacy.axes = TRUE,  # Use 1-Specificity on the x-axis
     xlab = "False Positive Rate (1 - Specificity)", 
     ylab = "True Positive Rate (Sensitivity)")
# Add a diagonal reference line representing a random guess (AUC = 0.5)
abline(a = 0, b = 1, lty = 2, col = "darkgray")
# Optional: Add the AUC value and 95% CI to the plot
text(0.4, 0.2, labels = paste("AUC =", round(auc_value, 3)), cex = 1.2, col = "blue")


# 7d ----------------------------------------------------------------------
# Split the data to get the test set based on the 'subset' variable
test_data <- subset(data, subset == "test")
# Get predicted probabilities for the test set using the model from Q7a
# 'model_train' is the model fitted using only the training data
test_probs <- predict(model_train, newdata = test_data, type = "response")
# Calculate the ROC curve for the test set
roc_test <- roc(test_data$switch, test_probs)
# Calculate AUC and the 95% Confidence Interval
auc_test_val <- auc(roc_test)
auc_test_ci <- ci.auc(roc_test)
# Print results rounded to 3 decimal places
cat("Test Set AUC:", round(auc_test_val, 3), "\n")
cat("95% CI: [", round(auc_test_ci[1], 3), ", ", round(auc_test_ci[3], 3), "]\n")


# EXTRA CREDIT ------------------------------------------------------------
# Define grid for search (e.g., from 0.5 to 4.0 by increments of 0.01)
threshold_grid <- seq(0.5, 4.0, by = 0.01)
aic_results <- data.frame(x = threshold_grid, aic = NA)

# Perform Grid Search to minimize AIC
for(i in 1:nrow(aic_results)) {
  current_x <- aic_results$x[i]
  
  # Create indicator and interaction terms for the broken-stick model
  # Model: logit(P) = b0 + b1*As + b2*(As >= x) + b3*As*(As >= x)
  data$above_x <- as.numeric(data$arsenic >= current_x)
  
  # Fit the logistic regression model
  fit <- glm(switch ~ arsenic * above_x, family = binomial(link = "logit"), data = data)
  aic_results$aic[i] <- AIC(fit)
}

# Identify the optimal threshold x
optimal_x <- aic_results$x[which.min(aic_results$aic)]
cat("Optimal Threshold (x):", round(optimal_x, 2), "\n")

# Fit final model using optimal x and calculate Odds Ratios
data$above_x_opt <- as.numeric(data$arsenic >= optimal_x)
final_model <- glm(switch ~ arsenic * above_x_opt, family = binomial, data = data)

# Extract coefficients
b1 <- coef(final_model)["arsenic"]
b3 <- coef(final_model)["arsenic:above_x_opt"]

# Calculate OR for a 0.05-unit increase
or_below <- exp(0.05 * b1)
or_above <- exp(0.05 * (b1 + b3))

# Output results
cat("Odds Ratio for 0.05-unit increase (Below threshold):", round(or_below, 2), "\n")
cat("Odds Ratio for 0.05-unit increase (At or Above threshold):", round(or_above, 2), "\n")

# Generate Probability Plot
library(ggplot2)
plot_data <- data.frame(arsenic = seq(min(data$arsenic), max(data$arsenic), length.out = 1000))
plot_data$above_x_opt <- as.numeric(plot_data$arsenic >= optimal_x)
plot_data$prob <- predict(final_model, newdata = plot_data, type = "response")

ggplot(plot_data, aes(x = arsenic, y = prob)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_vline(xintercept = optimal_x, linetype = "dashed", color = "red") +
  annotate("text", x = optimal_x + 0.3, y = 0.2, label = paste("Optimal x =", optimal_x), color = "red") +
  labs(title = "Broken-Stick Model: Probability of Switching vs. Arsenic",
       subtitle = paste("Threshold determined by AIC minimization"),
       x = "Arsenic Level",
       y = "Predicted Probability") +
  theme_minimal()



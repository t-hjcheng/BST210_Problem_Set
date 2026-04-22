
# Q1 ----------------------------------------------------------------------
#Read the dataset 
schizophrenia <- read.csv("data/Problem Set 4/schizophrenia.csv")
View(schizophrenia)

# 1a ----------------------------------------------------------------------
library(dplyr)
library(tidyr)
schizophrenia %>%
  group_by(week, drug) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = drug, values_from = n) %>%
  rename(Placebo_0 = `0`, Drug_1 = `1`)

# 1b ----------------------------------------------------------------------
# Load necessary libraries
library(lme4)
library(lmerTest) # This package allows lmer to provide p-values in the summary

# Fit a linear mixed-effects model with a random intercept
# Formula explanation:
# - severity: Dependent variable (outcome)
# - week * drug: Fixed effects for week, drug, and their interaction (week + drug + week:drug)
# - (1 | ID): Random intercept for each participant (ID) to account for within-subject correlation
model_1b <- lmer(severity ~ week * drug + (1 | ID), data = schizophrenia)

# Display the model results
summary(model_1b)



# Q2 ----------------------------------------------------------------------
# 2a ----------------------------------------------------------------------
# Filter data: only drug group
schiz_drug_only <- subset(schizophrenia, drug == 1)

# Fit model: random intercept and random slope for week
# gender is included as a fixed effect (no random slope requested)
model_2a <- lmer(severity ~ gender + week + (1 + week | ID), data = schiz_drug_only)

# Display results
summary(model_2a)


# Q3 ----------------------------------------------------------------------
# 3c ----------------------------------------------------------------------
# Load the necessary library
library(geepack)
# Fit the GEE model
# Note: Data should ideally be sorted by ID for GEE
schizophrenia <- schizophrenia[order(schizophrenia$ID), ]
model_gee <- geeglm(severity ~ week * drug, 
                    data = schizophrenia, 
                    id = ID, 
                    family = gaussian, 
                    corstr = "exchangeable")
# View the results
summary(model_gee)


# 3e ----------------------------------------------------------------------
# Extract the coefficient table (including Robust Standard Errors)
# 'model_gee' is the object from your geeglm() fit
gee_summary <- summary(model_gee)$coefficients

# Extract the Estimate and Robust SE for the 'week' variable
# In this model, 'week' represents the slope for the Placebo group
week_est <- gee_summary["week", "Estimate"]
week_se  <- gee_summary["week", "Std.err"]

# Calculate the Lower and Upper bounds of the 95% CI
# Using the critical value Z = 1.96 for a 95% interval
lower_95 <- week_est - (1.96 * week_se)
upper_95 <- week_est + (1.96 * week_se)

# Print the results
cat("95% Confidence Interval for Week (Placebo Group):\n")
cat("Estimate: ", round(week_est, 5), "\n")
cat("95% CI:  [", round(lower_95, 5), ", ", round(upper_95, 5), "]\n")


# Q4 ----------------------------------------------------------------------
# 4a ----------------------------------------------------------------------
# Create the binary outcome variable
# if severity is >= 5, 0 otherwise
schizophrenia$sev_high <- ifelse(schizophrenia$severity >= 5, 1, 0)
#  Load the lme4 library for mixed effects models
library(lme4)

# Fit the Generalized Linear Mixed Effects Model (GLMM)
# We use the logit link by specifying family = binomial
model_glmm <- glmer(sev_high ~ week * drug + (1 | ID), 
                    data = schizophrenia, 
                    family = binomial(link = "logit"))

#Display the model summary
summary(model_glmm)


# 4c ----------------------------------------------------------------------
# Load library for testing linear combinations
library(multcomp)
# Estimate the combination: 1*week + 1*week:drug
# This gives the slope specifically for the Treated group
treated_slope <- glht(model_glmm, linfct = c("week + week:drug = 0"))

# Get the Point Estimate and 95% CI on the log-odds scale
slope_summary <- summary(treated_slope)
slope_confint <- confint(treated_slope)

# Transform to Odds Ratios (exponentiate)
or_point_estimate <- exp(slope_summary$test$coefficients)
or_ci <- exp(slope_confint$confint)

# Print results
cat("Treated Group Odds Ratio:", or_point_estimate, "\n")
cat("95% CI for Odds Ratio: [", or_ci[2], ",", or_ci[3], "]\n")


# 4d ----------------------------------------------------------------------
# Ensure the geepack library is loaded
library(geepack)
# Sort data by ID (required for GEE correlation structures)
schizophrenia <- schizophrenia[order(schizophrenia$ID), ]

# Fit the GEE model with logit link and exchangeable correlation
model_gee_binary <- geeglm(sev_high ~ week * drug, 
                           data = schizophrenia, 
                           id = ID, 
                           family = binomial(link = "logit"), 
                           corstr = "exchangeable")
# Display the results
summary(model_gee_binary)


# EXTRA CREDIT ------------------------------------------------------------
# Prepare data for plotting (Treated group, IDs 3314, 4104, 4514, 7112, 8317)
target_ids <- c(3314, 4104, 4514, 7112, 8317)
weeks_seq <- seq(0, 6, by = 0.1)

# Create a grid for GLMM predictions (includes ID for random effects)
plot_data_glmm <- expand.grid(ID = target_ids, week = weeks_seq, drug = 1)

# Generate Predicted Probabilities for GLMM (Subject-specific)
# type = "response" gives probabilities instead of log-odds
plot_data_glmm$prob <- predict(model_glmm, newdata = plot_data_glmm, type = "response")

# Generate Predicted Probabilities for GEE (Population-averaged)
# GEE does not use random effects, so we only need week and drug
plot_data_gee <- data.frame(week = weeks_seq, drug = 1)
plot_data_gee$prob <- predict(model_gee_binary, newdata = plot_data_gee, type = "response")

#  Create the plot using ggplot2
library(ggplot2)
ggplot() +
  # Individual GLMM curves (Subject-specific)
  geom_line(data = plot_data_glmm, aes(x = week, y = prob, color = as.factor(ID)), size = 1) +
  # GEE curve (Population-averaged) - making it stand out with a thick dashed line
  geom_line(data = plot_data_gee, aes(x = week, y = prob), color = "black", linetype = "dashed", size = 1.5) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Predicted Probability of High Severity (Treated Group)",
       subtitle = "Colored lines: GLMM (Individual) | Black dashed line: GEE (Population Average)",
       x = "Week", y = "Probability (Severity >= 5)",
       color = "Participant ID") +
  theme_minimal()







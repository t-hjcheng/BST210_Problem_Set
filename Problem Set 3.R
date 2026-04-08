# Q1 ----------------------------------------------------------------------
#Read the dataset 
nmes <- read.csv("data/Problem Set 3/nmes.csv")
View(nmes)

#Read the dataset 
nmes_codebook <- read.csv("data/Problem Set 3/nmes_codebook.csv")
View(nmes_codebook)


# 1a ----------------------------------------------------------------------
# Calculate the quantile breaks for the 'visits' variable
# probs = seq(0, 1, 0.25) provides the 0%, 25%, 50%, 75%, and 100% boundary values
breaks <- quantile(nmes$visits, probs = seq(0, 1, 0.25))
# Use the cut function to categorize 'visits' into quartiles
# include.lowest = TRUE ensures the minimum value (e.g., 0) is included in the first group
nmes$visits_q <- cut(nmes$visits, breaks = breaks, include.lowest = TRUE)
# Calculate descriptive statistics for each quartile
library(dplyr)
summary_table <- nmes %>%
  group_by(visits_q) %>%
  summarize(
    n = n(),
    mean_visits = mean(visits, na.rm = TRUE),
    median_visits = median(visits, na.rm = TRUE)
  )
#Display the final summary table
print(summary_table)



# 1d ----------------------------------------------------------------------
# Load required libraries
library(nnet)
library(dplyr)

# Ensure the reference levels are set correctly
# Reference for health: "average"
# Reference for insurance: "no"
# Reference for outcome (visits_q): "[0,1]" (lowest quartile)
nmes$health <- relevel(as.factor(nmes$health), ref = "average")
nmes$insurance <- relevel(as.factor(nmes$insurance), ref = "no")
# Assuming visits_q was created as a factor in previous steps
nmes$visits_q <- relevel(nmes$visits_q, ref = "[0,1]")

#  Fit the polytomous regression model
# Equation based on your 1c request
model_poly <- multinom(visits_q ~ hospital + age_10 + health + chronic + 
                         male + school + insurance, data = nmes)
# View the coefficient table
summary(model_poly)
# Get the summary (Coefficients and Standard Errors)
summary_poly <- summary(model_poly)

# Calculate Odds Ratios (OR) and 95% Confidence Intervals
# We need to exponentiate the coefficients for OR
# Extract coefficients and SE specifically for Quartile 2 vs 1
coeffs <- summary_poly$coefficients[1, ] # First row is usually Quartile 2
ses <- summary_poly$standard.errors[1, ]

# Calculate OR and 95% CI for 'chronic'
or_chronic <- exp(coeffs["chronic"])
ci_lower <- exp(coeffs["chronic"] - 1.96 * ses["chronic"])
ci_upper <- exp(coeffs["chronic"] + 1.96 * ses["chronic"])

# Output the results
cat("OR for chronic (Q2 vs Q1):", or_chronic, "\n")
cat("95% CI:", "[", ci_lower, ",", ci_upper, "]\n")


# 1d ----------------------------------------------------------------------
# Fit the model with interaction terms (*)
# The '*' operator includes both main effects and their interaction
model_int <- multinom(visits_q ~ hospital + age_10 + health * chronic + 
                        male + school + insurance, data = nmes)
# View the coefficient table
summary(model_int)
# Get the summary to see the coefficients
summary_int <- summary(model_int)
#Extract coefficients specifically for Quartile 3 vs 1
# Row 2 of the coefficient matrix usually corresponds to Quartile 3
coeffs_q3 <- summary_int$coefficients[2, ] 
# View the specific coefficients needed for the calculation
coeffs_q3["chronic"]
coeffs_q3["healthpoor:chronic"] # The interaction term


# 1f ----------------------------------------------------------------------
# Access the coefficients from model 1e for the 3rd quartile (vs 1st)
# In multinom, the second row usually corresponds to the comparison of Q3 vs Q1
coeffs_q3 <- summary(model_int)$coefficients[2, ]
#  Extract the specific coefficient for age_10
beta_age10 <- coeffs_q3["age_10"]
# Calculate the Log-odds for a 5-year increase
# Since the unit of age_10 is 10 years, a 5-year increase is 0.5 units
log_odds_5yr <- beta_age10 * 0.5
# Calculate the Conditional Odds Ratio (OR) by exponentiating
or_5yr <- exp(log_odds_5yr)
#  Output the results
cat("Log-odds for a 5-year increase (Q3 vs Q1):", log_odds_5yr, "\n")
cat("Conditional Odds Ratio (OR) for a 5-year increase:", or_5yr, "\n")


# 1g ----------------------------------------------------------------------
# Define the two individuals for comparison
# Individual A: Excellent health
person_excellent <- data.frame(
  hospital = 1, age_10 = 5.8, health = "excellent", 
  chronic = 1, male = 1, school = 12, insurance = "yes"
)

# Individual B: Average health
person_average <- data.frame(
  hospital = 1, age_10 = 5.8, health = "average", 
  chronic = 1, male = 1, school = 12, insurance = "yes"
)

# Predict probabilities for each quartile
prob_exc <- predict(model_int, newdata = person_excellent, type = "probs")
prob_avg <- predict(model_int, newdata = person_average, type = "probs")

# Calculate Conditional Odds (Q2 vs Q1) for both
odds_exc <- prob_exc[2] / prob_exc[1]  # Odds for Excellent
odds_avg <- prob_avg[2] / prob_avg[1]  # Odds for Average

# Calculate the Odds Ratio (OR)
calculated_or <- odds_exc / odds_avg

# Output results
print(prob_exc)
print(prob_avg)
cat("Calculated OR (Excellent vs Average):", calculated_or, "\n")


# 2b ----------------------------------------------------------------------
# Fit the Poisson regression model
# Ensure reference levels are set correctly as before
nmes$health <- relevel(as.factor(nmes$health), ref = "average")
nmes$insurance <- relevel(as.factor(nmes$insurance), ref = "no")
model_poisson <- glm(visits ~ hospital + age_10 + health + chronic + 
                       male + school + insurance, 
                     data = nmes, 
                     family = poisson)
# View the coefficient table
summary(model_poisson)
# Extract coefficients and 95% Confidence Intervals
# Use the 'broom' package for a clean table or base R confint()
library(broom)
health_results <- tidy(model_poisson, conf.int = TRUE) %>%
  filter(grepl("health", term))

# Exponentiate for Incidence Rate Ratios (IRR)
health_results_irr <- health_results %>%
  mutate(
    IRR = exp(estimate),
    IRR_conf.low = exp(conf.low),
    IRR_conf.high = exp(conf.high)
  )
print(health_results_irr)


# 2c ----------------------------------------------------------------------
# Fit the interaction model
model_int_poi <- glm(visits ~ hospital + age_10 + health * chronic + 
                       male + school + insurance, 
                     data = nmes, family = poisson)
# View the coefficient table
summary(model_int_poi)
# Extract coefficients
coeffs_poi <- coef(model_int_poi)
# Calculate for Excellent group
# chronic main effect + healthexcellent:chronic interaction
combined_beta <- coeffs_poi["chronic"] + coeffs_poi["healthexcellent:chronic"]
irr_excellent <- exp(combined_beta)
pct_change <- (irr_excellent - 1) * 100

cat("Combined Beta:", combined_beta, "\n")
cat("IRR for Excellent:", irr_excellent, "\n")
cat("Percentage Change:", pct_change, "%\n")



# 2d ----------------------------------------------------------------------
# Load the MASS library
library(MASS)
# Fit the Negative Binomial model with interaction terms
# Using the same predictors and interaction as in 2c
model_nb <- glm.nb(visits ~ hospital + age_10 + health * chronic + 
                     male + school + insurance, 
                   data = nmes)

# Extract the coefficients
coeffs_nb <- coef(model_nb)

#  Calculate for the Excellent health group
# chronic main effect + healthexcellent:chronic interaction
combined_beta_nb <- coeffs_nb["chronic"] + coeffs_nb["healthexcellent:chronic"]
irr_nb <- exp(combined_beta_nb)
pct_change_nb <- (irr_nb - 1) * 100

# Output the results
cat("Combined Beta (NB):", combined_beta_nb, "\n")
cat("IRR for Excellent (NB):", irr_nb, "\n")
cat("Percentage Change (NB):", pct_change_nb, "%\n")



# 2e ----------------------------------------------------------------------
# Method: Likelihood Ratio Test comparing Poisson and NB models
# model_poisson is from 2b, model_nb is from 2d
lr_test_stat <- 2 * (logLik(model_nb) - logLik(model_int_poi))
p_value <- pchisq(lr_test_stat, df = 1, lower.tail = FALSE)
cat("Likelihood Ratio Test Statistic:", lr_test_stat, "\n")
cat("p-value:", p_value, "\n")



# Q3 ----------------------------------------------------------------------
#Read the dataset 
melanoma <- read.csv("data/Problem Set 3/melanoma.csv")
View(melanoma)

#Read the dataset 
melanoma_codebook <- read.csv("data/Problem Set 3/melanoma_codebook.csv")
View(melanoma_codebook)


# 3a ----------------------------------------------------------------------
# Turn into Factor
melanoma$latitude <- as.factor(trimws(melanoma$latitude))
melanoma$ageg <- as.factor(trimws(melanoma$ageg))
#  Levels name？
lev_lat <- levels(melanoma$latitude)
lev_age <- levels(melanoma$ageg)
print(lev_lat)
print(lev_age)
# Reference group
melanoma$latitude <- relevel(melanoma$latitude, ref = lev_lat[grep("Middle", lev_lat)])
melanoma$ageg <- relevel(melanoma$ageg, ref = lev_age[grep("35", lev_age)[1]])
# Model
model_melanoma <- glm(inccases ~ latitude + ageg, 
                      family = poisson, 
                      offset = log(persyrs), 
                      data = melanoma)
#See result
summary(model_melanoma)
exp(coef(model_melanoma))



# 3d ----------------------------------------------------------------------
# Define the person-years per age group
py_value <- 4000
# Create a prediction data frame for Birmingham (Southern)
birmingham_data <- data.frame(
  latitude = factor("Southern", levels = levels(melanoma$latitude)),
  ageg = levels(melanoma$ageg),
  persyrs = py_value
)
# Predict number of cases for each age group
predicted_counts <- predict(model_melanoma, newdata = birmingham_data, type = "response")
# Sum the predicted cases
total_cases <- sum(predicted_counts)
# Output results
print(predicted_counts)
cat("Estimated total melanoma cases:", total_cases, "\n")


# EXTRA CREDIT ------------------------------------------------------------
# Load Required Packages ---
library(nnet)
library(ggplot2)
library(tidyr)
# Create Hypothetical Data Frame for Prediction ---
# Note: Variables must match the types (numeric/factor) used in the original model.
# Define a sequence for chronic conditions from 0 to 8
chronic_seq <- seq(0, 8, by = 1)
plot_data <- data.frame(
  chronic = chronic_seq,
  age_10 = 7.5,             # 75 years old (scaled by 10)
  male = 0,                 # 0 represents Female (numeric as per model requirements)
  hospital = 0,             # 0 previous hospital stays
  school = 12,              # 12 years of education
  insurance = "no"          # No private insurance (matches factor level in model)
)

# Ensure 'health' is a factor and matches the levels from the original dataset
plot_data$health <- factor("average", levels = levels(nmes$health))

# Calculate Predicted Probabilities
# Using the multinomial model 'model_poly' created in Question 1e
probs <- predict(model_poly, newdata = plot_data, type = "probs")

# Reshape Data for Visualization 
# Combine the chronic sequence with the prediction matrix and pivot for ggplot
plot_df <- cbind(data.frame(chronic = chronic_seq), as.data.frame(probs))
plot_long <- pivot_longer(plot_df, 
                          cols = -chronic, 
                          names_to = "Quartile", 
                          values_to = "Probability")

# Generate the Plot 
ggplot(plot_long, aes(x = chronic, y = Probability, color = Quartile, group = Quartile)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 0:8) +
  scale_y_continuous(limits = c(0, 1)) + # Probabilities range from 0 to 1
  labs(title = "Predicted Probabilities of Physician Visit Quartiles",
       subtitle = "Profile: 75yo Female, Average Health, 0 Hospital Stays, 12yr Schooling",
       x = "Number of Chronic Conditions (0-8)",
       y = "Predicted Probability",
       color = "Quartile Group") +
  theme_minimal() +
  theme(legend.position = "right")


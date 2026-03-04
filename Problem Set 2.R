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
# Option A: Using Identity Link to get RD directly
# Note: This might fail to converge if predicted probabilities are <0 or >1
model_identity <- glm(switch ~ dist + arsenic + I(arsenic^2) + educ, 
                      family = binomial(link = "identity"), 
                      data = data)

# Extract coefficient for dist (this is the RD for 1 meter)
beta_dist <- coef(model_identity)["dist"]
ci_dist <- confint(model_identity)["dist", ]

# Calculate for 50-meter increase
rd_50m <- beta_dist * 50
ci_50m <- ci_dist * 50

# Format for reporting (%)
rd_pct <- round(rd_50m * 100, 1)
ci_low_pct <- round(ci_50m[1] * 100, 1)
ci_high_pct <- round(ci_50m[2] * 100, 1)

cat("RD for 50m (Identity Link):", rd_pct, "%\n")
cat("95% CI:", "[", ci_low_pct, "%,", ci_high_pct, "%]\n")

# 3a ----------------------------------------------------------------------



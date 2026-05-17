# 1. DATA CREATION & PREPROCESSING/CLEANING
# Simulating a dataset of Airline delays by Weather Condition to create a contingency table
airline_weather <- data.frame(
    Delay_Status = sample(c("Delayed", "On-Time", "Delayed", NA), 100, replace = TRUE),
    Weather = sample(c("Clear", "Rain", "Snow", "Clear", "Rain"), 100, replace = TRUE)
)

# Data Cleaning
# Remove missing values
airline_weather <- na.omit(airline_weather)

# Preprocessing: convert to factors
airline_weather$Delay_Status <- as.factor(airline_weather$Delay_Status)
airline_weather$Weather <- as.factor(airline_weather$Weather)

# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics ---\n")
print(summary(airline_weather))

# Create a contingency table
contingency_tab <- table(airline_weather$Delay_Status, airline_weather$Weather)
cat("\n--- Contingency Table ---\n")
print(contingency_tab)


# 3. TASK SPECIFIC OPERATIONS
# Calculate probabilities using chi-square distribution
# Here we perform the chi-square test to get the statistic and df
chisq_test <- chisq.test(contingency_tab)
chisq_statistic <- as.numeric(chisq_test$statistic)
degrees_of_freedom <- as.numeric(chisq_test$parameter)

cat("\n--- Chi-Square Probabilities ---\n")
# Find the cumulative probability (lower tail)
prob_lower <- pchisq(q = chisq_statistic, df = degrees_of_freedom)

# Find the p-value (upper tail probability)
p_value <- pchisq(q = chisq_statistic, df = degrees_of_freedom, lower.tail = FALSE)

cat("Chi-square Statistic:", chisq_statistic, "\n")
cat("Degrees of Freedom:", degrees_of_freedom, "\n")
cat("Probability (Lower Tail):", prob_lower, "\n")
cat("Probability (Upper Tail / P-value):", p_value, "\n")

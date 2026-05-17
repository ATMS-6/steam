# 1. DATA LOADING & PREPROCESSING/CLEANING
# Simulating count data for Poisson (e.g., number of customers per hour)
merch_events <- data.frame(
    Hour = 1:24,
    Customers = c(rpois(20, lambda = 5), NA, 3, -1, 5) # NAs and invalid negative
)

# Data Cleaning
# Remove missing values
merch_events <- na.omit(merch_events)

# Ensure counts are non-negative integers
merch_events <- merch_events[merch_events$Customers >= 0, ]
merch_events$Customers <- as.integer(merch_events$Customers)

# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics for Event Counts ---\n")
print(summary(merch_events$Customers))
cat("Variance of Counts:", var(merch_events$Customers), "\n")
cat("Mean of Counts:", mean(merch_events$Customers), "\n")
# For Poisson, Mean should roughly equal Variance


# 3. TASK SPECIFIC OPERATIONS
# Poisson distribution: probability of events occurring in a fixed interval
# Let lambda be the mean of our cleaned data
lambda_events <- mean(merch_events$Customers)

cat("\n--- Poisson Probabilities (lambda = ", round(lambda_events, 2), ") ---\n", sep="")

# Find the probability of exactly 3 events occurring
prob_exactly_3 <- dpois(x = 3, lambda = lambda_events)
cat("Probability of exactly 3 events:", prob_exactly_3, "\n")

# Find the probability of up to 3 events occurring
prob_up_to_3 <- ppois(q = 3, lambda = lambda_events)
cat("Probability of 3 or fewer events:", prob_up_to_3, "\n")

# Find the probability of more than 3 events occurring
prob_more_than_3 <- ppois(q = 3, lambda = lambda_events, lower.tail = FALSE)
cat("Probability of more than 3 events:", prob_more_than_3, "\n")

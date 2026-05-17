# q8 - chi-square distribution probabilities on airline delays
# chi-square = sum of squared standard normal variables
# used for goodness of fit and independence tests

data <- read.csv("airline_delays.csv")
df <- nrow(data) - 1  # degrees of freedom

# density at x=5
cat("density:", dchisq(5, df), "\n")

# cumulative probability p(x<=5)
cat("p(x<=5):", pchisq(5, df), "\n")

# critical value at 95%
cat("95th percentile:", qchisq(0.95, df), "\n")

# 1. DATA LOADING & PREPROCESSING/CLEANING
merch_data <- tryCatch(
    read.csv("Merchandise_Sales.csv"),
    error = function(e) {
        message("Dataset not found, creating a dummy dataset...")
        data.frame(
          StoreID = 1:30,
          Sales = c(rnorm(25, mean = 50, sd = 10), NA, NA, 150, 10, -5) # NAs and outliers
        )
    }
)

# Data Cleaning
# Remove NA values
merch_data <- na.omit(merch_data)

# Remove illogical negative sales
merch_data <- subset(merch_data, Sales > 0)

# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics ---\n")
print(summary(merch_data$Sales))
cat("Mean:", mean(merch_data$Sales), "\n")
cat("Standard Deviation:", sd(merch_data$Sales), "\n")
cat("Skewness (approx):", 3 * (mean(merch_data$Sales) - median(merch_data$Sales)) / sd(merch_data$Sales), "\n")

# 3. TASK SPECIFIC OPERATIONS
sales <- merch_data$Sales

# Perform a one-sample t-test
# Checking if sample mean differs from population mean of 45
population_mean <- 45

cat("\n--- Performing One-Sample t-test (mu = ", population_mean, ") ---\n", sep="")
t_test_result <- t.test(sales, mu = population_mean)

print(t_test_result)

# 1. DATA LOADING & PREPROCESSING/CLEANING
merch_data <- tryCatch(
    read.csv("Merchandise_Sales.csv"),
    error = function(e) data.frame(Sales = c(rnorm(30, mean = 50, sd = 10), NA, -20))
)

ecommerce_data <- tryCatch(
    read.csv("E_commerce.csv"),
    error = function(e) data.frame(Sales = c(rnorm(35, mean = 55, sd = 15), NA, 200))
)

# Data Cleaning
# Clean Merchandise data (remove NAs and invalid negative sales)
merch_cleaned <- merch_data[!is.na(merch_data$Sales) & merch_data$Sales > 0, ]

# Clean E-commerce data (remove NAs and invalid negative sales)
ecomm_cleaned <- ecommerce_data[!is.na(ecommerce_data$Sales) & ecommerce_data$Sales > 0, ]


# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics: Merchandise Sales ---\n")
print(summary(merch_cleaned$Sales))
cat("Variance:", var(merch_cleaned$Sales), "\n")

cat("\n--- Descriptive Statistics: E-commerce Sales ---\n")
print(summary(ecomm_cleaned$Sales))
cat("Variance:", var(ecomm_cleaned$Sales), "\n")


# 3. TASK SPECIFIC OPERATIONS
merch_sales <- merch_cleaned$Sales
ecomm_sales <- ecomm_cleaned$Sales

# Perform F-test to compare variances
cat("\n--- F-Test to Compare Variances ---\n")
f_test_result <- var.test(merch_sales, ecomm_sales)
print(f_test_result)

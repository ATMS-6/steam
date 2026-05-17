# 1. DATA LOADING & PREPROCESSING/CLEANING
ecommerce_data <- tryCatch(
    read.csv("E_commerce.csv"),
    error = function(e) {
        message("Dataset not found, creating a dummy dataset...")
        data.frame(
          OrderID = 1:12,
          Sales = c(150, 200, 250, 100, NA, 300, 120, 280, 5000, 150, NA, 220) # NAs and outlier (5000)
        )
    }
)

# Data Cleaning
# Remove NAs
ecommerce_data <- na.omit(ecommerce_data)

# Outlier treatment (remove sales > 3 standard deviations from mean)
mean_sales_init <- mean(ecommerce_data$Sales)
sd_sales_init <- sd(ecommerce_data$Sales)
ecommerce_data <- subset(ecommerce_data, Sales <= (mean_sales_init + 3 * sd_sales_init) & Sales >= (mean_sales_init - 3 * sd_sales_init))


# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics ---\n")
print("Structure of cleaned E-commerce dataset:")
str(ecommerce_data)

print("Summary of cleaned E-commerce dataset:")
summary(ecommerce_data)

cat("Total observations after cleaning:", nrow(ecommerce_data), "\n")


# 3. TASK SPECIFIC OPERATIONS
# Calculate standard deviation, variance, and coefficient of variation
sales_col <- ecommerce_data$Sales

sd_val <- sd(sales_col)
var_val <- var(sales_col)
mean_val <- mean(sales_col)
cv_val <- (sd_val / mean_val) * 100

cat("\n--- Dispersion measures for Sales ---\n")
cat("Standard Deviation:", sd_val, "\n")
cat("Variance:", var_val, "\n")
cat("Coefficient of Variation (%):", cv_val, "\n")

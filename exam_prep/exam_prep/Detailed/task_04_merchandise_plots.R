# 1. DATA LOADING & PREPROCESSING/CLEANING
merch_data <- tryCatch(
    read.csv("Merchandise_Sales.csv"),
    error = function(e) {
        message("Dataset not found, creating a dummy dataset...")
        data.frame(
          Date = as.Date('2023-01-01') + 0:19,
          Sales = c(12, 15, 14, 22, 25, 27, 32, 18, 19, 21, 35, 11, 16, NA, 14, 28, 30, NA, 19, 22)
        )
    }
)

# Data Cleaning
# Impute missing values with median
median_sales <- median(merch_data$Sales, na.rm = TRUE)
merch_data$Sales[is.na(merch_data$Sales)] <- median_sales

# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics ---\n")
print(summary(merch_data))
cat("Variance of Sales:", var(merch_data$Sales), "\n")
cat("Standard Deviation:", sd(merch_data$Sales), "\n")
cat("IQR:", IQR(merch_data$Sales), "\n")

# 3. TASK SPECIFIC OPERATIONS
sales <- merch_data$Sales

# Set up plotting area for multiple plots
par(mfrow=c(1,2))

# Plot a histogram
hist(sales, 
     main = "Histogram of Sales", 
     xlab = "Sales Value", 
     col = "skyblue", 
     border = "black")

# Plot box and whiskers
boxplot(sales, 
        main = "Boxplot of Sales", 
        ylab = "Sales Value", 
        col = "lightgreen")

# Reset plotting area
par(mfrow=c(1,1))

# Stem and leaf display
cat("\n--- Stem and Leaf Display for Merchandise Sales ---\n")
stem(sales)

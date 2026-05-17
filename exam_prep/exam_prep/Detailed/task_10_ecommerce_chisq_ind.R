# 1. DATA LOADING & PREPROCESSING/CLEANING
ecommerce_data <- tryCatch(
    read.csv("E_commerce.csv"),
    error = function(e) {
        message("Dataset not found, creating a dummy dataset...")
        data.frame(
            PaymentMethod = sample(c("CreditCard", "PayPal", "BankTransfer", " ", NA), 160, replace = TRUE),
            Device = sample(c("Mobile", "Desktop", "Tablet", NA), 160, replace = TRUE)
        )
    }
)

# Data Cleaning
# Remove rows with NA values
ecommerce_data <- na.omit(ecommerce_data)

# Remove rows with empty/invalid strings in PaymentMethod
ecommerce_data <- subset(ecommerce_data, trimws(PaymentMethod) != "")

# Data Preprocessing
# Convert to factors and drop unused levels
ecommerce_data$PaymentMethod <- droplevels(as.factor(ecommerce_data$PaymentMethod))
ecommerce_data$Device <- droplevels(as.factor(ecommerce_data$Device))


# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics ---\n")
print(summary(ecommerce_data))

# Create a contingency table between two categorical variables
contingency_table <- table(ecommerce_data$PaymentMethod, ecommerce_data$Device)

cat("\n--- Contingency Table (Cross-Tabulation) ---\n")
print(contingency_table)
cat("\nMarginal Totals (Payment Method):\n")
print(margin.table(contingency_table, 1))
cat("\nMarginal Totals (Device):\n")
print(margin.table(contingency_table, 2))


# 3. TASK SPECIFIC OPERATIONS
# Conduct a chi-square test for independence
cat("\n--- Chi-Square Test for Independence ---\n")
chisq_test_result <- chisq.test(contingency_table)
print(chisq_test_result)

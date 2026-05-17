# ============================================
# TASK 3: E-commerce - Structure, Summary, SD / Variance / CV
# ============================================

# READ DATASET
# ⚠️ Change filename to match what is given in exam
ecom <- read.csv("Ecommerce.csv")

# Structure and Summary
str(ecom)
summary(ecom)

# ⚠️ Change "Sales" to the numeric column name from your dataset
# Run str(ecom) first to see available column names
col <- ecom$Sales

# Standard Deviation
sd_val  <- sd(col, na.rm = TRUE)

# Variance
var_val <- var(col, na.rm = TRUE)

# Coefficient of Variation (CV = SD / Mean * 100)
cv_val  <- (sd_val / mean(col, na.rm = TRUE)) * 100

cat("Standard Deviation      :", sd_val,  "\n")
cat("Variance                :", var_val, "\n")
cat("Coefficient of Variation:", cv_val,  "%\n")

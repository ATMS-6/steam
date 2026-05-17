# ============================================
# TASK 10: Chi-Square Test for Independence (E-commerce)
# ============================================

# READ DATASET
# ⚠️ Change filename to match what is given in exam
ecom <- read.csv("Ecommerce.csv")

# Check column names first
# Run: str(ecom)  to see all column names and types
# Pick two CATEGORICAL (Factor / character) columns

# ⚠️ Change "Category" and "Region" to two categorical column names
# from your dataset
tab <- table(ecom$Category, ecom$Region)

cat("Contingency Table:\n")
print(tab)

# Chi-square test for independence
# H0: The two variables are independent
# H1: The two variables are NOT independent (association exists)
chi_result <- chisq.test(tab)
print(chi_result)

# Interpretation
if (chi_result$p.value < 0.05) {
  cat("p-value < 0.05 → Reject H0\n")
  cat("Conclusion: Variables are NOT independent (association exists).\n")
} else {
  cat("p-value >= 0.05 → Fail to Reject H0\n")
  cat("Conclusion: Variables are independent.\n")
}

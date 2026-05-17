# ============================================
# TASK 9: F-test - Compare Variances (Merchandise Sales vs E-commerce)
# ============================================

# READ DATASETS
# ⚠️ Change filenames to match what is given in exam
merch <- read.csv("MerchandiseSales.csv")
ecom  <- read.csv("Ecommerce.csv")

# ⚠️ Change column names to numeric columns from each dataset
col_merch <- merch$Sales    # Merchandise Sales column
col_ecom  <- ecom$Sales     # E-commerce column

# F-test
# H0: Variance of col_merch = Variance of col_ecom
# H1: Variances are different
f_result <- var.test(col_merch, col_ecom)
print(f_result)

# Interpretation
if (f_result$p.value < 0.05) {
  cat("p-value < 0.05 → Reject H0\n")
  cat("Conclusion: Variances are significantly different.\n")
} else {
  cat("p-value >= 0.05 → Fail to Reject H0\n")
  cat("Conclusion: No significant difference in variances.\n")
}

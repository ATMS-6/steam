# ============================================
# TASK 5: One-Sample t-test (Merchandise Sales)
# ============================================

# READ DATASET
# ⚠️ Change filename to match what is given in exam
merch <- read.csv("MerchandiseSales.csv")

# ⚠️ Change "Sales" to the numeric column name from your dataset
col <- merch$Sales

# ⚠️ Change mu to the population mean given in the exam question
mu <- 500

# Perform one-sample t-test
# H0: population mean = mu
# H1: population mean ≠ mu
t_result <- t.test(col, mu = mu)
print(t_result)

# Interpretation
if (t_result$p.value < 0.05) {
  cat("p-value < 0.05 → Reject H0\n")
  cat("Conclusion: Sample mean significantly differs from population mean.\n")
} else {
  cat("p-value >= 0.05 → Fail to Reject H0\n")
  cat("Conclusion: No significant difference from population mean.\n")
}

# ============================================
# TASK 8: Chi-Square Distribution Probabilities (Airline Delays)
# ============================================

# ⚠️ Change df_val to the degrees of freedom given in the exam question
df_val <- 5

# ⚠️ Change x_val to the chi-square statistic given in the exam question
x_val  <- 11.07

# P(X <= x_val) — cumulative probability
cat("P(X <=", x_val, ") with df =", df_val, ":",
    pchisq(x_val, df = df_val), "\n")

# P(X > x_val) — upper tail probability
cat("P(X  >", x_val, ") with df =", df_val, ":",
    1 - pchisq(x_val, df = df_val), "\n")

# Critical value at alpha = 0.05 (right tail)
alpha <- 0.05
cv    <- qchisq(1 - alpha, df = df_val)
cat("\nCritical value at alpha = 0.05, df =", df_val, ":", cv, "\n")

# Decision based on x_val vs critical value
if (x_val > cv) {
  cat("x_val > critical value → Reject H0\n")
} else {
  cat("x_val <= critical value → Fail to Reject H0\n")
}

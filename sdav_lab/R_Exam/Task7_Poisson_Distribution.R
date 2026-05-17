# ============================================
# TASK 7: Poisson Distribution (Merchandise Sales)
# ============================================

# READ DATASET
# ⚠️ Change filename to match what is given in exam
merch <- read.csv("MerchandiseSales.csv")

# ⚠️ Change "Sales" to the numeric column name from your dataset
col <- merch$Sales

# Lambda = mean of the data (average events per interval)
lambda <- mean(col, na.rm = TRUE)
cat("Lambda (mean):", lambda, "\n\n")

# P(X = k) for k = 0 to 5
k_vals <- 0:5
probs  <- dpois(k_vals, lambda = lambda)

cat("Poisson Probabilities:\n")
print(data.frame(k = k_vals, Probability = round(probs, 6)))

# Cumulative probability: P(X <= 3)
cat("\nP(X <= 3):", ppois(3, lambda = lambda), "\n")

# P(X > 3)
cat("P(X  > 3):", 1 - ppois(3, lambda = lambda), "\n")

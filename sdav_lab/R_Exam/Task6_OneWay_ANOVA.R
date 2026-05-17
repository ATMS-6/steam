# ============================================
# TASK 6: One-Way ANOVA (Airline Delays - 3 Groups)
# ============================================

# READ DATASET
# ⚠️ Change filename to match what is given in exam
airline <- read.csv("AirlineDelays.csv")

# Check available groups first
# Run this line to see group names in your dataset:
# unique(airline$Carrier)

# ⚠️ Change "Carrier" to your grouping column name
# ⚠️ Change "AA","UA","DL" to 3 actual group values from your dataset
groups <- airline[airline$Carrier %in% c("AA", "UA", "DL"), ]

# ⚠️ Change "DepDelay" to your numeric column name
# ⚠️ Change "Carrier"  to your grouping column name
anova_result <- aov(DepDelay ~ Carrier, data = groups)
summary(anova_result)

# Interpretation
p_val <- summary(anova_result)[[1]][["Pr(>F)"]][1]
if (p_val < 0.05) {
  cat("p-value < 0.05 → Reject H0\n")
  cat("Conclusion: Significant difference in means across groups.\n")
} else {
  cat("p-value >= 0.05 → Fail to Reject H0\n")
  cat("Conclusion: No significant difference in means across groups.\n")
}

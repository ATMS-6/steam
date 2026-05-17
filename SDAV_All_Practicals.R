# ================================================================
#   SDAV PRACTICALS 1-10 — SINGLE CSV DATASET
#   File: sdav_data.csv  (keep it in the same folder as this script)
#
#   DATASET COLUMNS:
#   Name        : Student name              (character)
#   Marks       : Exam marks 0-100          (numeric)
#   Grade       : Letter grade              (character/factor)
#   Sales       : Merchandise sales value   (numeric)
#   Order_Value : E-commerce order value    (numeric)
#   Delay_Min   : Airline delay in minutes  (numeric)
#   Airline     : IndiGo / Air India / SpiceJet  (factor — 3 groups)
#   Category    : Product category          (factor)
#   Returns     : Yes / No                  (factor)
# ================================================================


# ---------------------------------------------------------------
# STEP 0 — Read the CSV (do this ONCE; all practicals use it)
# ---------------------------------------------------------------

# Set working directory to where sdav_data.csv is saved, e.g.:
# setwd("C:/Users/YourName/Documents/SDAV")   # Windows
# setwd("~/Documents/SDAV")                   # Mac/Linux

sdav_data <- read.csv("sdav_data.csv", stringsAsFactors = TRUE)

cat("Dataset loaded successfully!\n")
cat("Rows:", nrow(sdav_data), " | Columns:", ncol(sdav_data), "\n")
cat("Column names:", paste(names(sdav_data), collapse = ", "), "\n\n")


# ================================================================
# PRACTICAL 1 — DataFrame, Subsetting, ggplot2, Scatter Plot
# ================================================================
cat("========== PRACTICAL 1 ==========\n")

# Display the data frame
cat("Student DataFrame (first 10 rows):\n")
print(head(sdav_data[, c("Name","Marks","Grade")], 10))

# Subset: students scoring above 75
above_75 <- subset(sdav_data, Marks > 75)
cat("\nStudents scoring above 75:\n")
print(above_75[, c("Name","Marks","Grade")])
cat("Total students above 75:", nrow(above_75), "out of", nrow(sdav_data), "\n")

# Install and load ggplot2
if (!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
library(ggplot2)

# Scatter plot — Marks vs Sales, colour by Grade
p1 <- ggplot(sdav_data, aes(x = Marks, y = Sales, color = Grade)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_vline(xintercept = 75, linetype = "dashed", color = "red") +
  annotate("text", x = 76, y = max(sdav_data$Sales) - 10,
           label = "Marks = 75", color = "red", hjust = 0, size = 3.5) +
  labs(title = "Scatter Plot: Student Marks vs Sales",
       x = "Marks (out of 100)", y = "Sales Value", color = "Grade") +
  theme_minimal()
print(p1)
cat("Scatter plot displayed.\n")


# ================================================================
# PRACTICAL 2 — Structure, Summary, Mean / Median / Mode
#               (Airline Delays — Delay_Min column)
# ================================================================
cat("\n========== PRACTICAL 2 ==========\n")

cat("Structure of dataset:\n")
str(sdav_data)

cat("\nSummary of dataset:\n")
print(summary(sdav_data))

# Central tendency on Delay_Min
delay_vec <- sdav_data$Delay_Min

cat("\n--- Central Tendency for Delay_Min (Airline Delays) ---\n")
cat("Mean   :", round(mean(delay_vec), 2), "minutes\n")
cat("Median :", median(delay_vec), "minutes\n")

# Mode — base R has no built-in mode for statistics, so we write one
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
cat("Mode   :", get_mode(delay_vec), "minutes\n")

cat("\nMean Delay_Min by Airline:\n")
print(tapply(sdav_data$Delay_Min, sdav_data$Airline, mean))


# ================================================================
# PRACTICAL 3 — Structure/Summary, SD/Variance/CV (E-commerce)
#               Histogram, Box Plot, Stem-and-Leaf (Sales)
# ================================================================
cat("\n========== PRACTICAL 3 ==========\n")

cat("Structure of E-commerce related columns:\n")
str(sdav_data[, c("Category","Order_Value","Returns")])

cat("\nSummary of Order_Value (E-commerce):\n")
print(summary(sdav_data$Order_Value))

# --- Dispersion: Order_Value ---
ov <- sdav_data$Order_Value
cat("\n--- Dispersion for Order_Value (E-commerce dataset) ---\n")
cat("Mean                     :", round(mean(ov), 2), "\n")
cat("Standard Deviation       :", round(sd(ov), 2), "\n")
cat("Variance                 :", round(var(ov), 2), "\n")
cat("Coefficient of Variation :", round((sd(ov)/mean(ov))*100, 2), "%\n")

# --- Dispersion: Sales ---
sv <- sdav_data$Sales
cat("\n--- Dispersion for Sales (Merchandise dataset) ---\n")
cat("Mean                     :", round(mean(sv), 2), "\n")
cat("Standard Deviation       :", round(sd(sv), 2), "\n")
cat("Variance                 :", round(var(sv), 2), "\n")
cat("Coefficient of Variation :", round((sd(sv)/mean(sv))*100, 2), "%\n")

# Histogram of Sales
p3a <- ggplot(sdav_data, aes(x = Sales)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "white", alpha = 0.85) +
  labs(title = "Histogram — Merchandise Sales",
       x = "Sales Value", y = "Frequency") +
  theme_minimal()
print(p3a)

# Box and Whisker Plot — Sales by Airline
p3b <- ggplot(sdav_data, aes(x = Airline, y = Sales, fill = Airline)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Box & Whisker Plot — Sales by Airline Group",
       x = "Airline", y = "Sales Value") +
  theme_minimal() +
  theme(legend.position = "none")
print(p3b)

# Stem-and-Leaf
cat("\nStem-and-Leaf Display for Sales:\n")
stem(sdav_data$Sales)


# ================================================================
# PRACTICAL 4 — One-Sample t-Test on Sales
#               H0: population mean = 500
# ================================================================
cat("\n========== PRACTICAL 4 ==========\n")

pop_mean <- 500
cat("One-Sample t-Test: Does sample mean of Sales differ from", pop_mean, "?\n\n")

t_result <- t.test(sdav_data$Sales,
                   mu           = pop_mean,
                   alternative  = "two.sided",
                   conf.level   = 0.95)
print(t_result)

cat("\nSummary:\n")
cat("Sample Mean :", round(mean(sdav_data$Sales), 2), "\n")
cat("t-statistic :", round(t_result$statistic, 4), "\n")
cat("p-value     :", round(t_result$p.value, 4), "\n")
cat("95% CI      : [", round(t_result$conf.int[1], 2),
    ",", round(t_result$conf.int[2], 2), "]\n")

if (t_result$p.value < 0.05) {
  cat("Decision: REJECT H0 — Sales mean differs significantly from", pop_mean, "\n")
} else {
  cat("Decision: FAIL TO REJECT H0 — No significant difference from", pop_mean, "\n")
}


# ================================================================
# PRACTICAL 5 — One-Way ANOVA on Delay_Min across 3 Airlines
# ================================================================
cat("\n========== PRACTICAL 5 ==========\n")

cat("One-Way ANOVA: Delay_Min across IndiGo / Air India / SpiceJet\n")
cat("\nGroup means:\n")
print(tapply(sdav_data$Delay_Min, sdav_data$Airline, mean))

anova_model <- aov(Delay_Min ~ Airline, data = sdav_data)
cat("\nANOVA Table:\n")
print(summary(anova_model))

anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
cat("\nInterpretation:\n")
cat("p-value:", round(anova_p, 6), "\n")
if (anova_p < 0.05) {
  cat("Decision: REJECT H0 — At least one airline has a significantly different mean delay.\n")
} else {
  cat("Decision: FAIL TO REJECT H0 — No significant difference across airlines.\n")
}

cat("\nTukey HSD Post-Hoc Test (which pairs differ?):\n")
print(TukeyHSD(anova_model))


# ================================================================
# PRACTICAL 6 — Poisson Distribution on Sales
#               Lambda = mean Sales / 100  (daily event rate)
# ================================================================
cat("\n========== PRACTICAL 6 ==========\n")

lambda <- round(mean(sdav_data$Sales) / 100)
cat("Lambda (mean daily events) =", lambda,
    " [derived as mean(Sales)/100 =",
    round(mean(sdav_data$Sales)/100, 2), "]\n\n")

cat(sprintf("%-6s  %-18s  %-18s\n", "k", "P(X = k)", "P(X <= k)"))
cat(strrep("-", 46), "\n")
for (k in 0:12) {
  cat(sprintf("%-6d  %-18.6f  %-18.6f\n",
              k, dpois(k, lambda), ppois(k, lambda)))
}

cat("\nKey probabilities:\n")
cat("P(X = lambda)         :", round(dpois(lambda, lambda), 4), "\n")
cat("P(X <= lambda)        :", round(ppois(lambda, lambda), 4), "\n")
cat("P(X > lambda + 2)     :", round(ppois(lambda+2, lambda, lower.tail=FALSE), 4), "\n")
cat("P(X = 0) (no events)  :", round(dpois(0, lambda), 6), "\n")

# Poisson PMF bar plot
poisson_df <- data.frame(k    = 0:15,
                         prob = dpois(0:15, lambda))
p6 <- ggplot(poisson_df, aes(x = k, y = prob)) +
  geom_col(fill = "coral", color = "white", alpha = 0.85, width = 0.7) +
  geom_vline(xintercept = lambda, linetype = "dashed", color = "navy") +
  annotate("text", x = lambda + 0.5, y = max(poisson_df$prob) * 0.92,
           label = paste0("λ = ", lambda), hjust = 0, color = "navy", size = 3.5) +
  labs(title = paste0("Poisson Distribution (λ = ", lambda, ")"),
       x = "Number of Events (k)", y = "P(X = k)") +
  theme_minimal()
print(p6)


# ================================================================
# PRACTICAL 7 — Chi-Square Distribution on Delay_Min
# ================================================================
cat("\n========== PRACTICAL 7 ==========\n")

df_chi <- 5   # degrees of freedom = bins - 1
cat("Chi-Square Distribution (df =", df_chi, "):\n\n")
cat(sprintf("%-8s  %-16s  %-16s  %-14s\n",
            "x", "P(X <= x)", "P(X > x)", "Density f(x)"))
cat(strrep("-", 58), "\n")
for (x in c(1, 2, 3, 5, 7, 9, 11, 13, 15)) {
  cat(sprintf("%-8.1f  %-16.6f  %-16.6f  %-14.6f\n",
              as.numeric(x),
              pchisq(x, df_chi),
              pchisq(x, df_chi, lower.tail = FALSE),
              dchisq(x, df_chi)))
}

cv_chi <- qchisq(0.95, df = df_chi)
cat("\nCritical value (alpha=0.05, df=5):", round(cv_chi, 4), "\n")

# Goodness-of-fit: bin Delay_Min into 6 equal-width groups
delay_bins <- cut(sdav_data$Delay_Min,
                  breaks = 6,
                  include.lowest = TRUE)
obs_freq <- table(delay_bins)
cat("\nObserved frequencies (Delay_Min in 6 bins):\n")
print(obs_freq)

gof <- chisq.test(obs_freq, p = rep(1/6, 6))
cat("\nChi-Square Goodness-of-Fit (uniform distribution):\n")
print(gof)

cat("\nInterpretation:\n")
if (gof$p.value < 0.05) {
  cat("p-value =", round(gof$p.value, 4),
      "< 0.05: Delays do NOT follow a uniform distribution.\n")
} else {
  cat("p-value =", round(gof$p.value, 4),
      ">= 0.05: No significant deviation from uniform distribution.\n")
}


# ================================================================
# PRACTICAL 8 — F-Test: Compare Variances of Sales vs Order_Value
# ================================================================
cat("\n========== PRACTICAL 8 ==========\n")

cat("Dataset 1 — Merchandise Sales:\n")
cat("  n =", length(sdav_data$Sales),
    " | Mean =", round(mean(sdav_data$Sales), 2),
    " | Var =",  round(var(sdav_data$Sales),  2),
    " | SD =",   round(sd(sdav_data$Sales),   2), "\n")

cat("Dataset 2 — E-commerce Order_Value:\n")
cat("  n =", length(sdav_data$Order_Value),
    " | Mean =", round(mean(sdav_data$Order_Value), 2),
    " | Var =",  round(var(sdav_data$Order_Value),  2),
    " | SD =",   round(sd(sdav_data$Order_Value),   2), "\n\n")

f_result <- var.test(sdav_data$Sales, sdav_data$Order_Value,
                     alternative = "two.sided", conf.level = 0.95)
print(f_result)

cat("\nInterpretation:\n")
cat("F-statistic:", round(f_result$statistic, 4), "\n")
cat("p-value    :", round(f_result$p.value, 4), "\n")
if (f_result$p.value < 0.05) {
  cat("Decision: REJECT H0 — Variances are significantly different.\n")
} else {
  cat("Decision: FAIL TO REJECT H0 — No significant difference in variances.\n")
}


# ================================================================
# PRACTICAL 9 — Chi-Square Test of Independence
#               Category vs Returns
# ================================================================
cat("\n========== PRACTICAL 9 ==========\n")

cat("Chi-Square Independence Test: Category vs Returns\n\n")

cont_table <- table(sdav_data$Category, sdav_data$Returns)
cat("Contingency Table:\n")
print(cont_table)

chi_indep <- chisq.test(cont_table)
cat("\nTest Result:\n")
print(chi_indep)

cat("\nExpected Frequencies:\n")
print(round(chi_indep$expected, 2))

cat("\nInterpretation:\n")
cat("Chi-square statistic:", round(chi_indep$statistic, 4), "\n")
cat("Degrees of freedom  :", chi_indep$parameter, "\n")
cat("p-value             :", round(chi_indep$p.value, 4), "\n")
if (chi_indep$p.value < 0.05) {
  cat("Decision: REJECT H0 — Category and Returns are associated (not independent).\n")
} else {
  cat("Decision: FAIL TO REJECT H0 — Category and Returns are independent.\n")
}


# ================================================================
# PRACTICAL 10 — User-Defined Function & If-Else
# ================================================================
cat("\n========== PRACTICAL 10 ==========\n")

# ---- A: Function to compute average ----
compute_average <- function(vec) {
  if (!is.numeric(vec)) stop("Input must be a numeric vector.")
  if (length(vec) == 0) stop("Vector cannot be empty.")
  return(sum(vec) / length(vec))
}

cat("Testing compute_average() on all numeric columns:\n")
cat("Average Marks       :", round(compute_average(sdav_data$Marks), 2), "\n")
cat("Average Sales       :", round(compute_average(sdav_data$Sales), 2), "\n")
cat("Average Order_Value :", round(compute_average(sdav_data$Order_Value), 2), "\n")
cat("Average Delay_Min   :", round(compute_average(sdav_data$Delay_Min), 2), "\n")

cat("\nVerification — compute_average vs mean():\n")
cat("Marks  : compute_average =", round(compute_average(sdav_data$Marks), 2),
    " | mean =", round(mean(sdav_data$Marks), 2),
    " | Match:", isTRUE(all.equal(compute_average(sdav_data$Marks), mean(sdav_data$Marks))), "\n")

# ---- B: If-Else — classify number ----
classify_number <- function(n) {
  if (n > 0) {
    cat(n, "-> POSITIVE\n")
  } else if (n < 0) {
    cat(n, "-> NEGATIVE\n")
  } else {
    cat(n, "-> ZERO\n")
  }
}

cat("\nIf-Else: Classify numbers as Positive / Negative / Zero:\n")
test_nums <- c(45, -12, 0, 100, -0.5, 0.001, -99, 0)
for (num in test_nums) classify_number(num)

# Bonus: classify Marks from dataset
sdav_data$Performance <- ifelse(sdav_data$Marks > 75, "High",
                         ifelse(sdav_data$Marks >= 50, "Average", "Low"))
cat("\nPerformance classification from dataset:\n")
print(table(sdav_data$Performance))

cat("\n========== ALL 10 PRACTICALS DONE ==========\n")
cat("Single file used: sdav_data.csv\n")
cat("Rows:", nrow(sdav_data), "| Columns:", ncol(sdav_data), "\n")

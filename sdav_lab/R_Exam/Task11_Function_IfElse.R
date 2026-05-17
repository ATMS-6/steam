# ============================================
# TASK 11: Custom Average Function + if-else Number Classifier
# ============================================

# --- PART A: Custom function to compute average ---
my_avg <- function(x) {
  avg <- sum(x) / length(x)
  return(avg)
}

# Test the function
v <- c(10, 20, 30, 40, 50)
cat("Vector       :", v, "\n")
cat("Average      :", my_avg(v), "\n")   # Expected: 30

# Cross-check with built-in mean()
cat("Built-in mean:", mean(v), "\n")


# --- PART B: if-else to classify a number ---
classify_number <- function(n) {
  if (n > 0) {
    cat(n, "is Positive\n")
  } else if (n < 0) {
    cat(n, "is Negative\n")
  } else {
    cat(n, "is Zero\n")
  }
}

# Test cases
classify_number(15)
classify_number(-7)
classify_number(0)

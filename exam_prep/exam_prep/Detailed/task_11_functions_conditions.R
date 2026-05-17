# 1. & 2. PREPROCESSING / DESCRIPTIVE STATS WITH FUNCTIONS
# Create a dummy dataset with missing/messy values
test_data <- data.frame(
  ID = 1:6,
  Values = c(5, 10, 15, NA, 20, -5)
)

cat("--- Original Data ---\n")
print(test_data)

# Data Cleaning: Removing NAs and negatives for our specific test
clean_data <- na.omit(test_data)
clean_data <- clean_data[clean_data$Values >= 0, ]

cat("\n--- Cleaned Data ---\n")
print(clean_data)

cat("\n--- Descriptive Statistics ---\n")
print(summary(clean_data))


# 3. TASK SPECIFIC OPERATIONS
# Create a function to compute the average of a numeric vector
compute_average <- function(vec) {
  # Preprocessing inside function
  if (!is.numeric(vec)) {
    stop("Input must be a numeric vector.")
  }
  
  # Calculate average ignoring NA values (Cleaning inside function)
  total_sum <- sum(vec, na.rm = TRUE)
  valid_length <- length(vec[!is.na(vec)])
  
  if (valid_length == 0) return(NA)
  return(total_sum / valid_length)
}

# Test the function on the messy vector
test_vec <- test_data$Values
cat("\n--- Function Testing ---\n")
cat("Average of messy vector", toString(test_vec), "is:", compute_average(test_vec), "\n\n")


# R program using if-else to classify a number as positive, negative, or zero
classify_number <- function(num) {
  if (is.na(num)) {
    cat("Missing value cannot be classified.\n")
  } else if (num > 0) {
    cat(num, "is Positive.\n")
  } else if (num < 0) {
    cat(num, "is Negative.\n")
  } else {
    cat(num, "is Zero.\n")
  }
}

# Test the classification program
cat("Classifying numbers:\n")
classify_number(12)
classify_number(-7.5)
classify_number(0)
classify_number(NA)

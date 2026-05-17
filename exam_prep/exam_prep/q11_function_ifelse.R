# q11 - function for average and if-else classification

# function to compute average of numeric vector
avg <- function(x) {
  return(sum(x) / length(x))
}

# test the function
nums <- c(10, 20, 30, 40, 50)
cat("average:", avg(nums), "\n")

# if-else: classify number as positive, negative, or zero
n <- -5  # change this to test different values

if (n > 0) {
  cat(n, "is positive\n")
} else if (n < 0) {
  cat(n, "is negative\n")
} else {
  cat(n, "is zero\n")
}

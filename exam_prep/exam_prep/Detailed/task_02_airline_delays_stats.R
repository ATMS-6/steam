# 1. DATA LOADING & PREPROCESSING/CLEANING
airline_data <- tryCatch(
    read.csv("Airline_Delays.csv"),
    error = function(e) {
        message("Dataset not found, creating a dummy dataset...")
        data.frame(
          FlightID = 1:15,
          Delay = c(10, 20, 15, 20, 30, NA, 20, 45, 10, -5, 20, 15, NA, 60, 10), # NAs and negative (invalid)
          Airline = c(rep("A", 5), rep("B", 5), rep("C", 5))
        )
    }
)

cat("--- Original Data (First 6 rows) ---\n")
print(head(airline_data))

# Data Cleaning
# Remove invalid negative delays
airline_data <- airline_data[airline_data$Delay >= 0 | is.na(airline_data$Delay), ]

# Handle missing values: Remove rows with NA in Delay
airline_data <- na.omit(airline_data)

# Preprocessing: Convert Airline to factor
airline_data$Airline <- as.factor(airline_data$Airline)

# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics ---\n")
print("Structure of Airline Delays dataset:")
str(airline_data)

print("Summary of Airline Delays dataset:")
summary(airline_data)

# 3. TASK SPECIFIC OPERATIONS
# Compute mean, median, and mode
delay_column <- airline_data$Delay

mean_val <- mean(delay_column)
median_val <- median(delay_column)

# Custom function for mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_val <- get_mode(delay_column)

cat("\n--- Delay Statistics ---\n")
cat("Mean:", mean_val, "\n")
cat("Median:", median_val, "\n")
cat("Mode:", mode_val, "\n")

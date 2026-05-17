# 1. DATA LOADING & PREPROCESSING/CLEANING
airline_data <- tryCatch(
    read.csv("Airline_Delays.csv"),
    error = function(e) {
        message("Dataset not found, creating a dummy dataset...")
        data.frame(
            Delay = c(rnorm(15, 10, 5), rnorm(15, 15, 5), rnorm(15, 20, 5), NA, 50, -10),
            AirlineGroup = c(rep(c("AirlineA", "AirlineB", "AirlineC"), each = 15), "AirlineA", "AirlineB", "AirlineC")
        )
    }
)

# Data Cleaning
# Remove NAs
airline_data <- na.omit(airline_data)

# Remove negative delays (data entry errors)
airline_data <- airline_data[airline_data$Delay >= 0, ]

# Data Preprocessing
# Ensure AirlineGroup is a factor
airline_data$AirlineGroup <- as.factor(airline_data$AirlineGroup)

# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics ---\n")
print(summary(airline_data))

# Descriptive stats by group
cat("\n--- Mean Delay by Airline Group ---\n")
aggregate(Delay ~ AirlineGroup, data = airline_data, mean)

cat("\n--- Variance of Delay by Airline Group ---\n")
aggregate(Delay ~ AirlineGroup, data = airline_data, var)


# 3. TASK SPECIFIC OPERATIONS
# Perform one-way ANOVA to compare means across three groups
cat("\n--- One-Way ANOVA Result ---\n")
anova_result <- aov(Delay ~ AirlineGroup, data = airline_data)
print(summary(anova_result))

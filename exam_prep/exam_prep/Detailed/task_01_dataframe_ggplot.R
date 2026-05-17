# 1. DATA CREATION & PREPROCESSING/CLEANING
# Create a data frame with some deliberate missing and messy data
students <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David", "Eve", "Frank", "Grace", "Alice"), # Duplicate Alice
  Marks = c(85, 65, 92, 78, 55, NA, 88, 85), # Missing mark for Frank
  Grade = c("A", "C", "A", "B", "D", "F", "A", "A")
)

cat("--- Original Data ---\n")
print(students)

# Data Cleaning
# Remove duplicates
students <- unique(students)

# Handle missing values (Impute with mean for Marks)
mean_marks <- mean(students$Marks, na.rm = TRUE)
students$Marks[is.na(students$Marks)] <- mean_marks

# Data Preprocessing: Convert Grade to factor
students$Grade <- as.factor(students$Grade)

# 2. DESCRIPTIVE STATISTICS
cat("\n--- Descriptive Statistics ---\n")
print(dim(students))
print(str(students))
print(summary(students))
cat("Standard Deviation of Marks:", sd(students$Marks), "\n")

# 3. TASK SPECIFIC OPERATIONS
# Perform subsetting to display students scoring above 75
high_scorers <- subset(students, Marks > 75)
cat("\n--- Students scoring above 75 ---\n")
print(high_scorers)

# Install and load the ggplot2 package
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Create a simple scatter plot using the created data frame
ggplot(students, aes(x = Name, y = Marks, color = Grade)) +
  geom_point(size = 4) +
  ggtitle("Student Marks") +
  theme_minimal()

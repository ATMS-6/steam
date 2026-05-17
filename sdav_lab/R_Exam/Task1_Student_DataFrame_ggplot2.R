# ============================================
# TASK 1: Student Data Frame + ggplot2 Scatter Plot
# ============================================

# Create data frame
students <- data.frame(
  Name  = c("Alice","Bob","Charlie","Diana","Eve","Frank"),
  Marks = c(85, 60, 90, 72, 78, 55),
  Grade = c("A","C","A","B","B","D")
)

print("Full Student Data:")
print(students)

# Subset: students scoring above 75
above75 <- students[students$Marks > 75, ]
print("Students scoring above 75:")
print(above75)

# Install & load ggplot2
# install.packages("ggplot2")   # Uncomment if not installed
library(ggplot2)

# Scatter plot
ggplot(students, aes(x = Name, y = Marks, color = Grade)) +
  geom_point(size = 4) +
  labs(title = "Student Marks Scatter Plot", x = "Student", y = "Marks") +
  theme_minimal()

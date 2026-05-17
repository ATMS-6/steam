# q4 - merchandise sales: histogram, boxplot, stem-leaf

# read dataset
data <- read.csv("merchandise_sales.csv")
sales <- data$sales  # use correct column name from exam

# histogram - shows frequency distribution of continuous data
hist(sales, main="histogram of sales", xlab="sales", col="steelblue", border="white")

# box and whiskers plot - shows min, q1, median, q3, max, outliers
boxplot(sales, main="boxplot of sales", ylab="sales", col="orange")

# stem and leaf display - shows distribution preserving actual values
stem(sales)

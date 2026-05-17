# q3 - ecommerce: read, structure, summary, sd variance cv

# read dataset
data <- read.csv("ecommerce.csv")

# structure and summary
str(data)
summary(data)

# sd = sqrt(variance), variance = avg squared deviation
# cv = (sd/mean)*100 - relative measure of dispersion
s <- sd(data$price, na.rm=TRUE)
v <- var(data$price, na.rm=TRUE)
cv <- (s / mean(data$price, na.rm=TRUE)) * 100

cat("sd:", s, "\nvariance:", v, "\ncv:", cv, "%\n")

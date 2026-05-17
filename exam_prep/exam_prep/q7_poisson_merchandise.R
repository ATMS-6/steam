# q7 - poisson distribution on merchandise sales
# poisson models rare events at constant rate lambda
# p(x=k) = (e^-lambda * lambda^k) / k!

data <- read.csv("merchandise_sales.csv")
lambda <- mean(data$sales)  # use correct column

# probability of exactly k events
k <- 5  # replace with value given in exam
cat("p(x=", k, ") =", dpois(k, lambda), "\n")

# cumulative probability p(x<=k)
cat("p(x<=", k, ") =", ppois(k, lambda), "\n")

# 95th percentile
cat("95th percentile:", qpois(0.95, lambda), "\n")

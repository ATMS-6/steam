# q9 - f-test to compare variances of merchandise and ecommerce
# h0: sigma1^2 = sigma2^2 (equal variances)
# h1: sigma1^2 != sigma2^2
# f = s1^2 / s2^2, compare to f-distribution

d1 <- read.csv("merchandise_sales.csv")
d2 <- read.csv("ecommerce.csv")

# replace column names with correct ones from exam
var.test(d1$sales, d2$price)

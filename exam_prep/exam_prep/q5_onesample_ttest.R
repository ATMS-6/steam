# q5 - one sample t-test on merchandise sales
# tests if sample mean differs from population mean
# h0: mu = mu0, h1: mu != mu0
# uses t-distribution with n-1 df when sigma unknown

data <- read.csv("merchandise_sales.csv")

# replace 50 with given population mean in exam
t.test(data$sales, mu=50)

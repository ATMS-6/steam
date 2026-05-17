# q10 - chi-square test for independence on ecommerce
# tests if two categorical variables are associated
# h0: variables are independent
# h1: variables are associated
# chi-sq = sum((o-e)^2 / e)

data <- read.csv("ecommerce.csv")

# replace column names with correct categorical variables from exam
tbl <- table(data$category, data$region)
print(tbl)
chisq.test(tbl)

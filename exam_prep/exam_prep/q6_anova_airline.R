# q6 - one way anova on airline delays
# compares means across 3+ groups
# h0: all group means are equal
# h1: at least one group mean differs
# f = msb/msw, large f means groups differ

data <- read.csv("airline_delays.csv")

# replace delay~airline with correct column names from exam
result <- aov(delay ~ airline, data=data)
summary(result)

# post-hoc if significant
TukeyHSD(result)

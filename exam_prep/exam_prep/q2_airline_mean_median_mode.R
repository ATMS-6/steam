# q2 - airline delays: read, structure, summary, mean median mode

# read dataset
data <- read.csv("airline_delays.csv")

# structure and summary
str(data)
summary(data)

# mean median mode
# mean = sum/n, median = middle value, mode = most frequent
m <- mean(data$delay, na.rm=TRUE)
med <- median(data$delay, na.rm=TRUE)

# mode - r has no built in mode function
getmode <- function(v) {
  t <- table(v)
  as.numeric(names(t)[which.max(t)])
}
mo <- getmode(data$delay)

cat("mean:", m, "\nmedian:", med, "\nmode:", mo, "\n")

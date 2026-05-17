# ============================================
# TASK 2: Airline Delays - Structure, Summary, Mean/Median/Mode
# ============================================

# READ DATASET
# ⚠️ Change filename to match what is given in exam
airline <- read.csv("AirlineDelays.csv")

# Structure and Summary
str(airline)
summary(airline)

# ⚠️ Change "DepDelay" to the numeric column name from your dataset
# Run str(airline) first to see available column names
col <- airline$DepDelay

# Mean
cat("Mean   :", mean(col, na.rm = TRUE), "\n")

# Median
cat("Median :", median(col, na.rm = TRUE), "\n")

# Mode (no built-in function in R, so we define one)
get_mode <- function(x) {
  x  <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
cat("Mode   :", get_mode(col), "\n")

# q1 - data frame, subsetting above 75, ggplot2 scatter

# create data frame
student <- data.frame(
  name = c("a","b","c","d","e"),
  marks = c(85, 62, 91, 45, 78),
  grade = c("a","b","a","c","b")
)
print(student)

# subset marks > 75
above75 <- student[student$marks > 75, ]
cat("students scoring above 75:\n")
print(above75)

# install and load ggplot2
# install.packages("ggplot2")
library(ggplot2)

# scatter plot
ggplot(student, aes(x=name, y=marks)) +
  geom_point(color="blue", size=3) +
  ggtitle("student marks scatter plot")

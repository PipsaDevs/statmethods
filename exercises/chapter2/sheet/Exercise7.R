# 7. Find the Spearman correlation coefficient between the working hours and the number of errors
# committed in the table from the exercise 5.

source("R/chapter2_formulas.R")

x <- c(2, 2, 3, 3, 4, 4, 5, 6, 7, 7)
y <- c(0, 1, 1, 1, 3, 3, 6, 7, 8, 7)

pipsa_linear_correlation(x, y)

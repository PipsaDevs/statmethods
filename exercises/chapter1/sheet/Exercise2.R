# The grades of 50 students in a certain subject are
# 5 − 2 − 4 − 9 − 5 − 7 − 4 − 5 − 6 − 5 − 7 − 7 − 5 − 5 − 2 − 10 − 5 − 6 − 5 − 4 −
# 5 − 8 − 8 − 4 − 0 − 8 − 4 − 8 − 6 − 6 − 3 − 6 − 7 − 6 − 6 − 7 − 6 − 7 − 3 − 5 −
# 6 − 9 − 6 − 1 − 4 − 6 − 3 − 5 − 5 − 6 − 7.
# Construct the table of absolute and relative frequencies (both ordinary and cumulative) and
# create a bar chart to represent the relative frequencies graphically.

source("R/chapter1_formulas.R")

grades <- c(5, 2, 4, 9, 5, 7, 4, 5, 6, 5, 7, 7, 5, 5, 2, 10, 5, 6, 5, 4, 5, 8, 8, 4, 0, 8, 4, 8, 6, 6, 3, 6, 7, 6, 6, 7, 6, 7, 3, 5, 6, 9, 6, 1, 4, 6, 3, 5, 5, 6, 7)

pipsa_print(pipsa_absolute_freq(grades), "absolute freq:")
pipsa_print(pipsa_relative_freq(grades), "relative freq:")

barplot(pipsa_relative_freq(grades))

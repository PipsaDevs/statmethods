source("R/chapter5_formulas.R")

df <- 15

probability <- pt(2.95, df) - pt(-2.95, df)

cat("the probability p(-2.95 <= t <= 2.95) is: ", probability, "\n")

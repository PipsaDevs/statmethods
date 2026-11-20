# EXAMPLE FROM THE BOARD

# degrees of freedom
df <- 15

probability <- pt(2.95, df) - pt(-2.95, df)

cat("the probability p(-2.95 <= t <= 2.95) is: ", probability, "\n")

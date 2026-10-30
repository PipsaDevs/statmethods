# Let X ∼ B(10, 0.5) be the variable that measures the number of heads
# in the toss of 10 coins. Calculate the following:
# The probability of getting 4 heads Sol.: 0.2051
# The probability of getting two or fewer heads Sol.: 0.0547
# And the expected number of heads Sol: 5 heads

dbinom(4, 10, 0.5)
pbinom(2, 10, 0.5)
n <- 10
p <- 0.5
n * p

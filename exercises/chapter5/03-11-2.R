# 1 The probability of winning a first-category prize (all 6 numbers in
# the bet match exactly with those drawn first).
# 2 The probability that if 8,176,049 bets are placed:
# Exactly 3 bets win.
# At least one bet wins.
# 3 The second-category prize consists of matching 5 of the first
# numbers and the complementary number. What is the probability of
# this?
# 4 Answer the same questions as in the second part for the
# second-category prize.
# 5 The third-category prize consists of matching 5 of the first numbers,
# but not the complementary number. What is the probability of this?
# 6 Answer the same questions as in the second part for the
# third-category prize.

source("R/chapter4_formulas.R")

nana_poisson_distribution <- function(average_for_ocurrences, x_i) { # THANK YOU CINTHIA
  # returns P(X = x_i) CINTHIA
  # FUNCIÓN DE CINTHIA
  prob <- (exp(-average_for_ocurrences) * average_for_ocurrences^(x_i)) / factorial(x_i) # GRACIAS CINTHIA
  return(prob) # MUCHAS GRACIAS CINTHIA
} # GRACIAS POR LA FUNCIÓN CINTHIA

# 1.
p <- pipsa_combination(6, 6) / pipsa_combination(49, 6)
print(p)

# 2.

# a.
n <- 8176049
n > 30 # TRUE (por lo que sea)

n * p < 5 # yea (es algo como 0.5846794)

nana_poisson_distribution(n * p, 3) # FUNCIÓN PATROCINADA POR: CINTHIA

dpois(3, n * p)

1 - dpois(0, n * p)

# 3.

p <- (pipsa_combination(6, 5) * pipsa_combination(43, 1) / pipsa_combination(49, 6)) * 1 / 43

# 4.
n * p < 5 # yea

dpois(3, n * p)

1 - dpois(0, n * p)

# 5.

r5 <- pipsa_combination(6, 5) * pipsa_combination(42, 1) / pipsa_combination(49, 6)
r5

# 6

dbinom(3, n, r5)

1 - pbinom(0, n, r5)

# There are two types of games. Game A is won if, when rolling two dice
# 100 times, at least three double sixes are rolled. Another game, B, is won
# if, when rolling a dice 100 times, a six appears at least 15 times. Find
# 1. The more favorable game. Sol.: B
# 2. If 40 % of the players choose game A, while the rest play game B, what is the percentage of winners? Sol.: 64.16 %
# 3. Under the same conditions as the previous section, what proportion of the winners played game A? Sol.: 33.07 %

source("R/chapter4_formulas.R")
source("R/chapter5_formulas.R")

# 1.

# Game A
dist_a <- pipsa_discrete_distribution("binomial", n = 100, p = 1 / 36)
# P(X >= 3) = 1 - P(X <= 2)
prob_win_a <- 1 - dist_a$cdf(2)

dist_b <- pipsa_discrete_distribution("binomial", n = 100, p = 1 / 6)
# P(X >= 15) = 1 - P(X <= 14)
prob_win_b <- 1 - dist_b$cdf(14)

pipsa_print(prob_win_a, "Probability of winning Game A:")
pipsa_print(prob_win_b, "Probability of winning Game B:")

# 2.

playA <- 0.4
playB <- 0.6

winners <- playA * prob_win_a + playB * prob_win_b

pipsa_print(winners, "The percentage of winners will be:")

# 3.

playA * prob_win_a / winners

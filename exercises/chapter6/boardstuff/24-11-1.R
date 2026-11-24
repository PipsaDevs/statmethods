# In a survey of 500 voters from neighbourhood A and 300 from
# neighbourhood B, a candidate was preferred by 43 % of those in A and
# 42 % of those in B. At the α = 5 % level.
#
# Find confidence intervals for the expected results in A and B.

pA <- 43 / 100
nA <- 500

e <- qnorm(.975) * sqrt(pA * (1 - pA) / nA)
c(pA - e, pA + e)

pB <- 42 / 100
nB <- 300

e <- qnorm(.975) * sqrt(pB * (1 - pB) / nB)
c(pB - e, pB + e)

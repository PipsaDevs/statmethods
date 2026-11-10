# A telephone exchange can establish 450 connections per minute. The
# number of connections requested per minute follows a Poisson
# distribution with λ = 400. What is the probability that, in a given
# minute, the exchange is fully occupied?.
#
# Solve it in R

cpm <- 450
lambda <- 400

sqrt(lambda)

1 - pnorm(cpm - 0.5, lambda, sqrt(lambda))

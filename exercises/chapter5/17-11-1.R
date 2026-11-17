library(statmethods)

# a)
treated <- pipsa_discrete_distribution("binomial", n = 200, p = 0.01)

print(1 - treated$cdf(3))

# b)
untreated <- pipsa_discrete_distribution("binomial", n = 200, p = 0.1)
1 - untreated$cdf(5)

x <- c(
  2.2, 2.66, 2.74, 3.41, 2.46, 2.96, 3.34,
  2.16, 2.46, 2.71, 2.04, 3.74, 3.24, 3.92, 2.38,
  2.82, 2.2, 2.42, 2.82, 2.84, 4.22, 3.64, 1.77,
  3.44, 1.53
)
# Suppose this sample was drawn from a normal population.
# We hypothesize that the population mean is not 2.

t.test(x, mu = 2, alternative = "two.sided", conf.level = 0.95)

# Suppose a bus arrives at a stop every 15 minutes. If a person can arrive
# at the stop at any moment, what is the probability that they will wait
# between 5 and 10 minutes?

source("R/chapter5_formulas.R")

fn <- function(x) {
  1 / 15
}

print(pipsa_continuous_density_fn(fn, 5, 10))

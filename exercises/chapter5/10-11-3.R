# Page 76 in the slides
source("R/chapter4_formulas.R")
source("R/chapter5_formulas.R")
c <- 1
func_x <- function(x) {
  if (0 <= x && x <= 2) {
    return(c * x * x)
  } else {
    return(0)
  }
}

# A
c <- 1 / integrate(Vectorize(func_x), lower = 0, upper = 2)$value

# B
dist_func_x <- function(x) {
  if (x < 0) {
    return(0)
  } else if (x > 1) {
    return(1)
  } else {
    return(x^3 / 8)
  }
}

# C.
prob_1_to_2 <- pipsa_continuous_density_fn(func_x, a = 1, b = 2)

# D.

pipsa_continuous_popstats(func_x, 0, 2)$expectedval
pipsa_continuous_popstats(func_x, 0, 2)$variance

# shamelessly copied from daniel lopez tena. thx :D

library(dplyr) # For case_when

# Required for calculating the median
dist_median <- function(F, min_val, max_val, tol = 1e-10) {
  if (min_val >= max_val) {
    stop("min_val must be smaller than max_val")
  }

  mid <- (min_val + max_val) / 2
  if (max_val - min_val < tol) {
    return(mid)
  }

  val <- F(mid)

  if (val <= 0.5) {
    return(dist_median(F, mid, max_val, tol))
  } else {
    return(dist_median(F, min_val, mid, tol))
  }
}

# We need to calculate manually the value of k: k = 1
F <- function(x) {
  case_when(
    x < 0 ~ 0,
    x <= 1 ~ x * (2 - x),
    x > 1 ~ 1
  )
}

# We also calculate f = dF/dx manually
f <- function(x) {
  case_when(
    x < 0 ~ 0,
    x <= 1 ~ 2 - 2 * x,
    x > 1 ~ 0
  )
}

# Mean
x_f <- function(x) x * f(x)
mu <- integrate(x_f, -Inf, Inf)$value
mu

# Median
dist_median(F, 0, 1)

# Variance
x2_f <- function(x) x^2 * f(x)
integrate(x2_f, -Inf, Inf)$value - mu^2

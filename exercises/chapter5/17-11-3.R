library(dplyr)

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

a <- 5 / 3

f <- function(x) {
  case_when(
    x < 0 ~ 0,
    x <= 1 ~ x^2,
    x <= a ~ 1,
    x > 0 ~ 0
  )
}

F <- function(x) {
  case_when(
    x < 0 ~ 0,
    x <= 1 ~ x^3 / 3,
    x <= a ~ x - 2 / 3,
    x > a ~ 0
  )
}

x_f <- function(x) x * f(x)
mu <- integrate(x_f, -Inf, Inf)$value
mu

dist_median(F, 0, a)

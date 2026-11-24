library(dplyr)

source("R/chapter5_formulas.R")

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

a <- 4 / 3

f <- function(x) {
  case_when(
    x < -1 ~ 0,
    x <= 0 ~ a * (x + 1),
    x <= 1 ~ (x - 1) * (x - 1),
    x > 1 ~ 0
  )
}

F <- function(x) {
  case_when(
    x < -1 ~ 0,
    x <= 0 ~ a * (x + 1),
    x <= 1 ~ (x - 1) * (x - 1),
    x > 1 ~ 0
  )
}

print(F)

pipsa_continuous_popstats(f)

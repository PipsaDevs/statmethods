func <- function(x) {
  if (x < 0) {
    return(0)
  } else if (0 <= x && x <= 2) {
    return(0.1 * x^2)
  } else if (2 < x && x <= 3) {
    return(0.2 * x^2 - 0.4 * x + 0.4)
  } else {
    return(1)
  }
}

# We need to find the function for 0.5

sol <- 0.5 + func(0.75) - 0.4

polyroot(c(-sol, -0.4, 0.2))[2]

# DISCRETE FORMULAS ONLY

#' Generate probability distribution map
#'
#' @param x A vector of points x_i marked as references
#' @param p The corresponding probabilities to x_i
#' @return A data frame containing the variables
#' @export
pipsa_gen_prob_dist <- function(x, p) {
  ord <- order(x)
  data.frame(
    x = x[ord],
    p = p[ord]
  )
}

#' Discrete distribution function generator
#'
#' Generates, starting from the vector of points and their
#' probabilities, a function F(x_i) = P(X \leq x_i)
#'
#' @param dat The data frame containing the variables x and p
#' @return The function F(x_i) defined above
#' @examples
#' x <- 0:2
#' p <- c(1, 2, 1) / 4
#' F <- pipsa_discrete_dist_fn_gen(pipsa_gen_prob_dist(x, p))
#' F(1) # 0.75
#' @export
pipsa_discrete_dist_fn_gen <- function(dat) {
  cump <- c(0, cumsum(dat$p))
  function(q) {
    cump[findInterval(q, dat$x) + 1]
  }
}

#' Discrete population statistics
#'
#' Given a probability distribution map, generates a frame
#' with the population statistics values
#'
#' @param dat The data frame containing the variables x and p
#' @return A data frame containing the following values:
#' Mean AKA expected variance
#' Variance
#' Standard deviation
#' @examples
#' x <- 0:2
#' p <- c(1, 2, 1) / 4
#' popstats <- pipsa_discrete_popstats(pipsa_gen_prob_dist(x, p))
#' popstats$expectedval # 1
#' popstats$variance # 0.5
#' @export
pipsa_discrete_popstats <- function(dat) {
  f1 <- sum(dat$p * dat$x)
  f2 <- sum(dat$p * ((dat$x)^2)) - (f1^2)
  data.frame(
    expectedval = f1,
    variance = f2,
    std_dev = abs(sqrt(f2))
  )
}

# CONTINUOUS FORMULAS ONLY

# Note:
# To define functions, just use the standard definition
# of functions in R:
# f <- function(x) x^3 + 2x

#' Density function
#'
#' Given a function f, return the probability that the variable
#' takes a value within the interval specified
#'
#' @param f A continuous function
#' @param a Lower limit
#' @param b Upper limit
#' @return The value of the integral from a to b of f
#' @export

pipsa_continuous_density_fn <- function(f, a, b) {
  f_vec <- Vectorize(f)
  integrate(f_vec, a, b)$value
}

#' Continuous population statistics
#'
#' Given a probability function, generates a frame
#' with the population statistics values
#'
#' @param f The continuous probability function
#' @return A data frame containing the following values:
#' Mean AKA expected variance
#' Variance
#' Standard deviation
#' @examples
#'
#' popstats <- pipsa_continuous_popstats(function(x) {
#'   x^2
#' })
#' popstats$expectedval
#' popstats$variance
#' @export

pipsa_continuous_popstats <- function(f) {
  f1 <- function(x) x * f(x)
  f2 <- function(x) (x^2) * f(x)
  r1 <- integrate(f1, -Inf, Inf)$value
  r2 <- integrate(f2, -Inf, Inf) - (r1^2)

  data.frame(
    expectedval = r1,
    variance = r2,
    std_dev = abs(sqrt(r2))
  )
}

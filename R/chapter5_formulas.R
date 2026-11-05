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

#' Distribution function generator
#'
#' Generates, starting from the vector of points and their
#' probabilities, a function F(x_i) = P(X \leq x_i)
#'
#' @param dat The data frame containing the variables x and p
#' @return The function F(x_i) defined above
#' @examples
#' x <- 0:2
#' p <- c(1, 2, 1) / 4
#' F <- pipsa
#' @export
pipsa_dist_fn_gen <- function(dat) {
  cump <- c(0, cumsum(dat$p))
  function(q) {
    cump[findInterval(q, dat$x) + 1]
  }
}

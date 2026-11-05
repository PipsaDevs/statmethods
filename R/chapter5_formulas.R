#' Generate probability distribution map
#'
#' @param x A vector of points x_i marked as references
#' @param p The corresponding probabilities to x_i
#' @return A data frame containing the variables
#' @export
pipsa_gen_prob_dist <- function(x, p) {
  data.frame(
    x = x,
    p = p
  )
}

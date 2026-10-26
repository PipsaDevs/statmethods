# ========================================================================
# GENERAL FUNCTIONS (shared by most files)

#' Print
#'
#' Prints a number/series of numbers with the
#' correct rounding to do the exam faster
#'
#' @param x A number or series of numbers we want to print
#' @param string A string to print before the number (default is empty)
#' @param round The number of decimals to round to (default is 5)
#' @return The number printed on the console with correct rounding
#' @examples
#' pipsa_print(3.14159)
#' @export
pipsa_print <- function(x, string = "", round = 5, reverse = FALSE) {
  if (length(x) > 1) {
    print(string)
    print(round(x, round))
  } else {
    if (reverse == TRUE) {
      print(paste(round(x, round), string))
    } else {
      print(paste(string, round(x, round)))
    }
  }
}

# ========================================================================

#' Combinations (n choose k)
#'
#' Computes the number of combinations of x items
#' taken y at a time without repetition: choose(x, y).
#'
#' @param x Total number of items (n).
#' @param y Number of items to choose (k).
#' @return Number of possible combinations.
#' @examples
#' pipsa_combination(5, 2)
#' @export
pipsa_combination <- function(x, y) {
  choose(x, y)
}

#' Conditional Probability
#'
#' Computes the conditional probability between two arrays of numbers
#'
#' @param x An array
#' @param y An array
#' @return The conditional probability in decimal format
#' @examples
#' pipsa_conditional_probability(c(0.1, 0.2, 0.3), c(0.01, 0.02, 0.03))
#' @export
pipsa_conditional_probability <- function(x, y) {
  # both x and y will be an array of probabilities, c(a, b, c, ...)

  d <- 0

  for (i in seq_along(x)) {
    d <- d + x[i] * y[i]
  }

  print(paste(d, "%"))
  d
}

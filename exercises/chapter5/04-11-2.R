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
#' F <- pipsa_dist_fn_gen(pipsa_gen_prob_dist(x, p))
#' F(1) # 0.75
#' @export
pipsa_dist_fn_gen <- function(dat) {
  cump <- c(0, cumsum(dat$p))
  function(q) {
    cump[findInterval(q, dat$x) + 1]
  }
}

#' Population statistics
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
#' popstats <- pipsa_popstats(pipsa_gen_prob_dist(x, p))
#' popstats$expectedval # 1
#' popstats$variance # 0.5
#' @export
pipsa_popstats <- function(dat) {
  f1 <- sum(dat$p * dat$x)
  f2 <- sum(dat$p * ((dat$x)^2)) - (f1^2)
  data.frame(
    expectedval = f1,
    variance = f2,
    std_dev = abs(sqrt(f2))
  )
}

#' Continuous Distribution Function Generator
#'
#' Generates, starting from a density function, the cumulative
#' distribution function F(x) = P(X \leq x) using integration.
#'
#' @param pdf The probability density function (must be vectorized)
#' @param lower The lower bound of the distribution's support
#' @param upper The upper bound of the distribution's support
#' @return The cumulative distribution function F(x)
#' @examples
#' f <- Vectorize(function(x) {
#'   if (0 <= x && x <= 2) x / 2 else 0
#' })
#' F_cdf <- pipsa_continuous_dist_fn_gen(f, 0, 2)
#' F_cdf(1) # 0.25
#' @export
pipsa_continuous_dist_fn_gen <- function(pdf, lower, upper) {
  function(q) {
    sapply(q, function(x) {
      if (x < lower) {
        return(0)
      }
      if (x > upper) {
        return(1)
      }
      integrate(pdf, lower, x)$value
    })
  }
}

#' Continuous Population Statistics
#'
#' Given a probability density function and its support, generates a data frame
#' with the population statistics values using integration.
#'
#' @param pdf The probability density function (must be vectorized)
#' @param lower Lower bound of the support
#' @param upper Upper bound of the support
#' @return A data frame containing expectedval, variance, and std_dev
#' @export
pipsa_continuous_popstats <- function(pdf, lower, upper) {
  ex_func <- function(x) x * pdf(x)
  f1 <- integrate(ex_func, lower, upper)$value

  ex2_func <- function(x) (x^2) * pdf(x)
  ex2 <- integrate(ex2_func, lower, upper)$value

  f2 <- ex2 - (f1^2)

  data.frame(
    expectedval = f1,
    variance = f2,
    std_dev = sqrt(f2)
  )
}

#' Continuous Quantiles
#'
#' Finds the x value for a given probability p in a continuous distribution.
#'
#' @param cdf The cumulative distribution function
#' @param p The target probability (e.g., 0.5 for median)
#' @param lower Lower bound of the support
#' @param upper Upper bound of the support
#' @return The quantile x where F(x) = p
#' @export
pipsa_continuous_quantile <- function(cdf, p, lower, upper) {
  uniroot(function(x) cdf(x) - p, lower = lower, upper = upper)$root
}
#' Continuous Probability Interval
#'
#' Computes P(a \leq X \leq b) using a continuous CDF.
#' For P(X \leq b), set a to -Inf. For P(X \geq a), set b to Inf.
#'
#' @param cdf The cumulative distribution function
#' @param a Lower bound of the interval
#' @param b Upper bound of the interval
#' @return The computed probability
#' @export
pipsa_continuous_prob <- function(cdf, a, b) {
  val_b <- if (b == Inf) 1 else cdf(b)
  val_a <- if (a == -Inf) 0 else cdf(a)
  val_b - val_a
}

#' Continuous Conditional Probability (Less or Equal)
#'
#' Computes P(X \leq a | X \leq b).
#'
#' @param cdf The cumulative distribution function
#' @param a The event boundary P(X \leq a)
#' @param b The given condition boundary P(X \leq b)
#' @return The conditional probability
#' @export
pipsa_continuous_cond_prob <- function(cdf, a, b) {
  p_intersect <- cdf(min(a, b))
  p_condition <- cdf(b)

  if (p_condition == 0) {
    return(NA)
  } # Prevent division by zero
  p_intersect / p_condition
}
# ==== END OF PASTED FUNCTIONS ====

func <- function(x) {
  if (0 <= x && x <= 2) {
    return(x / 2)
  } else {
    return(0)
  }
}

func_v <- Vectorize(func)

curve(func_v,
  from = -1, to = 3,
  main = "a) Plot of f(x)",
  xlab = "x",
  ylab = "f(x)",
  col = "blue",
  lwd = 2
)

F_cdf <- pipsa_continuous_dist_fn_gen(func_v, lower = 0, upper = 2)

curve(F_cdf(x),
  from = -0.5, to = 2.5, col = "red", lwd = 2,
  main = "b) Distribution Function F(x)", ylab = "F(x)", xlab = "x"
)
grid()

stats <- pipsa_continuous_popstats(func_v, lower = 0, upper = 2)

pipsa_print(stats$expectedval, "Expected Value E(X):") # 4/3
pipsa_print(stats$variance, "Variance V(X):")
pipsa_print(stats$std_dev, "Standard Deviation:") # sqrt(2)/3

median_x <- pipsa_continuous_quantile(F_cdf, p = 0.50, lower = 0, upper = 2)

q1_x <- pipsa_continuous_quantile(F_cdf, p = 0.25, lower = 0, upper = 2)
q3_x <- pipsa_continuous_quantile(F_cdf, p = 0.75, lower = 0, upper = 2)
iqr_x <- q3_x - q1_x

mode_x <- optimize(func_v, interval = c(0, 2), maximum = TRUE)$maximum

pipsa_print(median_x, "Median (Me):") # sqrt 2 (1.4142)
pipsa_print(mode_x, "Mode (Mo):") # 2
pipsa_print(iqr_x, "Interquartile Range (Rc):") # sqrt(3)-1 (0.7320)

# 1. P(0.5 <= X <= 1.5)
prob_1 <- pipsa_continuous_prob(F_cdf, a = 0.5, b = 1.5)
pipsa_print(prob_1, "P(0.5 <= X <= 1.5):", round = 4)
# SOL: 1/2 = 0.5

# 2. P(X <= 0.5)
# Equivalent to P(-Inf <= X <= 0.5)
prob_2 <- pipsa_continuous_prob(F_cdf, a = -Inf, b = 0.5)
pipsa_print(prob_2, "P(X <= 0.5):", round = 4)
# SOL: 1/16 = 0.0625

# 3. P(X >= 1.5)
# Equivalent to P(1.5 <= X <= Inf)
prob_3 <- pipsa_continuous_prob(F_cdf, a = 1.5, b = Inf)
pipsa_print(prob_3, "P(X >= 1.5):", round = 4)
# SOL: 7/16 = 0.4375

# 4. P(X > 5)
# Equivalent to P(5 <= X <= Inf) for continuous distributions
prob_4 <- pipsa_continuous_prob(F_cdf, a = 5, b = Inf)
pipsa_print(prob_4, "P(X > 5):", round = 4)
# SOL: 0

# 5. P(X <= 1 | X <= Me)
prob_5 <- pipsa_continuous_cond_prob(F_cdf, a = 1, b = median_x)
pipsa_print(prob_5, "P(X <= 1 | X <= Me):", round = 4)
# SOL: 1/2 = 0.5

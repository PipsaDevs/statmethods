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
#' Given a probability distribution map, generates a single-row frame
#' with the complete set of population statistics: central tendency,
#' dispersion, position and shape measures.
#'
#' Quantiles are defined through the accumulated distribution F(x):
#' the median, q1 and q3 are the first x such that F(x) is at least
#' 0.5, 0.25 and 0.75 respectively. For the median only, when F(x)
#' is exactly 0.5 the mid-point between that x and the next x with
#' positive probability is returned. Comparisons use a small numerical
#' tolerance (1e-9) so that rounding errors in the accumulated sums
#' do not break ties.
#'
#' The probabilities are used as given (they are not renormalised).
#' A warning is issued if they do not add up to 1 (tolerance 1e-6).
#'
#' @param dat The data frame containing the variables x and p
#' @return A single-row data frame containing the following values:
#' \itemize{
#'   \item \code{expectedval}: Mean AKA expected value, E[X]
#'   \item \code{variance}: Variance, Var(X)
#'   \item \code{std_dev}: Standard deviation
#'   \item \code{cv}: Coefficient of variation, std_dev / |E[X]|
#'     (\code{NA} if E[X] is zero)
#'   \item \code{median}: Median
#'   \item \code{mode}: Value(s) of x with maximum probability. If
#'     there are several, they are returned collapsed in a single
#'     string (e.g. \code{"2, 3"})
#'   \item \code{q1}: First quartile
#'   \item \code{q3}: Third quartile
#'   \item \code{iqr}: Interquartile range, q3 - q1
#'   \item \code{skewness}: Fisher's skewness, g1 = m3 / std_dev^3
#'     (\code{NA} if the variance is zero)
#'   \item \code{kurtosis}: Excess kurtosis, g2 = m4 / std_dev^4 - 3
#'     (\code{NA} if the variance is zero)
#' }
#' @examples
#' x <- 0:2
#' p <- c(1, 2, 1) / 4
#' popstats <- pipsa_discrete_popstats(pipsa_gen_prob_dist(x, p))
#' popstats$expectedval # 1
#' popstats$variance # 0.5
#' popstats$median # 1
#' popstats$skewness # 0
#'
#' # Several modes are collapsed in a string
#' popstats2 <- pipsa_discrete_popstats(pipsa_gen_prob_dist(1:4, rep(0.25, 4)))
#' popstats2$mode # "1, 2, 3, 4"
#' popstats2$median # 2.5
#' @export
pipsa_discrete_popstats <- function(dat) {
  if (!is.data.frame(dat) || !all(c("x", "p") %in% names(dat))) {
    stop("'dat' must be a data frame with columns 'x' and 'p'")
  }
  if (nrow(dat) == 0 || anyNA(dat$x) || anyNA(dat$p) || any(dat$p < 0)) {
    stop("'dat' must be non-empty, without NA, and with p >= 0")
  }
  if (abs(sum(dat$p) - 1) > 1e-6) {
    warning("the probabilities in 'dat' do not add up to 1")
  }

  tol <- 1e-9
  dat <- dat[order(dat$x), ]
  n <- nrow(dat)
  cump <- cumsum(dat$p)

  # Expected value, variance and central moments
  f1 <- sum(dat$x * dat$p)
  dev <- dat$x - f1
  f2 <- sum(dev^2 * dat$p)
  sd_val <- sqrt(f2)
  m3 <- sum(dev^3 * dat$p)
  m4 <- sum(dev^4 * dat$p)

  # Coefficient of variation (mean is zero relative to the support scale)
  cv_val <- if (abs(f1) <= tol * max(abs(dat$x))) NA_real_ else sd_val / abs(f1)

  # Quantiles: first x with accumulated F(x) >= prob
  first_ge <- function(prob) which(cump >= prob - tol)[1]
  quant <- function(prob) {
    i <- first_ge(prob)
    if (is.na(i)) NA_real_ else dat$x[i]
  }

  # Median: mid-point average when F(x) is exactly 0.5
  i_med <- first_ge(0.5)
  if (is.na(i_med)) {
    med_val <- NA_real_
  } else if (abs(cump[i_med] - 0.5) <= tol) {
    nxt <- which(dat$p > tol & seq_len(n) > i_med)[1]
    med_val <- if (is.na(nxt)) dat$x[i_med] else (dat$x[i_med] + dat$x[nxt]) / 2
  } else {
    med_val <- dat$x[i_med]
  }

  # Mode: all x sharing the maximum probability
  p_max <- max(dat$p)
  modes <- dat$x[dat$p >= p_max * (1 - tol)]
  mode_val <- if (length(modes) == 1) modes else paste(modes, collapse = ", ")

  q1_val <- quant(0.25)
  q3_val <- quant(0.75)

  data.frame(
    expectedval = f1,
    variance = f2,
    std_dev = sd_val,
    cv = cv_val,
    median = med_val,
    mode = mode_val,
    q1 = q1_val,
    q3 = q3_val,
    iqr = q3_val - q1_val,
    skewness = if (f2 > 0) m3 / sd_val^3 else NA_real_,
    kurtosis = if (f2 > 0) m4 / sd_val^4 - 3 else NA_real_
  )
}

#' Discrete uniform probability distribution map
#'
#' Generates the probability distribution map of a discrete uniform
#' variable, where each of the k points of the support has probability
#' p_i = 1 / k. The support can be given either as the range of
#' integers a:b or as an explicit vector of values.
#'
#' @param a Lower limit of the range (if \code{b} is given) or a vector
#' with the (distinct) values of the support (if \code{b} is \code{NULL})
#' @param b Upper limit of the range (default is \code{NULL})
#' @return A data frame containing the variables x and p
#' @examples
#' pmf <- pipsa_pmf_uniform(1, 6) # fair die
#' pmf$p # 1/6 for each face
#'
#' # Explicit vector of values
#' pmf2 <- pipsa_pmf_uniform(c(2, 4, 8))
#' pmf2$p # 1/3 for each value
#' @export
pipsa_pmf_uniform <- function(a, b = NULL) {
  if (!is.numeric(a) || length(a) == 0 || !all(is.finite(a))) {
    stop("'a' must be a non-empty vector of finite numbers")
  }
  if (is.null(b)) {
    if (anyDuplicated(a)) stop("'a' must not contain repeated values")
    x <- a
  } else {
    if (length(a) != 1 || length(b) != 1 || !is.numeric(b) || !is.finite(b)) {
      stop("'a' and 'b' must be single finite numbers when a range is used")
    }
    if (a != round(a) || b != round(b)) stop("'a' and 'b' must be integers")
    if (a > b) stop("'a' must not be greater than 'b'")
    x <- a:b
  }
  k <- length(x)
  pipsa_gen_prob_dist(x, rep(1 / k, k))
}

#' Bernoulli probability distribution map
#'
#' Generates the probability distribution map of a Bernoulli variable,
#' with support x in {0, 1}, P(X = 0) = 1 - p and P(X = 1) = p.
#'
#' @param p Probability of success, between 0 and 1
#' @return A data frame containing the variables x and p
#' @examples
#' pmf <- pipsa_pmf_bernoulli(0.3)
#' pmf$p # 0.7 0.3
#' @export
pipsa_pmf_bernoulli <- function(p) {
  if (!is.numeric(p) || length(p) != 1 || is.na(p) || p < 0 || p > 1) {
    stop("'p' must be a single number between 0 and 1")
  }
  x <- 0:1
  prob <- c(1 - p, p)
  pipsa_gen_prob_dist(x, prob)
}

#' Binomial probability distribution map
#'
#' Generates the probability distribution map of a binomial variable,
#' with support x in {0, 1, ..., n}.
#'
#' @param n Number of trials (positive integer)
#' @param p Probability of success in each trial, between 0 and 1
#' @return A data frame containing the variables x and p
#' @examples
#' pmf <- pipsa_pmf_binomial(2, 0.5)
#' pmf$p # 0.25 0.50 0.25
#' @export
pipsa_pmf_binomial <- function(n, p) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 || n != round(n)) {
    stop("'n' must be a single positive integer")
  }
  if (!is.numeric(p) || length(p) != 1 || is.na(p) || p < 0 || p > 1) {
    stop("'p' must be a single number between 0 and 1")
  }
  x <- 0:n
  prob <- dbinom(x, n, p)
  pipsa_gen_prob_dist(x, prob)
}

#' Geometric probability distribution map
#'
#' Generates the probability distribution map of a geometric variable
#' following the UMA convention: X is the number of trials until the
#' first success, so the support is k in {1, 2, ...} and
#' P(X = k) = (1 - p)^(k - 1) p. The (infinite) support is truncated
#' at \code{max_k}.
#'
#' If \code{max_k} is \code{NULL}, it is chosen as the smallest k such
#' that P(X > k) < 1e-7. The probabilities are not renormalised, so
#' they add up to 1 minus the neglected tail.
#'
#' @param p Probability of success in each trial, in (0, 1]
#' @param max_k Maximum value of the support (default is \code{NULL},
#' meaning automatic truncation)
#' @return A data frame containing the variables x and p
#' @examples
#' pmf <- pipsa_pmf_geometric(0.5, max_k = 3)
#' pmf$p # 0.500 0.250 0.125
#'
#' # Automatic truncation
#' pmf2 <- pipsa_pmf_geometric(0.5)
#' max(pmf2$x) # 24
#' @export
pipsa_pmf_geometric <- function(p, max_k = NULL) {
  if (!is.numeric(p) || length(p) != 1 || is.na(p) || p <= 0 || p > 1) {
    stop("'p' must be a single number in (0, 1]")
  }
  if (is.null(max_k)) {
    # Smallest k with P(X > k) = (1 - p)^k < 1e-7
    max_k <- floor(log(1e-7) / log1p(-p)) + 1
    if (max_k > 1e6) {
      stop("automatic truncation needs more than 1e6 points; supply 'max_k'")
    }
  } else if (!is.numeric(max_k) ||
    length(max_k) != 1 ||
    is.na(max_k) ||
    max_k < 1 ||
    max_k != round(max_k)
  ) {
    stop("'max_k' must be a single positive integer")
  }
  x <- seq_len(max_k)
  prob <- dgeom(x - 1, p)
  pipsa_gen_prob_dist(x, prob)
}

#' Poisson probability distribution map
#'
#' Generates the probability distribution map of a Poisson variable,
#' with support x in {0, 1, ...}. The (infinite) support is truncated
#' at \code{max_k}.
#'
#' If \code{max_k} is \code{NULL}, it is chosen as the smallest k such
#' that P(X > k) < 1e-7. The probabilities are not renormalised, so
#' they add up to 1 minus the neglected tail.
#'
#' @param lambda Mean rate of the distribution (positive)
#' @param max_k Maximum value of the support (default is \code{NULL},
#' meaning automatic truncation)
#' @return A data frame containing the variables x and p
#' @examples
#' pmf <- pipsa_pmf_poisson(2, max_k = 4)
#' round(pmf$p, 4) # 0.1353 0.2707 0.2707 0.1804 0.0902
#'
#' # Automatic truncation
#' pmf2 <- pipsa_pmf_poisson(3)
#' sum(pmf2$p) # approximately 1
#' @export
pipsa_pmf_poisson <- function(lambda, max_k = NULL) {
  if (!is.numeric(lambda) || length(lambda) != 1 || is.na(lambda) ||
    !is.finite(lambda) || lambda <= 0) {
    stop("'lambda' must be a single positive number")
  }
  if (is.null(max_k)) {
    # Smallest k with P(X > k) < 1e-7
    max_k <- qpois(1e-7, lambda, lower.tail = FALSE)
    if (ppois(max_k, lambda, lower.tail = FALSE) >= 1e-7) max_k <- max_k + 1
    if (max_k > 1e6) {
      stop("automatic truncation needs more than 1e6 points; supply 'max_k'")
    }
  } else if (!is.numeric(max_k) || length(max_k) != 1 || is.na(max_k) ||
    max_k < 0 || max_k != round(max_k)) {
    stop("'max_k' must be a single non-negative integer")
  }
  x <- 0:max_k
  prob <- dpois(x, lambda)
  pipsa_gen_prob_dist(x, prob)
}

#' All-in-one discrete distribution
#'
#' Given the name of a discrete distribution and its parameters,
#' generates in a single call its probability distribution map, its
#' distribution function, its population statistics and the exact
#' theoretical values of the expected value and the variance.
#'
#' The parameters in \code{...} are those of the corresponding
#' \code{pipsa_pmf_*} function:
#' \itemize{
#'   \item \code{"uniform"}: \code{a, b = NULL}
#'   \item \code{"bernoulli"}: \code{p}
#'   \item \code{"binomial"}: \code{n, p}
#'   \item \code{"geometric"}: \code{p, max_k = NULL} (UMA convention,
#'     number of trials until the first success)
#'   \item \code{"poisson"}: \code{lambda, max_k = NULL}
#' }
#'
#' @param type The distribution: "uniform", "bernoulli", "binomial",
#' "geometric" or "poisson"
#' @param ... The parameters of the distribution (see Details)
#' @return A list containing the following values:
#' \itemize{
#'   \item \code{type}: The name of the distribution
#'   \item \code{pmf}: The probability distribution map, a data frame
#'     generated by the corresponding \code{pipsa_pmf_*} function
#'   \item \code{cdf}: The function F(x_i) = P(X <= x_i), generated by
#'     \code{pipsa_discrete_dist_fn_gen}
#'   \item \code{metrics}: The population statistics, a data frame
#'     generated by \code{pipsa_discrete_popstats}
#'   \item \code{theoretical}: A list with the exact values
#'     \code{expectedval} and \code{variance}, computed from the
#'     formulas of the distribution (no truncation involved)
#' }
#' @examples
#' dist <- pipsa_discrete_distribution("binomial", n = 10, p = 0.3)
#' dist$metrics$expectedval # 3
#' dist$theoretical$expectedval # 3
#' dist$theoretical$variance # 2.1
#' dist$cdf(3) # 0.6496107
#'
#' dist2 <- pipsa_discrete_distribution("uniform", 1, 6)
#' dist2$theoretical$variance # 2.916667
#'
#' dist3 <- pipsa_discrete_distribution("geometric", p = 0.25)
#' dist3$theoretical$expectedval # 4
#' @export
pipsa_discrete_distribution <- function(type, ...) {
  type <- match.arg(
    type,
    c("uniform", "bernoulli", "binomial", "geometric", "poisson")
  )
  pmf_fn <- switch(type,
    uniform = pipsa_pmf_uniform,
    bernoulli = pipsa_pmf_bernoulli,
    binomial = pipsa_pmf_binomial,
    geometric = pipsa_pmf_geometric,
    poisson = pipsa_pmf_poisson
  )
  pmf <- pmf_fn(...)

  # Parameters matched by name to the formals of the pmf function,
  # so that positional arguments are also supported
  prm <- as.list(match.call(
    pmf_fn,
    as.call(c(list(as.name("pmf_fn")), list(...)))
  ))[-1]

  theoretical <- switch(type,
    uniform = {
      if (is.null(prm$b)) {
        list(
          expectedval = mean(prm$a),
          variance = mean((prm$a - mean(prm$a))^2)
        )
      } else {
        k <- prm$b - prm$a + 1
        list(
          expectedval = (prm$a + prm$b) / 2,
          variance = (k^2 - 1) / 12
        )
      }
    },
    bernoulli = list(
      expectedval = prm$p,
      variance = prm$p * (1 - prm$p)
    ),
    binomial = list(
      expectedval = prm$n * prm$p,
      variance = prm$n * prm$p * (1 - prm$p)
    ),
    geometric = list(
      expectedval = 1 / prm$p,
      variance = (1 - prm$p) / prm$p^2
    ),
    poisson = list(
      expectedval = prm$lambda,
      variance = prm$lambda
    )
  )

  list(
    type = type,
    pmf = pmf,
    cdf = pipsa_discrete_dist_fn_gen(pmf),
    metrics = pipsa_discrete_popstats(pmf),
    theoretical = theoretical
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
#' @param a Lower limit of support (default is -Inf)
#' @param b Upper limit of support (default is Inf)
#' @return A data frame containing the following values:
#' Mean AKA expected variance
#' Variance
#' Standard deviation
#' @examples
#'
#' f <- function(x) ifelse(x >= 0 & x <= 2, x / 2, 0)
#' popstats <- pipsa_continuous_popstats(f)
#' popstats$expectedval
#' popstats$variance
#'
#' # Alternatively, pass limits directly:
#' popstats2 <- pipsa_continuous_popstats(function(x) x / 2, a = 0, b = 2)
#' @export

pipsa_continuous_popstats <- function(f, a = -Inf, b = Inf) {
  f_vec <- Vectorize(f)
  f1 <- function(x) x * f_vec(x)
  f2 <- function(x) (x^2) * f_vec(x)
  r1 <- integrate(f1, a, b)$value
  r2 <- integrate(f2, a, b)$value - (r1^2)

  data.frame(
    expectedval = r1,
    variance = r2,
    std_dev = abs(sqrt(r2))
  )
}

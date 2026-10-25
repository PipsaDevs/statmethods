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
pipsa_print <- function(x, string = "", round = 5) {
  print(paste(string, round(x, round)))
}

# ========================================================================


#' Quantile
#'
#' Calculates the quantile of a numeric vector
#'
#' @param x The vector with raw numeric values
#' @param y The desired quantile, either in percentage or in decimal format
#' @return The quantile y of x
#' @examples
#' pipsa_quantile(c(1, 1, 2, 5, -54, 9, 67), 0.25)
#' @export
pipsa_quantile <- function(x, y) {
  if (y > 1) {
    y <- y / 100
  }

  quantile(x, y, type = 2)
}

#' Percentile
#'
#' Alias function of the quantile function
pipsa_percentile <- function(x, y) {
  # alias function
  pipsa_quantile(x, y)
}

#' Quartile
#'
#' Calculates the quartile of a numeric vector
#'
#' @param x The vector with raw numeric values
#' @param y The desired quartile in 1:4
#' @return The quartile y of x
#' @examples
#' pipsa_quartile(c(1, 1, 2, 5, -54, 9, 67), 1)
#' @export
pipsa_quartile <- function(x, y) {
  if (!y %in% 1:4) {
    print("Invalid y value")
    stop()
  }
  y <- y / 4
  pipsa_quantile(x, y)
}

#' Decile
#'
#' Calculates the decile of a numeric vector
#'
#' @param x The vector with raw numeric values
#' @param y The desired decile
#' @return The decile y of x
#' @examples
#' pipsa_decile(c(1, 1, 2, 5, -54, 9, 67), 4)
#' @export
pipsa_decile <- function(x, y) {
  pipsa_quantile(x, y / 10)
}

#' Absolute frequency
#'
#' Computes the absolute frequency table of a vector with straight raw values
#'
#' @param x A vector of values.
#' @return A frequency table (`table` object) with counts of each unique value.
#' @examples
#' res <- pipsa_absolute_freq(c(1, 2, 2, 3, 3, 3, 4))
#' res == c(1, 2, 3, 1) # TRUE
#' @export
pipsa_absolute_freq <- function(x) table(x)

#' Statistical mode
#'
#' Computes the mode (most frequent value) of a numeric sample.
#'
#' @param x A numeric vector.
#' @return The mode of the vector provided
#' @examples
#' pipsa_stat_mode(c(1, 2, 2, 3, 3, 4))
#' @export
pipsa_stat_mode <- function(x) {
  tbl <- pipsa_absolute_freq(x)
  as.numeric(names(tbl)[which.max(tbl)])
}

#' Interquartile range
#'
#' Computes the Interquartile Range of a given array
#'
#' @param x A vector of values
#' @return The Interquartile Range of said array
#' @examples
#' pipsa_iqr(c(1, 2, 3, 4, 5))
#' @export
pipsa_iqr <- function(x) {
  q <- quantile(x, c(0.25, 0.75), type = 2)
  iqr <- q[2] - q[1]
  iqr
}

#' Absolute frequency of a number
#'
#' Counts how many times a specific value appears in a sample.
#'
#' @param x A vector of values.
#' @param y The specific value to count.
#' @return An integer representing the count of `y` in `x`.
#' @examples
#' pipsa_absolute_freq_of_number(c(1, 2, 2, 3, 4), 2)
#' @export
pipsa_absolute_freq_of_number <- function(x, y) {
  length(which(x == y))
}

#' Relative frequency
#'
#' Computes the relative frequency distribution (f_i = n_i / N) of a sample.
#'
#' @param x A vector of values.
#' @return A frequency table (`table` object) of proportions summing to 1.
#' @examples
#' pipsa_relative_freq(c(1, 2, 2, 3, 4))
#' @export
pipsa_relative_freq <- function(x) prop.table(table(x))

#' Relative frequency of a specific number
#'
#' Computes the relative frequency proportion
#' (f_i = n_i / N) of a specific value.
#'
#' @param x A vector of values.
#' @param y The specific value whose relative frequency is to be found.
#' @return A numeric proportion between 0 and 1.
#' @examples
#' pipsa_relative_freq_of_number(c(1, 2, 2, 3, 4), 2)
#' @export
pipsa_relative_freq_of_number <- function(x, y) {
  table <- prop.table(table(x))
  as.numeric(table)[which(names(table) == y)]
}

#' Cumulative absolute frequency
#'
#' Computes cumulative absolute frequencies (N_i = sum_{j<=i} n_j).
#'
#' @param x A vector of values.
#' @return A cumulative sum vector of counts.
#' @examples
#' pipsa_cum_absolute_freq(c(1, 2, 2, 3, 4))
#' @export
pipsa_cum_absolute_freq <- function(x) cumsum(table(x))

#' Cumulative relative frequency
#'
#' Computes cumulative relative frequencies (F_i = N_i / N).
#'
#' @param x A vector of values.
#' @return A cumulative sum vector of proportions ending at 1.
#' @examples
#' pipsa_cum_relative_freq(c(1, 2, 2, 3, 4))
#' @export
pipsa_cum_relative_freq <- function(x) cumsum(prop.table(table(x)))

# Position measures are all covered by R
# Dispersion measures

#' Mean deviation w.r.t. a point
#'
#' Computes the mean absolute deviation
#' of a sample with respect to a point p
#' (defaults to the mean).
#'
#' @param x A numeric vector.
#' @param p A reference numeric value (defaults to mean(x)).
#' @return The mean absolute deviation.
#' @examples
#' pipsa_mean_deviation(c(1, 2, 3, 4, 5))
#' @export
pipsa_mean_deviation <- function(x, p = mean(x)) {
  mean(abs(x - p))
}

#' Mean Squared Error
#'
#' Computes the mean squared error
#' of a sample with respect to a point p
#' (defaults to the mean).
#'
#' @param x A numeric vector.
#' @param p A reference numeric value (defaults to mean(x)).
#' @return The mean squared error.
#' @examples
#' pipsa_mse(c(1, 2, 3, 4, 5))
#' @export
pipsa_mse <- function(x, p = mean(x)) {
  mean((x - p)^2)
}

#' Population Variance
#'
#' Computes the population variance (divided by N, not quasi-variance N-1).
#'
#' @param x A numeric vector.
#' @return The population variance.
#' @examples
#' pipsa_variance(c(1, 2, 3, 4, 5))
#' @export
pipsa_variance <- function(x) {
  mean((x - mean(x))^2)
}

#' Population Standard Deviation
#'
#' Computes the population standard deviation (sqrt of population variance).
#'
#' @param x A numeric vector.
#' @return The population standard deviation.
#' @examples
#' pipsa_std_deviation(c(1, 2, 3, 4, 5))
#' @export
pipsa_std_deviation <- function(x) {
  sqrt(pipsa_variance(x))
}

#' Standardized variable
#'
#' Transforms data into standardized values: z = (x - mean) / std_dev.
#'
#' @param x A numeric vector.
#' @return A numeric vector of standardized z-scores.
#' @examples
#' pipsa_std_variable(c(10, 20, 30, 40, 50))
#' @export
pipsa_std_variable <- function(x) {
  (x - mean(x)) / pipsa_std_deviation(x)
}

#' Coefficient of variation
#'
#' Computes the Pearson coefficient of variation: cv = std_dev / |mean|.
#'
#' @param x A numeric vector.
#' @return The coefficient of variation.
#' @examples
#' pipsa_cv(c(10, 20, 30, 40, 50))
#' @export
pipsa_cv <- function(x) {
  pipsa_std_deviation(x) / abs(mean(x))
}

# Moments & Shape measures

#' Moment of order r w.r.t. constant c
#'
#' Computes the sample moment of order
#' r with respect to constant c: mean((x - c)^r).
#'
#' @param x A numeric vector.
#' @param r Integer order of the moment.
#' @param c Reference numeric constant.
#' @return The moment value.
#' @examples
#' pipsa_moment(c(1, 2, 3, 4, 5), r = 2, c = 0)
#' @export
pipsa_moment <- function(x, r, c) {
  mean((x - c)^r)
}

#' Ordinary moment
#'
#' Computes the ordinary moment of order r (w.r.t. origin 0): a_r = mean(x^r).
#'
#' @param x A numeric vector.
#' @param r Integer order of the moment.
#' @return The ordinary moment value.
#' @examples
#' pipsa_ord_moment(c(1, 2, 3, 4, 5), r = 2)
#' @export
pipsa_ord_moment <- function(x, r) pipsa_moment(x, r, 0)

#' Central moment
#'
#' Computes the central moment of order r
#'  (w.r.t. the mean): m_r = mean((x - mean)^r).
#'
#' @param x A numeric vector.
#' @param r Integer order of the moment.
#' @return The central moment value.
#' @examples
#' pipsa_cnt_moment(c(1, 2, 3, 4, 5), r = 2)
#' @export
pipsa_cnt_moment <- function(x, r) pipsa_moment(x, r, mean(x))

#' Pearson's skewness coefficient
#'
#' Computes Pearson's mode skewness coefficient: (mean - mode) / std_deviation.
#'
#' @param x A numeric vector.
#' @return The Pearson skewness coefficient.
#' @examples
#' pipsa_p_skewness(c(1, 2, 2, 3, 4))
#' @export
pipsa_p_skewness <- function(x) {
  (
    mean(x) - pipsa_stat_mode(x)
  ) / pipsa_std_deviation(x)
}

#' Fisher's skewness coefficient
#'
#' Computes Fisher's skewness coefficient: m_3 / (std_dev^3).
#'
#' @param x A numeric vector.
#' @return The Fisher skewness coefficient.
#' @examples
#' pipsa_f_skewness(c(1, 2, 3, 4, 5))
#' @export
pipsa_f_skewness <- function(x) {
  pipsa_cnt_moment(x, 3) / (pipsa_std_deviation(x)^3)
}

#' Fisher's kurtosis coefficient
#'
#' Computes Fisher's excess kurtosis: (m_4 / (std_dev^4)) - 3. Mesokurtic = 0.
#'
#' @param x A numeric vector.
#' @return The excess kurtosis value.
#' @examples
#' pipsa_kurtosis(c(1, 2, 3, 4, 5))
#' @export
pipsa_kurtosis <- function(x) {
  g <- pipsa_cnt_moment(x, 4) / (pipsa_std_deviation(x)^4)
  g - 3
}

#' Detect Moderate and Extreme Outliers using Interquartile Range
#'
#' @param x A numeric vector of observations.
#' @return A list containing moderate and extreme outliers.
#' @examples
#' pipsa_detect_outliers(c(1, 2, 3, 4, 5, 100, 200))
#' @export
pipsa_detect_outliers <- function(x) {
  q1 <- pipsa_quantile(x, 0.25)
  q3 <- pipsa_quantile(x, 0.75)
  iqr <- q3 - q1

  mod_lower <- q1 - 1.5 * iqr
  mod_upper <- q3 + 1.5 * iqr
  ext_lower <- q1 - 3.0 * iqr
  ext_upper <- q3 + 3.0 * iqr

  mod_outliers <- x[x < mod_lower | x > mod_upper]
  ext_outliers <- x[x < ext_lower | x > ext_upper]

  list(
    iqr = iqr,
    moderate_bounds = c(lower = mod_lower, upper = mod_upper),
    extreme_bounds = c(lower = ext_lower, upper = ext_upper),
    moderate_outliers = mod_outliers,
    extreme_outliers = ext_outliers
  )
}

#' Root mean square (rms)
#'
#' Computes the root mean square AKA the quadratic mean
#'
#' @param x Raw data vector
#' @return Do i really need to explain this
#' @examples
#' pipsa_rms(c(2, 0, 0, 4, 6, 9, 6, 7, 6, 6, 1))
#' @export

pipsa_rms <- function(x) {
  sqrt(mean(x^2))
}


#' Quantile on continuous case
#'
#' Calculates the quantile on the continuous case
#' NOTE: For the discrete (normal) case, use R's built-in quantile() function
#'
#' @param c A value between 0 and 1
#' @param N Size of the array
#' @param L Lower limit of the interval
#' @param N_prev The value in the cumulative absolute
#' frequencies such that N_prev <= cN
#' @param ni Absolute frequency of the interval
#' @param ai Interval width
#'
#' @export
pipsa_quantile_continuous <- function(c, N, L, N_prev, ni, ai) {
  L + ((c * N - N_prev) / ni) * ai
}

#' Generate vector from data and frequencies
#'
#' Generates a vector with given data and frequencies
#' @param x Vector of values
#' @param n Vector of frequencies
#' @return A vector with each value of x repeating by its respective y frequencies
#' @examples
#' pipsa_generate_vector_with_frequencies(c(1, 2, 3), c(4, 5, 6))
#' @export
pipsa_generate_vector_with_frequencies <- function(x, n) {
  res <- c()

  index <- 1

  for (i in seq_along(n)) {
    for (j in 1:n[i]) {
      res <- c(res, x[i])
      index <- index + 1
    }
  }

  return(res)
}

#' Vector info
#'
#' Prints a lot of useful information from an
#' array to the screen (and also returns info)
#'
#' @param x A numeric vector
#' @return A list with the mean, median, mode,
#' iqr, variance, standard deviation, coefficient of variation,
#' skewness, kurtosis, and outliers
#' @examples
#' pipsa_vector_info(c(1, 2, 3, 4, 5))
#' @export
pipsa_vector_info <- function(x) {
  outliers <- pipsa_detect_outliers(x)

  pipsa_print(mean(x), "Mean: ")
  pipsa_print(median(x), "Median: ")
  pipsa_print(pipsa_stat_mode(x), "Mode: ")
  pipsa_print(pipsa_iqr(x), "IQR: ")
  pipsa_print(pipsa_variance(x), "Variance: ")
  pipsa_print(pipsa_std_deviation(x), "Standard Deviation: ")
  pipsa_print(pipsa_cv(x), "Coefficient of Variation: ")
  pipsa_print(pipsa_f_skewness(x), "Skewness: ")
  pipsa_print(pipsa_kurtosis(x), "Kurtosis: ")

  print("Moderate Outlier Bounds:")
  pipsa_print(outliers$moderate_bounds)

  print("Extreme Outlier Bounds:")
  pipsa_print(outliers$extreme_bounds)

  if (length(outliers$moderate_outliers) > 0) {
    print("Moderate outliers:")
    pipsa_print(outliers$moderate_outliers)
  }

  if (length(outliers$extreme_outliers) > 0) {
    print("Extreme outliers:")
    pipsa_print(outliers$extreme_outliers)
  }

  return(list(
    mean = mean(x),
    median = median(x),
    mode = pipsa_stat_mode(x),
    iqr = pipsa_iqr(x),
    variance = pipsa_variance(x),
    std_deviation = pipsa_std_deviation(x),
    cv = pipsa_cv(x),
    skewness = pipsa_f_skewness(x),
    kurtosis = pipsa_kurtosis(x),
    outliers = outliers
  ))
}

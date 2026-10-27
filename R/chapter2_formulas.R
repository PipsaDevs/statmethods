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

#' Obtain Function from Model Coefficients
#'
#' Creates a string from a linear model's coefficients
#' in the form "intercept + slope * x".
#'
#' @param model A model (obtained from lm())
#' @param y_on_x A boolean to determine whether the model is y on x or x on y. Default: TRUE (meaning it's y on x)
#' @return A string representation of the linear function.
#' @examples
#' pipsa_obtain_function_from_model(lm(c(1, 2, 3) ~ c(4, 5, 6)))
#' @export
pipsa_obtain_function_from_model <- function(model, y_on_x = TRUE) {
  intercept <- round(model$coefficients[1], 5)
  xslope <- round(model$coefficients[2], 5)

  if (xslope >= 0) {
    if (y_on_x == TRUE) {
      print(paste("y =", intercept, "+", xslope, "x"))
    } else {
      print(paste("x =", intercept, "+", xslope, "y"))
    }
  } else {
    if (y_on_x == TRUE) {
      print(paste("y =", intercept, " ", xslope, "x"))
    } else {
      print(paste("x =", intercept, " ", xslope, "y"))
    }
  }
}

#' Obtain regression line from model coefficient
#'
#' Alias function. Creates a string from a linear model's coefficients
#' in the form "intercept + slope * x".
#'
#' @param model A model (obtained from lm())
#' @param y_on_x A boolean to determine whether the model is y on x or x on y. Default: TRUE (meaning it's y on x)
#' @return A string representation of the linear function.
#' @examples
#' pipsa_regression_line_from_model(lm(c(1, 2, 3) ~ c(4, 5, 6)))
#' @export
pipsa_regression_line_from_model <- function(model, y_on_x = TRUE) {
  # alias function
  pipsa_obtain_function_from_model(model, y_on_x)
}

#' Plot function from model
#'
#' Gets a model and plots the function
#'
#' @param model A model (obtained from lm())
#' @param col The color of the line. Default: "red"
#' @param lineWidth The width of the line. Default: 2
#' @param y_on_x A boolean to determine whether the model is y on x or x on y. Default: TRUE (meaning it's y on x)
#' @return The representation of the function in a plot
#' @examples
#' pipsa_plot_from_model(lm(c(1, 2, 3) ~ c(4, 5, 6)), "red", 2)
#' @export
pipsa_plot_from_model <- function(model, col = "red", lineWidth = 2, y_on_x = TRUE) {
  coef <- coef(model)

  if (y_on_x == TRUE) {
    abline(a = coef[1], b = coef[2], col = col, lwd = lineWidth)
  } else {
    abline(a = -coef[1] / coef[2], b = 1 / coef[2], col = col, lwd = lineWidth)
  }
}

#' Plot point
#'
#' Gets coordinates for a point and plots it
#'
#' @param x x coordinate
#' @param y y coordinate
#' @param color The color of the point
#' @param pch The "shape" of the point
#' @param cex The "thickness" of the point
#' @return The point plotted on the plot
#' @examples
#' pipsa_plot_point(2, 3, 19, 1.5)
#' @export
pipsa_plot_point <- function(x, y, color = "black", pch = 19, cex = 1.5) {
  points(x, y, col = color, pch = pch, cex = cex)
}

# ========================================================================

#' Matrix / Vector to Contingency Table
#'
#' Converts a raw vector
#' into a contingency table by row
#' with a specified number of rows.
#'
#' @param x A vector of table cell counts.
#' @param nrows Integer number of rows for the matrix.
#' @return An R `table` object.
#' @examples
#' pipsa_table_from_vector(c(10, 20, 30, 40), 2)
#' @export
pipsa_table_from_vector <- function(x, nrows) {
  as.table(matrix(x, nrow = nrows, byrow = TRUE))
}

#' Two-Variable Contingency Table
#'
#' Creates a 2-way frequency table from two vectors X and Y.
#'
#' @param x A vector for variable X.
#' @param y A vector for variable Y.
#' @return A contingency table.
#' @examples
#' pipsa_table_from_variables(c(1, 1, 2, 2), c("A", "B", "A", "B"))
#' @export
pipsa_table_from_variables <- function(x, y) {
  table(x, y)
}

#' Two-Variable Frequency distribution
#'
#' Creates a frequency table with their totals
#'
#' @param x A vector for variable X.
#' @param y A vector for variable Y.
#' @return A contingency table.
#' @examples
#' pipsa_table_from_variables(c(1, 1, 2, 2), c("A", "B", "A", "B"))
#' @export
pipsa_freq_distribution <- function(x, y) {
  table <- pipsa_table_from_variables(x, y)
  addmargins(table)
}

#' Reconstruct Observation Vectors from Contingency Table
#'
#' Expands a contingency table back into
#' individual observation vectors (x and y).
#'
#' @param tbl A contingency table.
#' @return A data.frame with two columns: `x` and `y`.
#' You can access d$x and d$y for the corresponding vectors
#' @examples
#' tbl <- table(c(1, 1, 2), c(3, 4, 4))
#' pipsa_table_to_vectors(tbl)
#' @export
pipsa_table_to_vectors <- function(tbl) {
  df_freq <- as.data.frame(tbl)

  names_tbl <- names(dimnames(tbl))

  x_vec <- rep(df_freq[[1]], times = df_freq$Freq)
  y_vec <- rep(df_freq[[2]], times = df_freq$Freq)

  if (!any(is.na(suppressWarnings(as.numeric(as.character(x_vec)))))) {
    x_vec <- as.numeric(as.character(x_vec))
  }
  if (!any(is.na(suppressWarnings(as.numeric(as.character(y_vec)))))) {
    y_vec <- as.numeric(as.character(y_vec))
  }

  if (!is.null(names_tbl) && tolower(names_tbl[1]) == "y") {
    data.frame(x = y_vec, y = x_vec)
  } else {
    data.frame(x = x_vec, y = y_vec)
  }
}

#' Marginal Distribution of X
#'
#' Computes the marginal frequency distribution for variable X (rows).
#'
#' @param x A vector for variable X.
#' @param y A vector for variable Y.
#' @return A 1D table of marginal frequencies for X.
#' @examples
#' pipsa_marginal_x(c(1, 1, 2, 2), c(3, 4, 3, 4))
#' @export
pipsa_marginal_x <- function(x, y) {
  margin.table(pipsa_table_from_variables(x, y), 1)
}

#' Marginal Distribution of Y
#'
#' Computes the marginal frequency distribution for variable Y (columns).
#'
#' @param x A vector for variable X.
#' @param y A vector for variable Y.
#' @return A 1D table of marginal frequencies for Y.
#' @examples
#' pipsa_marginal_y(c(1, 1, 2, 2), c(3, 4, 3, 4))
#' @export
pipsa_marginal_y <- function(x, y) {
  margin.table(pipsa_table_from_variables(x, y), 2)
}

#' Test for Statistical Independence
#'
#' Checks whether two variables X and Y
#' are statistically independent (f_ij == f_i * f_j).
#'
#' @param x A vector for variable X.
#' @param y A vector for variable Y.
#' @return Logical `TRUE` if independent, `FALSE` otherwise.
#' @examples
#' pipsa_is_independent(c(1, 1, 2, 2), c(3, 4, 3, 4))
#' @export
pipsa_is_independent <- function(x, y) {
  tbl <- pipsa_table_from_variables(x, y)
  freqs <- prop.table(tbl)

  rel_products <- outer(margin.table(freqs, 1), margin.table(freqs, 2), "*")

  isTRUE(all.equal(c(freqs), c(rel_products)))
}

#' Population Covariance
#'
#' Computes the population covariance between variables X and Y:
#'  mean((x - mean(x)) * (y - mean(y))).
#'
#' @param x Numeric vector for X.
#' @param y Numeric vector for Y.
#' @return Numeric covariance value.
#' @examples
#' pipsa_covariance(c(1, 2, 3), c(2, 4, 6))
#' @export
pipsa_covariance <- function(x, y) {
  mean((x - mean(x)) * (y - mean(y)))
}

#' Population Variance (Imported from chapter 1)
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

#' Population Standard Deviation (Imported from chapter 1)
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

#' Pearson Linear Correlation
#'
#' Computes the Pearson linear correlation coefficient:
#'  cov(X, Y) / (std_dev(X) * std_dev(Y)).
#'
#' @param x Numeric vector for X.
#' @param y Numeric vector for Y.
#' @return Numeric correlation between -1 and 1.
#' @examples
#' pipsa_linear_correlation(c(1, 2, 3, 4), c(2, 4, 6, 8))
#' @export
pipsa_linear_correlation <- function(x, y) {
  pipsa_covariance(x, y) / (pipsa_std_deviation(x) * pipsa_std_deviation(y))
}

#' Conditional Distribution Table Generator
#'
#' Computes the conditional distribution
#' (absolute and relative frequencies) given a condition variable.
#'
#' @param values A contingency table or matrix with row/column names.
#' @param condition_var Either "x" (conditioning on row value)
#' or "y" (conditioning on col value).
#' @param value The specific value conditioned on (character or numeric).
#' @param returnMean Returns the mean of the given conditional. Default FALSE
#' @return A data.frame with counts and relative frequencies.
#' @examples
#' tbl <- table(c(1, 1, 2, 2), c("A", "B", "A", "B"))
#' pipsa_cond_dist_gen(tbl, condition_var = "x", value = "1")
#' @export
pipsa_cond_dist_gen <- function(values, condition_var = c("x", "y"), value, returnMean = FALSE) {
  condition_var <- match.arg(condition_var)
  if (condition_var == "x") {
    counts <- values[as.character(value), ]
    rel_freq <- counts / sum(counts)
    res <- data.frame(
      y = names(counts),
      absolute = as.vector(counts),
      relative = as.vector(rel_freq)
    )
  } else {
    counts <- values[, as.character(value)]
    rel_freq <- counts / sum(counts)
    res <- data.frame(
      x = names(counts),
      absolute = as.vector(counts),
      relative = as.vector(rel_freq)
    )
  }

  if (returnMean) {
    mean <- sum(res$relative * as.numeric(res$y))
    return(list("table" = res, "mean" = mean))
  } else {
    res
  }
}

#' Bivariate Joint Moment of Order (r, s) w.r.t. (a, b)
#'
#' Computes bivariate joint moment sum((x - a)^r * (y - b)^s * `values[i, j]`).
#'
#' @param values A matrix/table of joint frequencies
#' with numeric row and column names.
#' @param r Integer order for X.
#' @param s Integer order for Y.
#' @param a Numeric reference point for X.
#' @param b Numeric reference point for Y.
#' @return Numeric moment value.
#' @export
pipsa_mom_ord <- function(values, r, s, a, b) {
  x <- as.numeric(rownames(values))
  y <- as.numeric(colnames(values))
  res <- 0
  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      res <- res + (((x[i] - a)^r) * ((y[j] - b)^s) * values[i, j])
    }
  }
  res
}

#' Bivariate Joint Central Moment
#'
#' Computes the bivariate central moment
#' of order (r, s) about means of row/col names.
#'
#' @param values A matrix/table of joint frequencies
#' with numeric row and column names.
#' @param r Integer order for X.
#' @param s Integer order for Y.
#' @return Numeric central moment value.
#' @export
pipsa_mom_cent <- function(values, r, s) {
  pipsa_mom_ord(
    values,
    r,
    s,
    mean(as.numeric(rownames(values))),
    mean(as.numeric(colnames(values)))
  )
}

#' Regression Curve Fitting & Evaluation
#'
#' Fits a mathematical model to bivariate data (X, Y)
#' and calculates Model, mse, Var(Y), R^2, and predictions.
#'
#' @param data_x Numeric vector for independent variable X.
#' @param data_y Numeric vector for dependent variable Y.
#' @param fitting Character specifying model type:
#'   - "l": Linear (y = a + bx)
#'   - "lo": Logarithmic (y = a + b * ln(x))
#'   - "e": Exponential (y = a * x^b)
#'   - "i": Hyperbolic/Inverse (y = 1 / (a + bx))
#'   - "q": Quadratic (y = ax^2 + bx + c)
#'   - "t": Trigonometric (y = a·sin(x) + b·cos(x))
#' @return A named list: Model, mse, VarianceY, R2, Plot, Curve, Predict (function).
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 5, 8, 10)
#' res <- pipsa_model_func(x, y, fitting = "l")
#' res$Predict(20)
#' @export
pipsa_model_func <- function(data_x, data_y, fitting) {
  n <- length(data_y)
  err <- data_y

  # preparing graphical info
  p <- plot(data_x, data_y)
  curve <- NULL
  predict_func <- NULL

  # the function must perform different
  # operations depending on the type of fitting specified,
  # but it mostly follows the same steps for each type
  if (fitting == "l") {
    # LINEAR
    model <- lm(data_y ~ data_x)

    a <- coef(model)[1]
    b <- coef(model)[2]

    curve <- curve(
      (a + b * x),
      from = min(data_x), to = max(data_x), col = "blue", add = TRUE
    )

    y_est <- a + b * data_x
    err <- err - y_est
    predict_func <- function(x_val) {
      a + b * x_val
    }
  } else if (fitting == "lo") {
    # LOGARITHMIC
    model <- lm(data_y ~ log(data_x))

    a <- coef(model)[1]
    b <- coef(model)[2]

    curve <- curve(
      (a + b * log(x)),
      from = min(data_x), to = max(data_x), col = "orange", add = TRUE
    )

    y_est <- a + b * log(data_x)
    err <- err - y_est
    predict_func <- function(x_val) {
      a + b * log(x_val)
    }
  } else if (fitting == "e") {
    # EXPONENTIAL
    lnX <- log(data_x)
    lnY <- log(data_y)

    model <- lm(lnY ~ lnX)

    a <- exp(coef(model))[1]
    b <- coef(model)[2]

    curve <- curve(
      (a * x^b),
      from = min(data_x), to = max(data_x), col = "green", add = TRUE
    )

    y_est <- a * data_x^b
    err <- err - y_est
    predict_func <- function(x_val) {
      a * x_val^b
    }
  } else if (fitting == "i") {
    # Hyperbolic aka inverse
    model <- lm(1 / data_y ~ data_x)

    a <- coef(model)[1]
    b <- coef(model)[2]

    curve <- curve(
      (1 / (a + b * x)),
      from = min(data_x), to = max(data_x), col = "red", add = TRUE
    )

    y_est <- 1 / (a + b * data_x)
    err <- err - y_est
    predict_func <- function(x_val) {
      1 / (a + b * x_val)
    }
  } else if (fitting == "q") {
    # QUADRATIC
    data_x2 <- data_x^2

    model <- lm(data_y ~ data_x + data_x2)

    a <- coef(model)[3]
    b <- coef(model)[2]
    c <- coef(model)[1]

    curve <- curve(
      (a * (x^2) + (b * x) + c),
      from = min(data_x), to = max(data_x), col = "black", add = TRUE
    )

    y_est <- a * (data_x2) + (b * data_x) + c
    err <- err - y_est
    predict_func <- function(x_val) {
      a * (x_val^2) + (b * x_val) + c
    }
  } else if (fitting == "t") {
    # TRIGONOMETRIC
    model <- lm(data_y ~ 0 + sin(data_x) + cos(data_x))

    a <- coef(model)[1]
    b <- coef(model)[2]

    curve <- curve(
      (a * sin(x) + b * cos(x)),
      from = min(data_x), to = max(data_x), col = "purple", add = TRUE
    )

    y_est <- a * sin(data_x) + b * cos(data_x)
    err <- err - y_est
    predict_func <- function(x_val) {
      a * sin(x_val) + b * cos(x_val)
    }
  }

  # calculation of final variables
  mse <- mean(err^2)
  var_y <- var(data_y) * (n - 1) / n
  r2 <- 1 - (mse / var_y)

  result <- list(
    "Model" = model,
    "mse" = mse,
    "VarianceY" = var_y,
    "R2" = r2,
    "Plot" = p,
    "Curve" = curve,
    "Predict" = predict_func
  )
  result
}

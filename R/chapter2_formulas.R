# ========================================================================
# GENERAL FUNCTIONS (shared by most files)

#' Print
#'
#' Prints a number/series of numbers with the
#' correct rounding to do the exam faster
#'
#' @param x A number or series of numbers we want to print
#' @param string A string to print before the number (default is empty)
#' @param round_val The number of decimals to round to (default is 5)
#' @return The number printed on the console with correct rounding
#' @examples
#' pipsa_print(3.14159)
#' @export
pipsa_print <- function(x, string = "", round_val = 5, reverse = FALSE) {
  val <- if (is.numeric(x)) {
    round(x, round_val)
  } else {
    x
  }
  if (length(x) > 1) {
    print(string)
    print(val)
  } else {
    if (reverse == TRUE) {
      print(paste(val, string))
    } else {
      print(paste(string, val))
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
#' It is the equivalent of doing cor.test(x, y, method = "pearson")
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
    list("table" = res, "mean" = mean)
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
#'   - "e": Exponential (y = a * e^(bx))
#'   - "p": Power (y = a * x^b)
#'   - "i": Hyperbolic/Inverse (y = 1 / (a + bx))
#'   - "q": Quadratic (y = ax^2 + bx + c)
#'   - "t": Trigonometric (y = a * sin(x) + b * cos(x))
#' @return A named list: Model, mse, VarianceY, R2, Plot, Curve, Predict (function).
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 5, 8, 10)
#' res <- pipsa_model_func(x, y, fitting = "e")
#' res$Predict(20)
#' @export
pipsa_model_func <- function(data_x, data_y, fitting) {
  n <- length(data_y)
  err <- data_y

  plot(data_x, data_y, main = paste("Curve Fitting:", toupper(fitting)), pch = 16)
  curve_obj <- NULL
  predict_func <- NULL

  if (fitting == "l") {
    # LINEAR
    model <- lm(data_y ~ data_x)

    a <- coef(model)[1]
    b <- coef(model)[2]

    curve_obj <- curve(
      (a + b * x),
      from = min(data_x), to = max(data_x), col = "blue", add = TRUE, lwd = 2
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

    curve_obj <- curve(
      (a + b * log(x)),
      from = min(data_x), to = max(data_x), col = "orange", add = TRUE, lwd = 2
    )

    y_est <- a + b * log(data_x)
    err <- err - y_est
    predict_func <- function(x_val) {
      a + b * log(x_val)
    }
  } else if (fitting == "e") {
    # EXPONENTIAL (y = a * e^(bx))
    lnY <- log(data_y)

    model <- lm(lnY ~ data_x)

    a <- exp(coef(model)[1])
    b <- coef(model)[2]

    curve_obj <- curve(
      (a * exp(b * x)),
      from = min(data_x), to = max(data_x), col = "green", add = TRUE, lwd = 2
    )

    y_est <- a * exp(b * data_x)
    err <- err - y_est
    predict_func <- function(x_val) {
      a * exp(b * x_val)
    }
  } else if (fitting == "p") {
    # POWER (y = a * x^b)
    lnX <- log(data_x)
    lnY <- log(data_y)

    model <- lm(lnY ~ lnX)

    a <- exp(coef(model)[1])
    b <- coef(model)[2]

    curve_obj <- curve(
      (a * x^b),
      from = min(data_x), to = max(data_x), col = "cyan", add = TRUE, lwd = 2
    )

    y_est <- a * data_x^b
    err <- err - y_est
    predict_func <- function(x_val) {
      a * x_val^b
    }
  } else if (fitting == "i") {
    # HYPERBOLIC / INVERSE
    model <- lm(1 / data_y ~ data_x)

    a <- coef(model)[1]
    b <- coef(model)[2]

    curve_obj <- curve(
      (1 / (a + b * x)),
      from = min(data_x), to = max(data_x), col = "red", add = TRUE, lwd = 2
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

    c <- coef(model)[1]
    b <- coef(model)[2]
    a <- coef(model)[3]

    curve_obj <- curve(
      (a * (x^2) + (b * x) + c),
      from = min(data_x), to = max(data_x), col = "black", add = TRUE, lwd = 2
    )

    y_est <- a * (data_x2) + (b * data_x) + c
    err <- err - y_est
    predict_func <- function(x_val) {
      a * (x_val^2) + (b * x_val) + c
    }
  } else if (fitting == "t") {
    # TRIGONOMETRIC (Added intercept 'c' for baseline shift)
    model <- lm(data_y ~ sin(data_x) + cos(data_x))

    c <- coef(model)[1]
    a <- coef(model)[2]
    b <- coef(model)[3]

    curve_obj <- curve(
      (c + a * sin(x) + b * cos(x)),
      from = min(data_x), to = max(data_x), col = "purple", add = TRUE, lwd = 2
    )

    y_est <- c + a * sin(data_x) + b * cos(data_x)
    err <- err - y_est
    predict_func <- function(x_val) {
      c + a * sin(x_val) + b * cos(x_val)
    }
  }

  # 3. Save the Plot properly using recordPlot()
  p <- recordPlot()

  # Calculation of final variables
  mse <- mean(err^2)
  var_y <- var(data_y) * (n - 1) / n

  # Prevent division by zero if all Y values are identical
  r2 <- if (var_y == 0) NA else 1 - (mse / var_y)

  result <- list(
    "Model" = model,
    "mse" = mse,
    "VarianceY" = var_y,
    "R2" = r2,
    "Plot" = p,
    "Curve" = curve_obj,
    "Predict" = predict_func
  )

  result
}
#' Obtain spearman correlation
#'
#' Obtains spearman correlation from 2 vectors
#' The cor.test function also accepts pearson and kendall
#'
#' @param x A vector for variable X
#' @param y A vector for variable Y
#' @return The Spearman correlation
#' @examples
#' pipsa_spearman_correlation(c(1, 2, 3), c(4, 5, 6))
#' @export
pipsa_spearman_correlation <- function(x, y) {
  cor.test(x, y, method = "spearman")
}

#' Chi-Square Statistic
#'
#' Computes the Pearson Chi-Square statistic for a contingency table.
#'
#' @param tab A contingency table or matrix.
#' @return The numeric Chi-square value.
#' @examples
#' data <- matrix(c(30, 20, 60, 40, 40, 10), nrow = 3, byrow = TRUE)
#' pipsa_chi_square(data)
#' @export
pipsa_chi_square <- function(tab) {
  test <- chisq.test(tab, correct = FALSE)
  test$statistic
}

#' Pearson's Contingency Coefficient (C)
#'
#' Computes Pearson's Contingency Coefficient C for a contingency table.
#'
#' @param tbl A contingency table or matrix.
#' @return The numeric coefficient C.
#' @examples
#' data <- matrix(c(30, 20, 60, 40, 40, 10), nrow = 3, byrow = TRUE)
#' pipsa_contingency_coef(data)
#' @export
pipsa_contingency_coef <- function(tbl) {
  chi2 <- pipsa_chi_square(tbl)
  n <- sum(tbl)
  sqrt(chi2 / (chi2 + n))
}

#' Plane Fitting & Evaluation
#'
#' Fits a multiple linear regression plane (z = a + bx + cy) to 3D spatial data
#' and calculates the Model, Sum of Squared Errors (SSE), Residual Variance, and R^2.
#'
#' @param x Numeric vector for independent variable X.
#' @param y Numeric vector for independent variable Y.
#' @param z Numeric vector for dependent variable Z.
#' @return A named list: Model, SSE, ResidualVariance, R2, Predict (function).
#' @examples
#' x <- c(0, 1, 0, -1, 0, 1, 1, 2)
#' y <- c(0, 1, 1, 1, 3, 2, 2, 0)
#' z <- c(2, 3, 4, 5, 7, 4, 5, 0)
#' pipsa_plane_fit(x, y, z)
#' @export
pipsa_plane_fit <- function(x, y, z) {
  n <- length(z)

  model <- lm(z ~ x + y)

  a <- coef(model)[1]
  b <- coef(model)[2]
  c <- coef(model)[3]

  z_est <- a + (b * x) + (c * y)
  err <- z - z_est

  sse <- sum(err^2)
  res_var <- sse / n
  sst <- sum((z - mean(z))^2)
  r2 <- 1 - (sse / sst)

  predict_func <- function(x_val, y_val) {
    unname(a + (b * x_val) + (c * y_val))
  }

  result <- list(
    "Model" = model,
    "SSE" = sse,
    "ResidualVariance" = res_var,
    "R2" = r2,
    "Predict" = predict_func
  )
  result
}

#' Compare Regression Models
#'
#' Fits all seven models, ranks them by highest R2 (then lowest MSE),
#' and optionally predicts a value using the best fit.
#'
#' @param x Numeric vector for independent variable X.
#' @param y Numeric vector for dependent variable Y.
#' @return Returns the results table, the winning model and the models list.
#' @export
pipsa_compare_models <- function(x, y) {
  keys <- c("l", "lo", "e", "p", "i", "q", "t")

  results <- data.frame(Model = character(), R2 = numeric(), MSE = numeric())

  models <- list()

  for (k in keys) {
    fit <- tryCatch(suppressWarnings(pipsa_model_func(x, y, fitting = k)),
      error = function(e) NULL
    )

    if (!is.null(fit)) {
      models[[k]] <- fit
      results <- rbind(results, data.frame(Model = k, R2 = fit$R2, MSE = fit$mse))
    }
  }
  results <- results[order(-results$R2, results$MSE), ]

  cat("\n--- MODEL COMPARISON ---\n")
  print(results, row.names = FALSE)
  cat("------------------------\n")

  best_key <- results$Model[1]
  best_model <- models[[best_key]]

  list(table = results, best_model = best_model, models = models)
}

#' Regression validity
#'
#' Check the validity of 2 regression slopes (Reference: Exercise 10)
#'
#' @param b_yx Slope of y on x
#' @param b_xy Slope of x on y
#' @return Whether the regression lines are valid
#' @examples
#' # Suppose you have r_{y/x} === y = 5 + 8x
#' # and r_{x/y} === y = 3 -2x => x = 3/2 - y/2
#' pipsa_is_regression_valid(8, -0.5)
#' @export
pipsa_is_regression_valid <- function(b_yx, b_xy) {
  r2 <- b_xy * b_yx
  (r2 > 0) && (r2 <= 1)
}

#' Theoretical Regression Parameters from Slopes
#'
#' Computes the linear correlation coefficient (r), covariance (s_xy),
#' and population variance of Y (sigma_y^2) given the regression slopes
#' b_yx and b_xy alongside the variance of X.
#'
#' @param b_yx Numeric slope of regression line Y on X.
#' @param b_xy Numeric slope of regression line X on Y.
#' @param var_x Numeric population variance of variable X.
#' @return A named list containing `r` (correlation),
#' `cov` (covariance), and `var_y` (variance of Y).
#' @examples
#' # Given slopes b_yx = 2/9, b_xy = 2, and var_x = 9
#' pipsa_regression_from_slopes(2 / 9, 2, 9)
#' @export
pipsa_regression_from_slopes <- function(b_yx, b_xy, var_x) {
  r <- sign(b_yx) * sqrt(b_yx * b_xy)
  cov_xy <- b_yx * var_x
  var_y <- cov_xy / b_xy

  pipsa_print(r, "Correlation coefficient (r): ")
  pipsa_print(cov_xy, "Covariance (s_xy): ")
  pipsa_print(var_y, "Variance of Y (sigma_y^2): ")

  list(r = r, cov = cov_xy, var_y = var_y)
}

#' Update Regression Parameters and Lines After Removing an Observation
#'
#' Recalculates sample means, population variances, covariance, and the updated
#' regression equations (r_Y/X and r_X/Y)
#' after removing a specific data point (x0, y0).
#'
#' @param N Integer original sample size.
#' @param mean_x Numeric original mean of X.
#' @param mean_y Numeric original mean of Y.
#' @param var_x Numeric original population variance of X.
#' @param var_y Numeric original population variance of Y.
#' @param cov_xy Numeric original population covariance between X and Y.
#' @param pt Numeric vector of length 2 representing
#' the removed point c(x0, y0).
#' @return A named list with updated summary statistics:
#' `mean_x`, `mean_y`, `var_x`, `var_y`, and `cov`.
#' @examples
#' # Remove point (2, -1) from a sample of N = 10
#' pipsa_remove_point_stats(
#'   N = 10, mean_x = 4, mean_y = 0,
#'   var_x = 9, var_y = 1, cov_xy = 2,
#'   pt = c(2, -1)
#' )
#' @export
pipsa_remove_point_stats <- function(
  N, mean_x, mean_y, var_x, var_y, cov_xy, pt
) {
  N_new <- N - 1
  x0 <- pt[1]
  y0 <- pt[2]

  mx_new <- (N * mean_x - x0) / N_new
  my_new <- (N * mean_y - y0) / N_new

  vx_new <- (N * (var_x + mean_x^2) - x0^2) / N_new - mx_new^2
  vy_new <- (N * (var_y + mean_y^2) - y0^2) / N_new - my_new^2
  cov_new <- (
    N * (
      cov_xy + mean_x * mean_y
    ) - x0 * y0
  ) / N_new - mx_new * my_new

  b_yx <- cov_new / vx_new
  a_yx <- my_new - b_yx * mx_new

  b_xy <- cov_new / vy_new
  a_xy <- mx_new - b_xy * my_new

  pipsa_print(
    paste0(
      "y = ",
      round(a_yx, 5),
      " + ",
      round(b_yx, 5), " x"
    ), "New r_{Y/X}: "
  )
  pipsa_print(
    paste0(
      "x = ",
      round(a_xy, 5),
      " + ",
      round(b_xy, 5),
      " y"
    ), "New r_{X/Y}: "
  )

  list(
    mean_x = mx_new,
    mean_y = my_new,
    var_x = vx_new,
    var_y = vy_new,
    cov = cov_new
  )
}

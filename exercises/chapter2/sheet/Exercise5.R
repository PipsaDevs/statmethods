# A study is being conducted on the impact of fatigue in a workplace on performance. For 10
# workers, the number of consecutive hours worked and the number of errors made have been
# recorded, as shown in the following table:
# Horas = x   | 2 2 3 3 4 4 5 6 7 7
# Errores = y | 0 1 1 1 3 3 6 7 8 7

x <- c(2, 2, 3, 3, 4, 4, 5, 6, 7, 7)
y <- c(0, 1, 1, 1, 3, 3, 6, 7, 8, 7)

# (a) Plot its scatter diagram and observe that, apparently, there is a good linear correlation.

source("R/chapter2_formulas.R")

res_y_on_x <- pipsa_model_func(x, y, "l") # there is good linear correlation! omg! so cute! so kawaii!

# (b) Calculate the coefficient of that linear correlation

pipsa_print(pipsa_linear_correlation(x, y), "Correlation: ") # Sol.: r = 0.972

# (c) Calculate the regression line of the errors on working hours.

pipsa_regression_line_from_model(res_y_on_x$Model) # Sol.: y = 1.55 x − 2.98

# (d) The regression line of working hours on errors.

res_x_on_y <- pipsa_model_func(y, x, "l")

pipsa_regression_line_from_model(res_x_on_y$Model, FALSE) # Sol.: x = 0.61 y + 2.05

# (e) Draw both regression lines and check that they intersect at the point where the means of both variables coincide.

p <- plot(x, y)
pipsa_plot_from_model(res_x_on_y$Model, "red", 2, FALSE)
pipsa_plot_from_model(res_y_on_x$Model, "blue")

pipsa_plot_point(mean(x), mean(y)) # They do!

# (f) Expected errors for 10 hours
res_y_on_x$Predict(10)

# (g) Expected hours for 13 errors
res_x_on_y$Predict(13)

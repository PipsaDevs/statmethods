# 6. Draw a scatter plot of the following data:
# X 1 20 50 100 200 300 500 1000
# Y 7 8.5 9 9.4 9.7 9.9 10.2 10.5

source("R/chapter2_formulas.R")

x <- c(1, 20, 50, 100, 200, 300, 500, 1000)
y <- c(7, 8.5, 9, 9.4, 9.7, 9.9, 10.2, 10.5)

# (a) Calculate its linear correlation coefficient and the regression line of Y on X.
pipsa_linear_correlation(x, y)
func <- pipsa_model_func(x, y, "l")
pipsa_obtain_function_from_model(func$Model)

# (b) Calculate a logarithmic regression model.
pipsa_obtain_function_from_model(pipsa_model_func(x, y, "lo")$Model)

# (c) Calculate a logarithmic regression model.
pipsa_model_func(x, y, "lo")$Predict(400)

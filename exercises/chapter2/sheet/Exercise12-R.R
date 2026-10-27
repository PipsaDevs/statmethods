source("R/chapter2_formulas.R")

xi <- c(0, 1, 0, -1, 0, 1, 1, 2)
yi <- c(0, 1, 1, 1, 3, 2, 2, 0)
zi <- c(2, 3, 4, 5, 7, 4, 5, 0)

plane_results <- pipsa_plane_fit(xi, yi, zi)

print(coef(plane_results$Model))

pipsa_print(plane_results$SSE, "Sum of Squared Errors (SSE):")
pipsa_print(plane_results$ResidualVariance, "Residual Variance:")
pipsa_print(plane_results$R2, "Coefficient of Determination (R2):")

expected_z <- plane_results$Predict(1, 1)
pipsa_print(expected_z, "Predicted Z for X=1, Y=1:")

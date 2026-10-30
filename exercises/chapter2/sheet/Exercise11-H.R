library(statmethods)
a <- 2
b <- 2 / 9
varx <- 9
n <- 10
pt <- c(2, -1)

pipsa_is_regression_valid(b, a)
res_a <- pipsa_regression_from_slopes(b, a, varx)
u <- matrix(c(1, -2, 2, -9), nrow = 2, byrow = TRUE)
v <- c(4, 8)
means <- solve(u, v)

res_b <- pipsa_remove_point_stats(
  N        = n,
  mean_x   = means[1],
  mean_y   = means[2],
  var_x    = varx,
  var_y    = res_a$var_y,
  cov_xy   = res_a$cov,
  pt       = pt
)

print(res_b)

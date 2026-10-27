library(statmethods)

xi <- c(0, 1, 0, -1, 0, 1, 1, 2)
yi <- c(0, 1, 1, 1, 3, 2, 2, 0)
zi <- c(2, 3, 4, 5, 6, 4, 5, 0)

plane_model <- lm(zi ~ xi + yi)

sse <- sum(residuals(plane_model)^2)
res_var <- sse / length(zi)

r2 <- 1 - (res_var / pipsa_variance(zi))

print(plane_model)
print(sse)
print(res_var)
print(r2)

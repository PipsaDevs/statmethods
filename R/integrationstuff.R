library(Ryacas)
library(dplyr)

yac_silent("Integrate(x) x;")
yac_silent("10 # AntiDeriv(_x, Exp(-(_x)^2)) <-- (Sqrt(Pi) / 2) * Erf(x);")
yac_silent("10 # Erf(0) <-- 0;")
yac_silent("10 # Erf(Infinity) <-- 1;")
yac_silent("10 # Erf(-Infinity) <-- -1;")
yac_silent("10 # Erf(-(_x)) <-- -Erf(x);")

# Erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
#
# x <- ysym("x")
#
# simplify(integrate(exp(-x^2), x, -Inf, Inf)) # y: Sqrt(Pi)
# res <- simplify(integrate(exp(-x^2), x, -1, 2)) # y: Sqrt(Pi)*Erf(1)
# res
#
# eval(as_r(res))


f <- function(x, a) {
  if (-1 <= x && x <= 0) {
    return(a * (x + 1))
  } else if (0 <= x && x <= 1) {
    return((x - 1)^2)
  } else {
    return(0)
  }
}

f <- case_when(
  -1 <= x & x <= 0 ~ a * (x + 1),
  0 <= x & x <= 1 ~ (x - 1)^2,
  TRUE ~ 0
)

pipsa_integrate <- function(f) {
  variables <- names(formals(f))
  environment <- new.env(parent = environment(f))
  for (sym in variables) {
    environment[[sym]] <- ysym(sym)
  }
  print(environment)
}

print(pipsa_integrate(f))
a

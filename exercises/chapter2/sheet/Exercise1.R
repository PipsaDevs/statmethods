# Five experimental pieces of an insulating material are tested under different pressures. The
# following are the values (P) of pressure (in Kg/cm2) and the resulting compression magnitudes
# (C) (in mm): (1,1), (2,1), (3,2), (4,2), and (5,4).

# Represent the frequency distribution

source("R/chapter2_formulas.R")

p <- c(1, 2, 3, 4, 5)
c <- c(1, 1, 2, 2, 4)

print(pipsa_freq_distribution(p, c))

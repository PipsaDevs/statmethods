# Considering the following table, calculate the mean, median, mode and quartiles C1 and C3.
# xi 1 2 3 4 5 6 7 8 9 10 11
# ni 2 9 14 20 18 15 9 6 4 2 1

source("R/chapter1_formulas.R")

xi <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
ni <- c(2, 9, 14, 20, 18, 15, 9, 6, 4, 2, 1)

data <- pipsa_generate_vector_with_frequencies(xi, ni)

pipsa_vector_info(data)

pipsa_print(pipsa_quartile(data, 1))
pipsa_print(pipsa_quartile(data, 3))

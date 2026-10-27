# 8. In a statistical study on the employment situation of 200 graduates after the first year of
# completing their studies, we are given the following contingency table:

source("R/chapter2_formulas.R")

data <- c(30, 20, 60, 40, 40, 10)

tab <- pipsa_table_from_vector(data, 3)

pipsa_chi_square(tab)

pipsa_contingency_coef(tab)

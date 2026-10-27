# By observing two statistical variables X and Y over a set of 20 individuals, the following values
# have been obtained:
# (5, 1), (5, 0), (5, 0), (8, 1), (5, 0), (5, 1), (8, 0), (5, 1), (5, 2), (8, 1),
# (5, 0), (8, 0), (5, 1), (5, 0), (8, 2), (5, 1), (5, 0), (5, 2), (8, 0), (5, 0)

# (a) Construct the joint frequency table.
# (b) Determine the relative distribution of Y conditioned on X = 5 and calculate its mean.
# (c) Are the variables independent? Justify your answer.

source("R/chapter2_formulas.R")

x <- c(5, 5, 5, 8, 5, 5, 8, 5, 5, 8, 5, 8, 5, 5, 8, 5, 5, 5, 8, 5)
y <- c(1, 0, 0, 1, 0, 1, 0, 1, 2, 1, 0, 0, 1, 0, 2, 1, 0, 2, 0, 0)

# a)
print(pipsa_freq_distribution(x, y))

# b)
table <- pipsa_table_from_variables(x, y)
pipsa_cond_dist_gen(table, "x", "5", TRUE)

# c)
pipsa_is_independent(x, y)

# Obtain the median of the following distribution:
# -----------
# | xi | ni |
# | 3  | 3  |
# | 4  | 15 |
# | 8  | 2  |
# | 10 | 4  |
# -----------
# Verify it by programming in R.

source("R/chapter1_formulas.R")

data <- c(3, 4, 8, 10)
frequencies <- c(3, 15, 2, 4)

pipsa_print(pipsa_vector_info(pipsa_generate_vector_with_frequencies(data, frequencies), FALSE)$median, "median: ")

# Median is 4

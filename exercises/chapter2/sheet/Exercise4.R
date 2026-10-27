source("R/chapter2_formulas.R")
source("R/chapter1_formulas.R")
# First, lets set up the data
x <- 0:1
y <- c(1, 3, 5)
tbl <- pipsa_table_from_vector(c(3, 7, 6, 14, 9, 21), nrows = 3)
# We use this so that x is associated to columns and y to rows
dimnames(tbl) <- list(y = y, x = x)

obs <- pipsa_table_to_vectors(tbl)

dothings <- function(a) {
  c(
    mean(a),
    median(a),
    pipsa_stat_mode(a)
  )
}

print(dothings(obs$x))

# Ok so we use x here because condition_var understands x as row value
cond <- pipsa_cond_dist_gen(tbl,
  condition_var = "x",
  value = 1,
  returnMean = TRUE
)

cond_vec <- rep(as.numeric(cond$table$y), times = cond$table$absolute)
print(dothings(cond_vec))
print(pipsa_is_independent(obs$x, obs$y))

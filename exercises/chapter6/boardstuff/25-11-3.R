# We wish to compare the best route to get home between two options, A
# and B. The time taken for each route is measured in minutes, yielding
# the following values

a <- c(29.5, 31.3, 28.5, 29.1, 30.9)
b <- c(29.98, 29.3, 28.42, 34.5, 32.5, 33.3)

t.test(a, b, conf.int = 0.95)

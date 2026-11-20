# A study is conducted on the salaries of employees at a company,
# where the focus is on the variability of these salaries. It is known
# that salaries follow a Normal distribution with a standard deviation
# of 1200.
# A sample of size 25 is taken.
# Calculate the probability that the sample variance (quasi-variance)
# is greater than 2500

n <- 25

sigma <- 1200

s2 <- 2500

pchisq((n - 1) * s2 / sigma^2, df = n - 1, lower.tail = FALSE)

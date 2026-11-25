# library(MKmisc)

n <- 20
x <- 15
p <- 0.5

p_value <- 1 - pbinom(x - 1, n, p)
p_value

# since 0.0206 < 0.05, we know the coin is biased, but there are other ways to demonstrate this

prob <- 1 - 15 / 20

prob / sqrt((prob * (1 - prob) / 20))

binom.test(x = 15, n = 20, p = 0.5, alternative = "greater", conf.level = 0.95)

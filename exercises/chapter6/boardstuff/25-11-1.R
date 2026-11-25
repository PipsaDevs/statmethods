# We have a coin, and we believe it is biased in favor of heads, meaning it
# produces more heads than tails when flipped. To decide whether this is
# true, we flip the coin 20 times and observe 15 heads. Let α = 0.05

# library(MKmisc)

n <- 20
x <- 15
p <- 0.5

p_value <- 1 - pbinom(x - 1, n, p)
p_value

# since 0.0206 < 0.05, we know the coin is biased, but there are other ways to demonstrate this

# prob <- 1 - 15 / 20

# prob / sqrt((prob * (1 - prob) / 20))

binom.test(x = 15, n = 20, p = 0.5, alternative = "greater", conf.level = 0.95)

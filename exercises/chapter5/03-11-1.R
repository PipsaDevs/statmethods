# Consider a hospital where, on average, there are 4 admissions per day.
# Then the random variable X that measures the number of admissions in
# a day follows a Poisson distribution model X ∼ P(4). Therefore, obtain:
# The probability that there are 5 admissions on any given day
# Sol.: 0.1563
# The probability that there are fewer than 2 admissions in a day
# Sol.: 0.0916
# The probability that there are more than 1 admission in a day
# Sol.: 0.9084

dpois(5, 4)

dpois(1, 4) + dpois(0, 4)

1 - dpois(1, 4) - dpois(0, 4)

# OTHER WAY TO SOLVE IT

dpois(5, 4)

ppois(1, 4)

1 - ppois(1, 4)

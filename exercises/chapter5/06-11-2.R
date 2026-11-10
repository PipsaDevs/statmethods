# Let X follows a N(60, 10). Calculate:
# P[X ≥ 70]
# P[X ≤ 80]
# P[X ≤ 30]
# P[X ≥ 46]
# P[39 ≤ X ≤ 80]
# P[80 ≤ X ≤ 82.5]
# P[30 ≤ X ≤ 40]

# P[X ≥ 70]
1 - pnorm(70, 60, 10)

# P[X ≤ 80]
pnorm(80, 60, 10)

# P[X ≤ 30]
pnorm(30, 60, 10)

# P[X ≥ 46]
1 - pnorm(46, 60, 10)

# P[39 ≤ X ≤ 80]
pnorm(80, 60, 10) - pnorm(39, 60, 10)

# P[80 ≤ X ≤ 82.5]
pnorm(82.5, 60, 10) - pnorm(80, 60, 10)

# P[30 ≤ X ≤ 40]
pnorm(40, 60, 10) - pnorm(30, 60, 10)

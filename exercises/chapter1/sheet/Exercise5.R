# According to the population census of 1990 for the towns in the province of Málaga, the total
# number of inhabitants in the province is 663.804. Based on this data, the- histogram in the
# activity sheet was created (can't paste it here bru)

# Calculate the frequency distribution of the inhabitants

source("R/chapter1_formulas.R")

freq_distribution <- pipsa_freq_distribution(c(0, 10, 50, 100), c(3.22, 1.34, 0.41))

print(freq_distribution)

# We want to test the effectiveness of a medication for fever reduction. To
# do so, the temperature was recorded for seven two-year-old children with
# the flu, before and after taking the medication. The results are as follows:

temperaturebefore <- c(39.3, 39.7, 39.9, 40, 38.9, 39.7, 39.3)

temperatureafter <- c(38.1, 38, 38.3, 37.9, 38.7, 38.3, 37)

sA <- sqrt(var(temperaturebefore))
sB <- sqrt(var(temperatureafter))

eA <- qt(0.95, length(temperaturebefore) - 1) * sA / sqrt(length(temperaturebefore))
eB <- qt(0.95, length(temperatureafter) - 1) * sB / sqrt(length(temperatureafter))

IA <- c(mean(temperaturebefore) - eA, mean(temperaturebefore) + eA)
IB <- c(mean(temperatureafter) - eB, mean(temperatureafter) + eB)

IA
IB

library(MKmisc)

normDiffCI()

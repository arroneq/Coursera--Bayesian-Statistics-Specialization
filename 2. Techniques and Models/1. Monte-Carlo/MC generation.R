set.seed(32)

m <- 100
a <- 2.0
b <- 1 / 3

theta <- rgamma(n = m, shape = a, rate = b)
head(theta)

hist(theta, freq = FALSE) # freq = FALSE -- probability distribition instead of counts

# compare to the analytical distribution
curve(
    dgamma(x, shape = a, rate = b),
    col = "blue",
    add = TRUE
) # x refers to the x-axis

# Monte Carlo approximation of dgamma expectation
mean(theta)
# real dgamma expectation
a / b

# Monte Carlo approximation of dgamma variation
var(theta)
# real dgamma variation
a / b^2

# let's approximate the probability that theta < 5
indicator <- theta < 5 # indicator variable
head(indicator) # TRUE FALSE TRUE FALSE TRUE FALSE

mean(indicator) # R converts TRUE FALSE into 1 and 0

# true probability of theta < 5
pgamma(q = 5, shape = a, rate = b)

# Monte Carlo approximation
quantile(theta, probs = 0.9)
# real quantile
qgamma(p = 0.9, shape = a, rate = b)
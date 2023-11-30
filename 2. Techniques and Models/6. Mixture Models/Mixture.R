# The PDF of this mixture distribution would look like this:

curve(
    0.4 * dexp(x, 1.0) + 0.6 * dnorm(x, 3.0, 1.0),
    from = -2.0,
    to = 7.0,
    ylab = "density",
    xlab = "y",
    main = "40/60 mixture of exponential and normal distributions",
    lwd = 2
)

# Let's draw the weighted PDFs for each population.
curve(
    0.4 * dexp(x, 1.0) + 0.6 * dnorm(x, 3.0, 1.0),
    from = -2.0,
    to = 7.0,
    ylab = "density",
    xlab = "y",
    main = "40/60 mixture of exponential and normal distributions",
    lwd = 2
)
curve(0.4 * dexp(x, 1.0), from = -2.0, to = 7.0, col = "red", lty = 2, add = TRUE)
curve(0.6 * dnorm(x, 3.0, 1.0), from = -2.0, to = 7.0, col = "blue", lty = 2, add = TRUE)

# Let's simulate from our example mixture distribution.

set.seed(117)
n <- 1000
z <- numeric(n)
y <- numeric(n)

for (i in 1:n) {
    z[i] <- sample.int(2, 1, prob = c(0.4, 0.6))
    # returns a 1 with probability 0.4, or a 2 with probability 0.6

    if (z[i] == 1) {
        y[i] <- rexp(1, rate = 1.0)
    } else if (z[i] == 2) {
        y[i] <- rnorm(1, mean = 3.0, sd = 1.0)
    }
}

hist(y, breaks = 30)
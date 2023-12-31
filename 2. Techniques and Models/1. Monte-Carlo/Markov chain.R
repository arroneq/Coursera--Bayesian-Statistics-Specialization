Q <- matrix(c(
    0.0, 0.5, 0.0, 0.0, 0.5,
    0.5, 0.0, 0.5, 0.0, 0.0,
    0.0, 0.5, 0.0, 0.5, 0.0,
    0.0, 0.0, 0.5, 0.0, 0.5,
    0.5, 0.0, 0.0, 0.5, 0.0),
nrow = 5, byrow = TRUE)

Q

Q %*% Q # Matrix multiplication in R. This is Q^2.

(Q %*% Q)[1,3]

Q5 <- Q %*% Q %*% Q %*% Q %*% Q # h=5 steps in the future
round(Q5, 3)

Q30 <- Q
for (i in 2:30) {
    Q30 <- Q30 %*% Q
}
round(Q30, 3) # h=30 steps in the future

# We can also demonstrate the stationary distribution by simulating a
# long chain from this example.

n <- 5000
x <- numeric(n)
x[1] <- 1 # fix the state as 1 for time 1
for (i in 2:n) {
    x[i] <- sample.int(5, size = 1, prob = Q[x[i-1],])
    # draw the next state from the intergers 1 to 5 with probabilities
    # from the transition matrix Q, based on the previous value of X.
}

# Now that we have simulated the chain, let’s look at the
# distribution of visits to the five states.

table(x) / n

set.seed(38)

n <- 1500
x <- numeric(n)
phi <- -0.6

for (i in 2:n) {
    x[i] <- rnorm(1, mean = phi*x[i-1], sd = 1.0)
}

plot.ts(x)

hist(x, freq = FALSE)
curve(
    dnorm(x, mean = 0.0, sd = sqrt(1.0/(1.0-phi^2))),
    col = "red",
    add = TRUE
)

legend(
    "topright",
    legend = "theoretical stationary\ndistribution",
    col = "red",
    lty = 1,
    bty = "n"
)
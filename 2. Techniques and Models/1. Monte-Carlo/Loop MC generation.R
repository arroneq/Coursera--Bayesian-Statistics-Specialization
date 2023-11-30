# Hierarchy model: first step is to simulate phi from Beta(2,3),
# then simulate y from Bin(10,phi)

m <- 1e5 # that is equal to m <- 100000

# create an empty zeros float arrays (vectors)
y <- numeric(m)
phi <- numeric(m)

for (i in 1:m) {
    phi[i] <- rbeta(1, shape1 = 2.0, shape2 = 2.0)
    y[i] <- rbinom(1, size = 10, prob = phi[i])
}

# loops in R are slow, use vectorised form instead:
phi <- rbeta(m, shape1 = 2.0, shape2 = 2.0)
y <- rbinom(m, size = 10, prob = phi)

hist(y)
likelihood <- function(n, y, theta) {
    return(theta^y * (1 - theta)^(n - y))
}

theta <- seq(0.01, 0.99, 0.01)

plot(theta, likelihood(400, 72, theta))

plot(seq(-10, 10, 0.01), dnorm(seq(-10, 10, 0.01)))

hist(rnorm(10000))
print(qnorm(0.975))

plot(theta, dbeta(theta, 32, 40), type = "l")
lines(theta, dbeta(theta, 8, 4), lty = 5)
lines(theta, dbeta(theta, 1, 5), type = "l", col = "red")

pbeta(0.5, 1, 5)

plot(theta, dbeta(theta, 8, 16), type = "l", col = "#26ff00")
qbeta(0.975, 8, 16)
pbeta(0.35, 8, 21)

lambda <- seq(0, 20)
plot(lambda, dgamma(lambda, 67, 6), type = "l")
lines(lambda, dgamma(lambda, 8, 1), lty = 2)
qgamma(0.95, 67, 6)

qnorm(0.975, 96.17, sqrt(0.042))
pnorm(100, 96.17, 0.042)

z <- rgamma(n = 500, shape = 3, rate = 200)
x <- 1 / z
print(mean(x))

z <- rgamma(1000, shape = 16.5, rate = 6022.9)
sigB <- 1 / z
muB <- rnorm(1000, mean = 609.3, sd = sqrt(sigB / 27.1))

quantile(x = mu, probs = c(0.025, 0.975))

z <- rgamma(1000, shape = 18, rate = 6796.4)
sigA <- 1 / z
muA <- rnorm(1000, mean = 622.4, sd = sqrt(sigA / 30.1))

mean(muA > muB)

theta <- seq(0, 1, 0.01)
plot(theta, dbeta(theta, 1/2, 1/2), type = "l")
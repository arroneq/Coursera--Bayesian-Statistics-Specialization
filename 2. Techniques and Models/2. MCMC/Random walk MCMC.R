log_lilelihood <- function(mu, n, ybar) {
    mu2 <- mu^2
    n * (ybar * mu - mu2 / 2.0) - log(1.0 + mu2)
}

metropolis_hastings <- function(n, ybar, n_iter, mu_init, cand_std) {
    mu_out <- numeric(n_iter)
    accept_count <- 0
    mu_now <- mu_init
    log_lilelihood_now <- log_lilelihood(mu = mu_now, n = n, ybar = ybar)

    for (i in 1:n_iter) {
        mu_cand <- rnorm(1, mean = mu_now, sd = cand_std)
        log_lilelihood_cand <- log_lilelihood(mu = mu_now, n = n, ybar = ybar)
        log_alpha <- log_lilelihood_cand - log_lilelihood_now
        alpha <- exp(log_alpha)

        u <- runif(1)
        if (u < alpha) {
            mu_now <- mu_cand
            accept_count <- accept_count + 1
            log_lilelihood_now <- log_lilelihood_cand
        }

        mu_out[i] <- mu_now
    }

    list(mu = mu_out, accept = accept_count / n_iter)
}

# data given (we can get an approximation of parameter mu from this data)
y <- c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)

ybar <- mean(y)
n <- length(y)
hist(y, freq = FALSE, xlim = c(-1.0, 3.0))
points(y, rep(0.0, n))
points(ybar, 0.0, col = "red")
curve(dt(x, df = 1), lty = 2, add = TRUE) # prior distribution for mu

# posterior sampling
set.seed(32) # in order to reproduce the same random sampling
posterior <- metropolis_hastings(
    n = n,
    ybar = ybar,
    n_iter = 1e3,
    mu_init = 0.0,
    cand_std = 0.05
)

str(posterior) # find out what is inside an object

# explore posterior using coda library
traceplot(as.mcmc(posterior$mu)) # history of MCMC accepting/rejecting process

# for random walk Metropolis-Hastings an appropriate acceptance rate is between 0.23 and 0.5
# in order to explore the wide amount of stated and dont get stuck a lot in the same state

# get rid of the first 1000 iterations when a chain is still exploring
posterior$mu_keep <- posterior$mu[-c(1:100)] # taking away first 100 elements
plot(density(posterior$mu_keep), xlim = c(-1.0, 3.0))
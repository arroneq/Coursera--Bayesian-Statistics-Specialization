library("rjags")

# 1. Specify the model

mod_string <- " model {
    for (i in 1:n) {
      y[i] ~ dnorm(mu, 1.0/sig2)
    }

    mu ~ dt(0.0, 1.0/1.0, 1.0) # location, inverse scale, degrees of freedom
    sig2 = 1.0
} "

# 2. Set up the model

set.seed(50)
y <- c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
n <- length(y)

data_jags <- list(y = y, n = n)
params <- c("mu")

inits <- function() {
    inits <- list("mu" = 0.0)
    inits
} # optional (and fixed)

mod <- jags.model(
    textConnection(mod_string),
    data = data_jags,
    inits = inits
)

# 3. Run the MCMC sampler

update(mod, 500) # burn-in

# 4. Post processing

mod_sim <- coda.samples(
    model = mod,
    variable.names = params,
    n.iter = 1000
)

summary(mod_sim)

library("coda")
plot(mod_sim)
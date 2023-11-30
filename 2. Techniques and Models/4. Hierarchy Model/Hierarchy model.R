getwd() # current working directory of R
setwd("/home/anton/Code/Coursera: Bayesian Statistics Specialization/2. Techniques and Models/4. Hierarchy Model/")

# ------------------------------------------------------------------------
# DATA PREPROCESSING
# ------------------------------------------------------------------------

# set R's working directory to the same directory as this file,
# or use the full path to the file

cookies <- read.table(file = "cookies.dat", header = TRUE)
head(cookies)
summary(cookies)

table(cookies$location)

head(london_weather)
tail(london_weather)

boxplot(chips ~ location, data = cookies)

# find out the dataset length (there is no id column in the dataset)
nrow(london_weather)

# ------------------------------------------------------------------------
# HIERARCHY MODEL (non-informative priors)
# ------------------------------------------------------------------------

set.seed(112)

n_sim <- 500

alpha_prior <- rexp(n_sim, rate = 1.0 / 2.0)
beta_prior <- rexp(n_sim, rate = 5.0)

mu_prior <- alpha_prior / beta_prior
sig_prior <- sqrt(alpha_prior / beta_prior^2)

summary(mu_prior)

# After simulating from the priors for α and β, we can use those
# samples to simulate further down the hierarchy:
lambda_prior <- rgamma(
    n = n_sim, shape = alpha_prior, rate = beta_prior
)
summary(lambda_prior)

# and the further down the hierarchy:
y_prior <- rpois(n = 150, lambda = rep(lambda_prior, each = 30))
hist(y_prior)

# Because these priors have high variance and are somewhat
# noninformative, they produce unrealistic predictive distributions.

# ------------------------------------------------------------------------
# HIERARCHY MODEL in JAGS (informative priors)
# ------------------------------------------------------------------------

mod_string <- " model {
    for (i in 1:length(chips)) {
        chips[i] ~ dpois(lam[location[i]])
    }

    for (j in 1:max(location)) {
        lam[j] ~ dgamma(alpha, beta)
    }

    alpha = mu^2 / sig^2
    beta = mu / sig^2

    mu ~ dgamma(2.0, 1.0/5.0)
    sig ~ dexp(1.0)
} "

set.seed(113)

data_jags <- as.list(cookies)

params <- c("lam", "mu", "sig")

model <- jags.model(
    textConnection(mod_string),
    data = data_jags,
    n.chains = 3
)

update(mod, 1e3) # burn-in

model_simulation <- coda.samples(
    model = model,
    variable.names = params,
    n.iter = 5e3
)

model_csim <- as.mcmc(do.call(rbind, model_simulation))

# convergence diagnostics
plot(model_simulation)

gelman.diag(model_simulation)
autocorr.diag(model_simulation)
autocorr.plot(model_simulation)
effectiveSize(model_simulation)

# compute DIC
dic <- dic.samples(model, n.iter = 1e3)
dic

# Mean deviance: 783.8
# penalty 4.878
# Penalized deviance: 788.7

# ------------------------------------------------------------------------
# POSTERIOR PREDICTIVE SIMULATION
# ------------------------------------------------------------------------

n_sim <- nrow(model_csim)

lambda_predicted <- rgamma(
    n = n_sim,
    shape = model_csim[,"mu"]^2 / model_csim[,"sig"]^2,
    rate = model_csim[,"mu"] / model_csim[,"sig"]^2
)

hist(lambda_predicted)

mean(lambda_predicted > 15)

y_predicted <- rpois(
    n = n_sim,
    lambda = lambda_predicted
)

hist(y_predicted)

mean(y_predicted > 15)
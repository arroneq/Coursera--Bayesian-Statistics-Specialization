getwd() # current working directory of R
setwd("/home/anton/Code/Coursera: Bayesian Statistics Specialization/2. Techniques and Models/3. Linear Regression/")

# ------------------------------------------------------------------------
# DATA PREPROCESSING
# ------------------------------------------------------------------------

# set R's working directory to the same directory as this file,
# or use the full path to the file
london_weather_raw <- read.csv(file = "london_weather.csv", header = TRUE)
summary(london_weather_raw)

head(london_weather)
tail(london_weather)

# clean the dataset
london_weather <- na.omit(london_weather_raw)
summary(london_weather)

# find out the dataset length (there is no id column in the dataset)
nrow(london_weather)

# shorten the dataset
london_weather <- london_weather[(1:1000), ]

# scatter plot of the data
pairs(london_weather)

rownames(london_weather)
colnames(london_weather)

# ------------------------------------------------------------------------
# BUILD-IN LINEAR REGRESSION MODEL (noninformative prior)
# ------------------------------------------------------------------------

plot(max_temp ~ min_temp, data = london_weather)
plot(sunshine ~ precipitation, data = london_weather)

sink(file = "output.txt")

pair_plot <- cbind(
    "max_temp" = london_weather$max_temp,
    "min_temp" = london_weather$min_temp
)

write.table(
    pair_plot,
    file = "output.txt",
    quote = FALSE,
    sep = " ",
    row.names = TRUE,
    col.names = TRUE
)

sink()
closeAllConnections()

london_weather$log_sunshine <- log(london_weather$sunshine)
london_weather$log_precipitation <- log(london_weather$precipitation)

plot(log_sunshine ~ log_precipitation, data = london_weather)

# the reference Bayesian analysis (with a noninformative prior) is available directly in R
linear_model <- lm(max_temp ~ min_temp, data = london_weather)
summary(linear_model)

# ------------------------------------------------------------------------
# LINEAR REGRESSION MODEL in JAGS (informative prior, 1 independent value)
# ------------------------------------------------------------------------

library("rjags")

jags_model1_string <- " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*min_temp[i] 
    }
    
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data1_jags <- list(
    y = london_weather$max_temp,
    n = nrow(london_weather),
    min_temp = london_weather$min_temp
)

params1 <- c("b", "sig")

inits1 <- function() {
    inits <- list(
        "b" = rnorm(2, 0.0, 100.0),
        "prec" = rgamma(1, 1.0, 1.0)
    )

    inits
}

model1 <- jags.model(
    textConnection(jags_model1_string),
    data = data1_jags,
    inits = inits1,
    n.chains = 3
)

update(model1, 1000) # burn-in period

model1_simulation <- coda.samples(
    model = model1,
    variable.names = params1,
    n.iter = 5000
)

# combine multiple chains
model1_combined_simulation <- do.call(rbind, model1_simulation)

# ------------------------------------------------------------------------
# MCMC CONVERGENCE GIAGNOSTIC
# ------------------------------------------------------------------------

plot(model1_simulation)

gelman.diag(model1_simulation)
autocorr.diag(model1_simulation)
autocorr.plot(model1_simulation)
effectiveSize(model1_simulation)

# we can get a posterior summary of the parameters in our model
summary(model1_simulation)

# ------------------------------------------------------------------------
# RESIDUAL CHECKS (for non-JAGS model)
# ------------------------------------------------------------------------

plot(resid(linear_model)) # to check independence (looks okay, no pattern)

sink(file = "output.txt")

linear_residuals <- resid(linear_model)

write.table(
    linear_residuals,
    file = "output.txt",
    quote = FALSE,
    sep = " ",
    row.names = TRUE,
    col.names = TRUE
)

sink()
closeAllConnections()

# to check for linearity, constant variance (looks okay, no pattern)
plot(predict(linear_model), resid(linear_model))

# to check Normality assumption (we want this to be a straight line)
qqnorm(resid(linear_model))

# which 5 days have the largest positive residuals? (possible outliers)
rownames(london_weather)[order(resid(linear_model), decreasing = TRUE)[1:5]]

# ------------------------------------------------------------------------
# LINEAR REGRESSION MODEL in JAGS (informative prior, 2 independent value)
# ------------------------------------------------------------------------

library("rjags")

jags_model2_string <- " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*min_temp[i] + b[3]*global_radiation[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data2_jags <- list(
    y = london_weather$max_temp,
    n = nrow(london_weather),
    min_temp = london_weather$min_temp,
    global_radiation = london_weather$global_radiation
)

params2 <- c("b", "sig")

inits2 <- function() {
    inits <- list(
        "b" = rnorm(3, 0.0, 100.0),
        "prec" = rgamma(1, 1.0, 1.0)
    )

    inits
}

model2 <- jags.model(
    textConnection(jags_model2_string),
    data = data2_jags,
    inits = inits2,
    n.chains = 3
)

update(model2, 1000) # burn-in period

model2_simulation <- coda.samples(
    model = model2,
    variable.names = params2,
    n.iter = 5000
)

# combine multiple chains
model2_combined_simulation <- do.call(rbind, model2_simulation)

# ------------------------------------------------------------------------
# MCMC CONVERGENCE GIAGNOSTIC
# ------------------------------------------------------------------------

plot(model2_simulation)

gelman.diag(model2_simulation)
autocorr.diag(model2_simulation)
autocorr.plot(model2_simulation)
effectiveSize(model2_simulation)

# we can get a posterior summary of the parameters in our model
summary(model2_simulation)

# ------------------------------------------------------------------------
# Deviance Information Criterion (DIC)
# ------------------------------------------------------------------------

# Let us compare models using Deviance Information Criterion.
# It essentially calculates the posterior mean of the log-likelihood
# and adds a penalty for model complexity.

dic.samples(model1, n.iter = 1e3) # -> b[1] + b[2]*min_temp[i]
# Mean deviance: 5493
# penalty 3.093
# Penalized deviance: 5496

dic.samples(model2, n.iter = 1e3) # -> b[1] + b[2]*min_temp[i] + b[3]*global_radiation[i]
# Mean deviance: 5172
# penalty 4.01
# Penalized deviance: 5176

# A smaller deviance means a higher likelihood.
# Thus the better-fitting model has a lower DIC value. The final DIC for
# the second model is lower than for the first, so we would prefer using
# the second model.
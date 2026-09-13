library(rethinking)
data <- read.csv("data.csv")
complexity <- data$complexity
lines_c <- standardize(complexity)
lines_c

req_quality <- data$req_quality
lines_rq <- standardize(req_quality)

## The book example (5.2, primate milk)
## says the expected outcome should also be zero
##when the standardized predictor equals zero.
##Since standardize() always produces mean≈0, sd≈1,
## can we always start with alpha ~ dnorm(0, 0.2)
## whenever both the outcome and predictors are standardized?

comm_volume <- data$comm_volume
lines_cv <- standardize(comm_volume)

dat_list <- list(CM = lines_cv, RQ = lines_rq, C = lines_c)
m1 <- ulam(
  alist(
    CM ~ dnorm(mu, sigma),
    mu <- alpha + beta * C + zeta * RQ,
    alpha ~ dnorm(0, 0.2),
    beta ~ dnorm(0, 0.5),
    zeta ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dat_list,
  chains = 4,
  cores = 4
)
source("load_data.R")

library(rethinking)
m2 <- ulam(
  alist(
    n_bugs ~ dgampois(lambda, phi), 
    log(lambda) <- alpha[language_id] + beta[project],
    alpha[language_id] ~ dnorm(0, sigma_l),
    beta[project] ~ dnorm(0, sigma_p),
    sigma_l ~ dexp(1),
    sigma_p ~ dexp(1),
    phi ~ dexp(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE,
  log_lik = TRUE, iter = 5e3
)
precis(m2)
#746 vector or matrix parameters hidden. Use depth=2 to show them.
#        mean   sd 5.5% 94.5% rhat ess_bulk
#sigma_l 4.74 0.73 3.73  6.04    1 20343.70
#sigma_p 1.08 0.06 0.99  1.17    1  2692.56
#phi     0.74 0.03 0.68  0.79    1  6775.31
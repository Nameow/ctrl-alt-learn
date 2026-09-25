source("load_data.R")
# [1] "project"     "n_bugs"      "language_id"

library(rethinking)
m4 <- ulam(
  alist(
    n_bugs ~ dgampois(lambda, phi),
    log(lambda) <- alpha[language_id] + beta[project],
    alpha[language_id] ~ dnorm(alpha_bar, sigma_l),
    beta[project] ~ dnorm(beta_bar, sigma_p),
    alpha_bar ~ dnorm(0, 1),
    beta_bar ~ dnorm(0, 1),
    sigma_l ~ dexp(1),
    sigma_p ~ dexp(1),
    phi ~ dexp(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE,
  log_lik = TRUE, iter = 5e3
)
precis(m4)
#746 vector or matrix parameters hidden. Use depth=2 to show them.
#          mean   sd  5.5% 94.5% rhat ess_bulk
#alpha_bar 1.40 0.62  0.44  2.41 1.00  7825.51
#beta_bar  0.23 0.21 -0.11  0.56 1.02   258.39
#sigma_l   3.35 0.80  2.16  4.70 1.00  2196.08
#sigma_p   1.08 0.06  0.99  1.17 1.00  4428.77
#phi       0.74 0.03  0.69  0.79 1.00  9514.82
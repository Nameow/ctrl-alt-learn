library(tidyverse)
source("~/Downloads/utils.R")
d <- load.FSE(cleanup = TRUE)
d <- by.project.language(d)
d <- d[, -c(2:5, 7:8)]
d$project <- as.integer(d$project)
colnames(d)
# [1] "project"     "n_bugs"      "language_id"

library(rethinking)
m3 <- ulam(
  alist(
    n_bugs ~ dgampois(lambda, phi),
    log(lambda) <- alpha[language_id] + beta[project],
    alpha[language_id] ~ dnorm(alpha_bar, sigma_l),
    beta[project] ~ dnorm(0, sigma_p),
    alpha_bar ~ dnorm(0, 1),
    sigma_l ~ dexp(1),
    sigma_p ~ dexp(1),
    phi ~ dexp(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE,
  log_lik = TRUE, iter = 5e3
)
precis(m3)
# 746 vector or matrix parameters hidden. Use depth=2 to show them.
#          mean   sd 5.5% 94.5% rhat ess_bulk
#alpha_bar 1.27 0.58 0.36  2.22    1 11460.85
#sigma_l   3.67 0.76 2.54  4.96    1 11089.43
#sigma_p   1.08 0.06 0.99  1.17    1  1758.18
#phi       0.74 0.03 0.68  0.80    1  4492.87
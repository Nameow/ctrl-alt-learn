library(tidyverse)
source("~/Downloads/utils.R")
d <- load.FSE(cleanup = TRUE)
d <- by.project.language(d)
d <- d[, -c(2:5, 7:8)]
d$project <- as.integer(d$project)
colnames(d)
# [1] "project"     "n_bugs"      "language_id"

library(rethinking)
# 如果只按语言分组,忽略项目, 估计每个project的 bug 数量
m1 <- ulam(
  alist(
    bugs ~ dgampois(lambda, phi),
    log(lambda) <- alpha[project],
    alpha[project] ~ dnorm(alpha_bar, sigma),
    alpha_bar ~ dnorm(0, 1),
    sigma ~ dexp(1),
    phi ~ dexp(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE,
  log_lik = TRUE, iter = 5e3
)
precis(m1)
post <- extract.samples(m1)
post_alpha <- mean(post$alpha_bar)
post_a_o <- exp(post_alpha)
post_a_o

post_sigma <- mean(post$sigma)
post_s_o <- exp(post_sigma)
post_s_o


#729 vector or matrix parameters hidden. Use depth=2 to show them.
#          mean   sd 5.5% 94.5% rhat ess_bulk
#alpha_bar 5.17 0.07 5.07  5.28    1  4890.63
#sigma     1.09 0.06 1.00  1.18    1  3129.56
#phi       0.61 0.03 0.57  0.66    1  8365.40
# mean(alpha_bar) = 5.17 : 换算回原始尺度 exp(5.17) ≈ 176.3542,代表729个项目一般情况下大约178个bug
# mean(sigma) = 1.09 : 两个项目相差 1 个 sigma,实际 bug 数期望值相差 exp(1.09) ≈ 2.962547 倍
# mean(phi) = 0.61 phi 越小, overdispersion 越强; 选NB而不是泊松

precis(m1, depth = 2)
# 729个项目的bug数量

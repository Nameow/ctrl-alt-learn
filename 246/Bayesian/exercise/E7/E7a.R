library(tidyverse)
source("~/Downloads/utils.R")
d <- load.FSE(cleanup = TRUE)
d <- by.project.language(d)
d <- d[, -c(2:5, 7:8)]
d$project <- as.integer(d$project)
dim(d)
# str(d)

library(rethinking)
# 如果只按语言分组,忽略项目, 估计每种语言的 bug 数量
m0 <- ulam(
  alist(
    bugs ~ dgampois(lambda, phi), # bug 数量服从 Negative-Binomial 分布
    # 因为 lambda (均值)必须是正数, 用log把它跟一个可以取任意值的线性组合连起来)
    log(lambda) <- alpha[language_id], # 每各语言一个alpha值，如下行所示
    alpha[language_id] ~ dnorm(alpha_bar, sigma), # 每个语言各自的alpha的先验都来自下面那行的正态分布
    alpha_bar ~ dnorm(0, 1), #hyperprior:对"语言的整体平均水平"这个 hyperparameter 的先验猜测
    sigma ~ dexp(1), #hyperprior:对"语言之间差异程度"这个 hyperparameter 的先验猜测(指数分布,保证是正数)
    phi ~ dexp(1) # 对离散度参数 phi 的先验(同样用指数分布保证正数) variance = lambda + lambda²/phi
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE,
  log_lik = TRUE, #额外保存每个数据点的 log-likelihood 值， 为了后面算 PSIS/WAIC 必需
  iter = 5e3 #每条链跑5000次迭代
)
precis(m0)
#           mean   sd 5.5% 94.5% rhat ess_bulk
# alpha_bar 5.18 0.35 4.60  5.66    1  9848.45
# sigma     1.19 0.28 0.84  1.67    1  9661.78
# phi       0.45 0.02 0.42  0.47    1 15670.89
# mean(alpha_bar) = 5.18 : 换算回原始尺度 exp(5.18) ≈ 178,代表17种语言"典型情况"下大约178个bug
# mean(sigma) = 1.19: 两个语言相差 1 个 sigma,实际 bug 数期望值相差 exp(1.19) ≈ 3.3 倍
# mean(phi) = 0.45 phi 越小, overdispersion (方差比模型预期大得多"的现象就叫 overdispersion) 越强;
precis(m0, depth = 2)
# 每种语言的bug数量
#           mean   sd 5.5% 94.5% rhat ess_bulk
# alpha[1]  7.67 0.16 7.41  7.93    1 14821.56
# alpha[2]  6.86 0.20 6.55  7.20    1 15936.51
# alpha[3]  6.81 0.16 6.56  7.07    1 14014.22
# alpha[4]  4.83 0.22 4.49  5.19    1 13552.49
# alpha[5]  4.82 0.21 4.49  5.16    1 14070.87
# alpha[6]  5.29 0.23 4.95  5.66    1 15894.41
# alpha[7]  4.81 0.24 4.43  5.21    1 13773.61
# alpha[8]  5.59 0.23 5.23  5.97    1 13860.31
# alpha[9]  6.15 0.17 5.89  6.43    1 15123.26
# alpha[10] 5.26 0.10 5.10  5.43    1 14994.70
# alpha[11] 4.86 0.20 4.54  5.20    1 14810.85
# alpha[12] 4.39 0.31 3.92  4.89    1 15326.03
# alpha[13] 6.69 0.19 6.40  7.01    1 15168.27
# alpha[14] 6.01 0.15 5.78  6.25    1 16156.50
# alpha[15] 6.11 0.18 5.83  6.40    1 15097.49
# alpha[16] 5.65 0.22 5.30  6.01    1 14196.82
# alpha[17] 3.78 0.20 3.46  4.11    1 13940.22
# alpha_bar 5.18 0.35 4.60  5.66    1  9848.45
# sigma     1.19 0.28 0.84  1.67    1  9661.78
# phi       0.45 0.02 0.42  0.47    1 15670.89

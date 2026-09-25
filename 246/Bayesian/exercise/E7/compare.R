setwd("C:/Users/Lin/sandbox")
source("246/Bayesian/exercise/E7/load_data.R")
# [1] "project"     "n_bugs"      "language_id"
length(unique(d$language_id))  # 17
length(unique(d$project))      # 729
library(rethinking)

#根据语言分组
m0 <- ulam(
  alist(
    n_bugs ~ dgampois(lambda, phi),
    log(lambda) <- alpha[language_id],
    alpha[language_id] ~ dnorm(alpha_bar, sigma),
    alpha_bar ~ dnorm(0, 1),
    sigma ~ dexp(1),
    phi ~ dexp(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE,
  log_lik = TRUE,
  iter = 5e3
)

#根据项目分组
m1 <- ulam(
  alist(
    n_bugs ~ dgampois(lambda, phi),
    log(lambda) <- alpha[project],
    alpha[project] ~ dnorm(alpha_bar, sigma),
    alpha_bar ~ dnorm(0, 0.2),
    sigma ~ dexp(1),
    phi ~ dexp(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE,
  log_lik = TRUE, iter = 5e3
)

#交叉分组
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

m3 <- ulam(
  alist(
    n_bugs ~ dgampois(lambda, phi),
    log(lambda) <- alpha[language_id] + beta[project],
    alpha[language_id] ~ dnorm(alpha_bar, sigma_l),
    beta[project] ~ dnorm(0, sigma_p),
    alpha_bar ~ dnorm(0, 0.5),
    sigma_l ~ dexp(1),
    sigma_p ~ dexp(1),
    phi ~ dexp(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE,
  log_lik = TRUE, iter = 5e3
)

m4 <- ulam(
  alist(
    n_bugs ~ dgampois(lambda, phi),
    log(lambda) <- alpha[language_id] + beta[project],
    alpha[language_id] ~ dnorm(alpha_bar, sigma_l),
    beta[project] ~ dnorm(beta_bar, sigma_p),
    alpha_bar ~ dnorm(0, 0.5),
    beta_bar ~ dnorm(0, 0.2),
    sigma_l ~ dexp(1),
    sigma_p ~ dexp(1),
    phi ~ dexp(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE,
  log_lik = TRUE, iter = 1e4
)
precis(m3)
precis(m4)

compare(m0, m1, m2, m3, m4, func = PSIS)
#      PSIS     SE   dPSIS  dSE pPSIS  weight
# m3 14147.5 128.45   0.0    NA 316.2   0.95
# m2 14153.6 128.86   6.1  4.26 318.0   0.04
# m4 14158.4 129.41  10.9  4.21 321.5   0.00
# m1 14411.8 137.71 264.3 37.17 368.1   0.00
# m0 14575.6 152.47 428.1 66.20  50.9   0.00

compare(m0, m1, m2, m3, m4, func = WAIC)
#      WAIC     SE dWAIC   dSE pWAIC weight
#m3 14037.1 121.67   0.0    NA 261.0   0.52
#m4 14037.7 121.76   0.6  1.00 261.1   0.38
#m2 14040.5 121.69   3.5  1.07 261.4   0.09
#m1 14248.3 128.76 211.2 32.48 286.3   0.00
#m0 14580.6 155.35 543.5 72.97  53.4   0.00

# Conclusion
# 1. 两个表都是 information criterion,估计样本外 deviance, PSIS 和 WAIC 都是越小越好
# 2. 每行的意思:
#   2.1 SE: 这个 PSIS/WAIC 值本身的 standard error
#   2.2 dPSIS/dWAIC : dPSIS/dWAIC的值和最好模型的差值
#   2.3 dSE: dPSIS/dWAIC和最好模型的差值的 standard error
#   2.4 pPSIS/pWAIC : effective number of parameters 它衡量的是模型的灵活程度,也就是过拟合的风险
#   2.5 weight : 所有模型的 weight 加起来等于 1。dWAIC 越小,weight 越大
#       在这几个模型里,每个模型"预测最好"的可能性各占多少
#      (an approximate way to indicate which model PSIS/WAIC prefers)
#
# 3. m3排第一行，看表的时候把另外三个分别和它比较
#  3.1 m3 m2 和 m4这种cross-classified的，
#      明显比m1, m0 那种只有一种 cluster type的对数据外的预测好
#      因为前三个的PSIS或者WAIC的值比后两个小很多
#  3.2 好不好主要看dPSIS/dWAIC除以dSE，
#      dPSIS/dWAIC 约为 dSE 的 4-6 倍,才算 "fairly strong indications"
#      例如 m2、m4 分别和 m3 相比:
#          6.1除以4.26约等于1.431，10.9除以4.21约等于2.57， 1.431和2.57都小于4，差不多
#          但是如果和m1相比 264.3除以37.17约等于7.11，7.11就大于4-6了
#
# 9. 为什么m4参数更多但没有m3好, 且rhat 1.02
#    m4 给两个分组都加了均值(alpha_bar 和 beta_bar),但数据只能确定两者的和,
#    单独每一个是多少确定不了。所以多加的这个参数是冗余的:没有带来预测上的提升,反而让模型更复杂。要把 iter 加到 1e4 才能跑稳
#    Rhat 应该接近 1,超过 1.01 就要警惕
#
# 10. language 和 project 哪个的变异更大?
#    看m3, alpha[language_id] ~ dnorm(alpha_bar, sigma_l)
#    beta[project] ~ dnorm(0, sigma_p)
#    后验 sigma_l (3.67) > sigma_p (1.08)，所以language影响更大
#
# 4. 有效参数
#    先看实际参数 precis(m0, depth = 2) 出了几行就是几个，17 种语言，729个项目
#     m0 : 17(alpha[language_id]) + 1 alpha_bar + 1(sigma) + 1(phi) = 20
#     m1:  729(alpha[project]) + 1 alpha_bar + 1(sigma) + 1(phi) = 732
#     m2:  729 + 17 + 1(sigma_l) + 1(sigma_p) + 1(phi) = 749
#     m3:  729 + 17 + alpha_bar + (sigma_l) + (sigma_p) + (phi) = 750
#     m4:  729 + 17 + alpha_bar + beta_bar + (sigma_l) + (sigma_p) + (phi) = 751
#    与他们的pPSIS/pWAIC相比:
#    m0 : 20 < 50.9/53.4 : 拟合很差:数据里有很多影响力很大的点,模型解释不了，项目之间的巨大差异完全没被建模
#    其他4个模型的pPSIS都是300+, pWAIC在200+，小于700+ 因为hyperparameter shrinkage 约束

# 5. 没有显著差别时选数值最低的(m3),或者选更简单的。
# 6. 两张表不一致时怎么看:
#     m2 和 m4 的排名对调了,因为它们本来就在误差范围内。
#     weight 差别很大(0.95 vs 0.52),不能用 weight 选模型。
# 7. PSIS 报了 Pareto k 警告,WAIC 没有这种诊断,所以这里 PSIS 更可信。
# 8. 因果问题(oct_2024 考过):这两张表只衡量预测能力。如果目的是因果推断,不能用它们选模型,要靠 DAG。
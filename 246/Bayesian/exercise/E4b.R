library(rethinking)
data("Wines2012")
d <- Wines2012

score <- d$score
lines_s <- standardize(score)

flight <- d$flight
is_red <- ifelse(flight == "red", 1, 0)

wine_amer <- d$wine.amer
judge_amer <- d$judge.amer

dat_list <- list(S = lines_s, fid = is_red, wid = wine_amer, jid = judge_amer)


## alpha 是起点,beta/zeta/delta 是"某个特征存在时,要在起点上加/减多少"。
## alpha : 基准情况(白酒+法国酒+法国评委)的平均分
## beta : "是红酒"这件事带来的分数变化量
## beta * fid : 只有这一行是红酒(fid=1)时才真的加上 beta,白酒(fid=0)时这一项等于0
## zeta	: "是美国酒"带来的分数变化量
## delta :  "评委是美国人"带来的分数变化量
m1 <- ulam(
  alist(
    S ~ dnorm(mu, sigma),
    mu <- alpha + beta * fid + zeta * wid + delta * jid,
    alpha ~ dnorm(0, 0.2),
    beta ~ dnorm(0, 0.5),
    zeta ~ dnorm(0, 0.5),
    delta ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dat_list,
  chains = 4,
  cores = 4
)
precis(m1, 2)

## 跑出来的 mean  sd  5.5% 94.5% rhat ess_bulk
## mean 就是"这件事平均让分数变化了多少"，每个系数的 mean,就是"这个特征存在 vs 不存在,平均会让 score 变化多少
## 比如alpha的mean等于-0.01的意思是基准组合(白酒+法国酒+法国评委)的平均分,大概就在整体平均水平(0)附近,几乎没有偏离
## delta = 0.22:评委是美国人 vs 法国人,平均让打分提高约0.22个标准差(美国评委平均打分偏高一点)

## sd:这个估计本身的不确定程度(越大越不确定)

## 5.5% / 94.5%:89%把握真实值落在这个范围内(下界/上界)
## delta (5.5% = -0.01,94.5% = 0.45)
## 模型有89%的把握认为，当评委是美国人时更可能的情况是打分更高,最多能高到0.45分左右;最坏情况下,美国评委给分甚至可能比法国评委平均低0.01分

## rhat:链是否收敛,越接近1越好
## ess_bulk:相当于抽到了多少个"独立"样本,越大越可靠，200就很好了
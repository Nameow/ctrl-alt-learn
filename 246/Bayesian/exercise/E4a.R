library(rethinking)
data("Wines2012")
d <- Wines2012
#d
score <- d$score
lines_x <- standardize(score)
lines_x
mean(lines_x)
sd(lines_x)

judge <- d$judge
index_j <- as.integer(judge)
index_j

wine <- d$wine
index_w <- as.integer(wine)
index_w

## S : 标准化后的 score(outcome)
## a : 9个评委各自的参数(文字转为编号一到9)
## a[jid]	: 取出这一行对应的那个评委的参数
## b : 20瓶酒各自的参数(一整组数字 编号一到20)
## b[wid]: 取出这一行对应的那瓶酒的参数
dat_list <- list(S = lines_x, jid = index_j, wid = index_w)

## 算的是: 拿已有的真实打分,倒推出"是什么原因造成了这些分数的差异"——评委的严格/宽松程度、酒本身的质量,各占多少
m1 <- ulam(
  alist(
    S ~ dnorm(mu, sigma),
    mu <- a[jid] + b[wid],
    a[jid] ~ dnorm(0, 0.5),
    b[wid] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dat_list,
  chains = 4,
  cores = 4
)
## 看 a[] 这组9个数字和 b[] 这组20个数字,哪组内部彼此差得更开,谁的变化幅度大,谁就是造成 score 差异的主要原因。
post <- extract.samples(m1)
sd(colMeans(post$a))   # 9个评委的 mean,彼此差多少 0.4840186
sd(colMeans(post$b))   # 20瓶酒的 mean,彼此差多少 0.2571812
## 所以评委是造成差异的主要原因

## precis(m1, 2)
## traceplot(m1)
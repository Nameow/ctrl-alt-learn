library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age < 18, ]
weight <- d2$weight
height <- d2$height
xbar <- mean(d2$weight)
xbar

## Modle
m1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- alpha + beta * (weight - xbar),
    alpha ~ dnorm(108.3189, 25.74514),
    beta ~ dnorm(2.57, 1.286),
    sigma ~ dunif(0, 50)
  ),
  data = list(height = height, weight = weight)
)
## sample from the quadratic approximation posterior stored in m1
post <- extract.samples(m1)

## generate 50 evenly spaced weight values from 1 to 45,
## used as x-axis points for plotting the regression line and intervals
weight_seq <- seq(from = 1, to = 45, length.out = 50)
head(weight_seq)

## draw mean regression line
## for each weight value, compute the average of mu across
## all 10000 posterior samples
## 如果体重是1kg (上一步x轴上的第一个)
## 1. 10000组不同参数会预测出的10000个不同的身高值> post$alpha + post$beta * (z - xbar)
## 2. 用mean平均一下这10000个数字
## apply 就是从x轴第一个开始算到最后一个，重复1. 和 2. 50 次，结果存为mu
mu <- sapply(weight_seq, function(z) mean(post$alpha + post$beta * (z - xbar)))
head(mu)

## for each weight value, get the 89% percentile interval (PI) of mu
## across the 10000 posterior samples — this captures uncertainty
## about the AVERAGE height at that weight (not individual predictions)
## PI 是 percentile interval(百分位区间) 其他和上一步一样
## "体重是z时,平均身高大概在什么范围"
mu_ci <- sapply(weight_seq, function(z) {
  PI(post$alpha + post$beta * (z - xbar), prob = 0.89)
})
head(mu_ci)

## for each weight value, simulate 10000 individual height predictions
## (adding sigma to capture person-to-person variation), then get the
## 89% percentile interval of those simulated heights
## "体重是z时,某一个具体的人身高大概在什么范围"
pred_ci <- sapply(weight_seq, function(z) {
  PI(rnorm(1e4, post$alpha + post$beta * (z - xbar), post$sigma),
     prob = 0.89)
})
head(pred_ci)


## 画原始数据散点图:体重(x轴) vs 身高(y轴)
## 半透明的slateblue颜色圆点,点的大小设为0.5
plot(height ~ weight, data = d2,
     col = col.alpha("slateblue", 0.5), cex = 0.5)
## 画出mu的平均回归线(50个体重点连成的实线)
lines(weight_seq, mu)

## 画mu的89%区间上下界(窄的虚线,只反映mu本身的不确定性)
lines(weight_seq, mu_ci[1, ], lty = 2)
lines(weight_seq, mu_ci[2, ], lty = 2)

## 画预测区间的89%上下界(宽的虚线,包含了sigma带来的个体差异
lines(weight_seq, pred_ci[1, ], lty = 2)
lines(weight_seq, pred_ci[2, ], lty = 2)

## 紫色散点	真实观测数据(d2里86个/352个个体的真实体重-身高)
##中间实线	模型算出的mu——"给定体重z，期望的平均身高是多少"
##内层虚线	mu的89%区间——"这条平均线本身有多大不确定性"
##外层虚线	预测区间——"给定体重z，一个具体的人身高会落在哪个范围"(包含sigma的个体差异)
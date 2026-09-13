library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18, ]
weight <- d2$weight
height <- d2$height
xbar <- mean(d2$weight)
xbar
sd_x <- sd(d2$weight)
sd_x
lines_x <- (d2$weight - xbar) / sd_x
##  lines_x
max(d2$weight) - min(d2$weight)

ybar <- mean(d2$height)
ybar
sd_y <- sd(d2$height)
sd_y
lines_y <- (d2$height - xbar) / sd_y
## lines_y
max(d2$height) - min(d2$height)

## Modle
m1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- alpha + beta * (weight - xbar),
    alpha ~ dnorm(154.5971, 7.742332),
    beta ~ dnorm(1.33, 0.5),
    sigma ~ dexp(1)
  ),
  data = list(height = height, weight = weight, lines_x = lines_x)
)
precis(m1)


## WHEN weightX = 43.72, hight =?
post <- extract.samples(m1)
y <- rnorm(1e4, post$alpha + post$beta * (43.72 - xbar), post$sigma)
mean(y)
PI(y, prob = 0.89)

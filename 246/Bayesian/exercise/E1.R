temp  <- c(15, 20, 25, 30, 35)
sales <- c(10, 25, 40, 55, 68)

xbar <- mean(temp)
xbar
sd_x <- sd(temp)
sd_x
lines_x <- (temp) / sd_x
lines_x

ybar <- mean(sales)
ybar
sd_y <- sd(sales)
sd_y
lines_y <- (sales) / sd_y
lines_y

m1 <- quap(
  alist(
    sales ~ dnorm(mu, sigma),
    mu <- alpha + beta * (temp - xbar),
    alpha ~ dnorm(40, 23),
    beta ~ dlnorm(log(3), 0.5),
    sigma ~ dexp(1)
  ),
  data = list(temp = temp, sales = sales, xbar = xbar)
)
precis(m1)
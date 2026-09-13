lines_changed <- c(50, 120, 30, 200, 80)
xbar <- mean(lines_changed)
xbar
sd_x <- sd(lines_changed)
sd_x
lines_x <- (lines_changed - xbar) / sd_x
lines_x


review_time <- c(15, 30, 10, 45, 20)
ybar <- mean(review_time)
ybar
sd_y <- sd(review_time)
sd_y

m1 <- quap(
  alist(
    review_time ~ dnorm(mu, sigma),
    mu <- alpha + beta * lines_z,
    alpha ~ dnorm(24, 14),
    beta  ~ dnorm(14, 7),
    sigma ~ dexp(1)
  ),
  data = list(review_time = review_time, lines_z = lines_z)
)
precis(m1)
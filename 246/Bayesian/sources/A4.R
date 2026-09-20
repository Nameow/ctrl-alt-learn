library(rethinking)
data <- read.csv("data.csv")

misunderstandings <- data$misunderstandings
xbar <- mean(misunderstandings)
# xbar
common <- chainmode(misunderstandings)
# common
log(xbar)
range(data$misunderstandings)

complexity <- data$complexity
sd_c <- sd(complexity)
sd_c
mean_c <- mean(complexity)
mean_c
lines_c <- standardize(complexity)
# lines_c

req_quality <- data$req_quality
sd_rq <- sd(req_quality)
sd_rq
mean_rq <- mean(req_quality)
mean_rq
lines_rq <- standardize(req_quality)

dat_list <- list(M = misunderstandings, RQ = lines_rq, C = lines_c)

m1 <- ulam(
  alist(
    M ~ dpois(lambda),
    log(lambda) <- alpha + beta * C + zeta * RQ,
    alpha ~ dnorm(1, 0.5),
    beta ~ dnorm(0, 0.2),
    zeta ~ dnorm(0, 0.2)
  ),
  data = dat_list,
  chains = 4,
  cores = 4
)
precis(m1)

post <- extract.samples(m1)
post_complexity <- mean(post$beta)
effect_complexity <- exp(post_complexity)   # complexity 效应
effect_complexity

post_req_quality <- mean(post$zeta)
effect_req_quality <- exp(post_req_quality) # req_quality 效应
effect_req_quality

library(rethinking)
data <- read.csv("data.csv")

misunderstandings <- data$misunderstandings
xbar <- mean(misunderstandings)
log(xbar)
range(data$misunderstandings)

complexity <- data$complexity
lines_c <- standardize(complexity)
lines_c

req_quality <- data$req_quality
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
traceplot(m1)

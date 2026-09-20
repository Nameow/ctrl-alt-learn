library(rethinking)
data <- read.csv("data.csv")

comm_volume <- data$comm_volume
lines_cv <- standardize(comm_volume)
range(lines_cv)
length(comm_volume)

misunderstandings <- data$misunderstandings
lines_m <- standardize(misunderstandings)
range(lines_m)
length(misunderstandings)


n <- 100
prior_alpha <- rnorm(n, 0, 0.2)
prior_beta  <- rnorm(n, 0, 0.5)
prior_zeta  <- rnorm(n, 0, 0.5)

mu_sim <- prior_alpha + prior_beta * lines_c + prior_zeta * lines_rq


n <- 100
prior_alpha <- rnorm(n, 1, 0.5)
prior_beta  <- rnorm(n, 0, 0.2)
prior_zeta  <- rnorm(n, 0, 0.2)

lambda_sim <- exp(prior_alpha + prior_beta * lines_c + prior_zeta * lines_rq)

# (1, 0.5) (0, 0.2) 
#       1%       50%       99% 
# 0.8258387 2.6543361 7.9089703 
# (1, 0.4) (0, 0.2) 11111111111111111111111
# 0.8736056 2.6765069 8.4021455 
# (1, 0.3) (0, 0.2) 
# 1.005750 2.627858 7.432938
# (1, 0.2) (0, 0.2) 
# 1.167374 2.669487 6.549818 
# (1, 0.1) (0, 0.2) 
# 1.207093 2.711391 6.405489 

# (1, 0.6) (0, 0.2)
# 0.5493548 2.3438628 9.7560176
# (1, 0.7) (0, 0.2) XXXXXXXXXXXX
# 0.552718  3.343263 14.367799 

##  2.632798 to  2.709302, 0-9
# (1, 0.4) (0, 0.15) (0, 0.2)
# 0.8522357 2.6026670 9.3486176 
# (1, 0.4) (0, 0.15) (0, 0.15)
# 0.8759581 2.7272351 6.7778110 

# (1, 0.4) (0, 0.15) (0, 0.25)
# 0.6571875 2.6544708 8.2623693 
# (1, 0.4) (0, 0.25) (0, 0.15)
# 0.9236662 2.8227118 8.6435540 
# (1, 0.4) (0, 0.3) (0, 0.1)XXXXXX
# 0.8640219 2.8287168 9.7569811 
# (1, 0.4) (0, 0.1) (0, 0.3) XXXXXX
# 0.8767123 2.9120716 8.0239597 

n <- 100
prior_alpha <- rnorm(n, 1, 0.4)
prior_beta2  <- rnorm(n, 0, 0.1)
prior_zeta2  <- rnorm(n, 0, 0.3)

lambda_sim <- matrix(NA, nrow = n, ncol = length(lines_c))
for (i in 1:n) {
  eta <- prior_alpha[i] + prior_beta2[i] * lines_c + prior_zeta2[i] * lines_rq
  lambda_sim[i, ] <- exp(eta)
}
quantile(lambda_sim, c(0.01, 0.5, 0.99))
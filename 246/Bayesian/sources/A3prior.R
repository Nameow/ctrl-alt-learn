library(rethinking)
data <- read.csv("data.csv")

comm_volume <- data$comm_volume
lines_cv <- standardize(comm_volume)
range(lines_cv)

misunderstandings <- data$misunderstandings
lines_m <- standardize(misunderstandings)
range(lines_m)


n <- 100
prior_alpha <- rnorm(n, 0, 0.2)
prior_beta  <- rnorm(n, 0, 0.5)
prior_zeta  <- rnorm(n, 0, 0.5)

mu_sim <- prior_alpha + prior_beta * lines_c + prior_zeta * lines_rq


n <- 100
prior_alpha <- rnorm(n, 1, 0.5)
prior_beta  <- rnorm(n, 0, 0.5)
prior_zeta  <- rnorm(n, 0, 0.5)

lambda_sim <- exp(prior_alpha + prior_beta * lines_c + prior_zeta * lines_rq)
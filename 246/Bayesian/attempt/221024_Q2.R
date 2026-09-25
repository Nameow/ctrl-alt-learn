# complete pooling, grand mean

# no pooling, fixed effect

# partial pooling
m3 <- quap(
  alist(
    y ~ dnorm(lambda, phi),
    log(lambda) <- alpha[group_id],
    alpha[group_id] ~ dnorm(alpha_bar, sigma),
    alpha_bar  ~ dnorm(0, 0.5),
    sigma ~ dexp(1),
    phi ~ dexp(1),
  ),
  data = list(review_time = review_time, lines_z = lines_z)
)
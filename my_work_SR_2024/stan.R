library(rethinking)
data("Wines2012")


d <- Wines2012


dat <- list(
  S=standardize(d$score),
  J=as.numeric(d$judge),
  W=as.numeric(d$wine),
  X=ifelse(d$wine.amer==1,1,2),
  Z=ifelse(d$judge.amer==1,1,2)
)




mQ <- ulam(
  alist(
    S ~ dnorm(mu,sigma),
    mu <- Q[W],
    Q[W]~dnorm(0,1),
    sigma ~ dexp(1)
  ), data = dat,chains = 4,cores = 4,cmdstan = FALSE
)

precis(mQ,2)


library(cmdstanr)


check_cmdstan_toolchain()

cmdstan_version()

file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(file)


install.packages("brms")
library(brms)


fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
            data = epilepsy, family = poisson())

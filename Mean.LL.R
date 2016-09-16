rm(list=ls())
cat("\014")

# log-likelihood for normal
ll.fn.norm <- function(mu, y) {
  -sum((y - mu)^2)
}


# function to estimate normal model
est.norm <- function(y) {
  est <- optim(par = 0, fn = ll.fn.norm, y = y,
               control = list(fnscale = -1),
               method = "Brent",  # for 1d problems
               lower = -100, upper = 100)
  if (est$convergence != 0) print("Model did not converge!")
  res <- list(est = est$par)
  return(res)
}

# data
y <- c(1.2, 3, 4.2)
# estimate mean
m1 <- est.norm(y)
m1

log_odds <- c(0.352,-0.435,-0.8,-0.099)
intercept <- c(-0.086,-0.086,0.176,0.176)
odds <- log_odds * intercept
probability <- odds / (1+odds); probability
library(VGAM)
library(stargazer)
mod_1a <- 1- 0.352
mod_1b <- -0.435
mod_2a <- -0.0800
mod_2b <- -0.988
mod_3a <- 0.306
mod_3b <- -0.267
mod_3c <- -0.489
mod_4a <- -0.689
mod_4b <- -0.921
mod_4c <- -0.913
mod <- c(mod_1a,mod_1b,mod_2a,mod_2b,mod_3a,mod_3b,mod_3c,mod_4a,mod_4b,mod_4c)
prob <- probit(mod,inverse=T); prob; 1-prob
stargazer(prob, type = "text")

calc_prob <- function(x){
  mod_const <-
  mod_coef <-
  prob <- probit(mod_coef,inverse=T)
  return(prob)
}

square.it <- function(x) {
  square <- x * x
  return(square)
}
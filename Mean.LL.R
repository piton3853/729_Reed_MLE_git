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

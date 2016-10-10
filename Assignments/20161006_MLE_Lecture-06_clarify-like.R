library(arm)
 
n <- 1000
educate <- rnorm(n)
age <- rnorm(n)
income<-rnorm(n)
X <- cbind(1,educate,age,income)
b <- c(1,1,-2,2)
p <- plogis(X%*%b)
vote <- rbinom(n, 1, p)
# estimate simple logit model
m <- glm(vote ~ educate + age + income, family = binomial)
# display results
display(m, detail = TRUE)

# set the medians
x.edu <- median(educate)  # in years
x.age <- median(age)  # in years
x.inc <- median(income)  # in 10,000s of dollars

# set beta.hat, Sigma.hat, and X.c
beta.hat <- coef(m)
Sigma.hat <- vcov(m)
X.c <- c(1, x.edu, x.age, x.inc)

# simulate p.tilde
library(MASS)  # mvrnorm()
n.sims <- 1000  # set number of simulations
p.tilde <- numeric(n.sims)  # create holder for simulations
for (i in 1:n.sims) {
  beta.tilde <- mvrnorm(1, beta.hat, Sigma.hat)
  p.tilde[i] <- plogis(X.c%*%beta.tilde) 
}

# characterize the simulations
mean(p.tilde)
sd(p.tilde)
quantile(p.tilde, c(0.05, 0.95))


vote<-data.frame(vote,educate,age,income)

mod <- obsval(vote~educate+age+income,data=vote,
              reg.model = "logit",
              n.draws = 1000,
              effect.var = "age",
              effect.vals = c(0,2), # lowest to mid
              verbose = TRUE)

# display model results
summary(mod$model)
summary(mod$preds)
# see the names of everything obsval returns
# names(mod)


vioplot(mod$sim.coef[,1], mod$sim.coef[,2], mod$sim.coef[,3], 
        mod$sim.coefs[,4], names=c(expression(beta[0]),expression(beta[age]),
                        expression(beta[educate]),expression(beta[income])), 
        col="red")
title("Coefficients")



vioplot(mod$preds[,1], mod$preds[,2], names=c("age=0","age=2"), 
        col="red")
title("Predicted Probability")
mean(mod$preds[, 1] - mod$preds[, 2])
quantile(mod$preds[, 1] - mod$preds[, 2], c(0.025, 0.975))







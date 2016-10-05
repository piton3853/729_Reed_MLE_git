#thanks to Chris Meserole for the code and the obsval package. 

rm(list=ls())
cat("\014")
install.packages("devtools")
library("devtools")
devtools::install_github("chrismeserole/obsval")

library(foreign)
library(mvtnorm)
library(obsval)



#########0. reading the data----
hk <- read.dta('~/Documents/GitHubRepo/729_Reed_MLE_git/Assignments/hk1.dta')



# run probit, show results
(model <- glm('bushvote~retecon+partyid+bushiraq+ideol+white+
              female+age+educ1_7+income', 
              family=binomial(link="probit"),
              data=hk))

summary(model)

# generate predicted probabilities automatically
hk$pprob <- predict(model,type="response")

# generate predicted probabilities manually
hk$pprob_manual <- pnorm(model$coef['(Intercept)'] + 
                           model$coef['retecon']*hk$retecon + 
                           model$coef['partyid']*hk$partyid + 
                           model$coef['bushiraq']*hk$bushiraq + 
                           model$coef['ideol']*hk$ideol + 
                           model$coef['white']*hk$white + 
                           model$coef['female']*hk$female + 
                           model$coef['age']*hk$age + 
                           model$coef['educ1_7']*hk$educ1_7 + 
                           model$coef['income']*hk$income)




# test that we did it right
hk$pprob_test <- hk$pprob - hk$pprob_manual
summary(hk$pprob_test) # should be zeros

#######1. calculate average effect of nwstate using observed values----
hk$pprob_retecon_neg1 <- pnorm(model$coef['(Intercept)'] + 
                                 model$coef['retecon']*-1 + 
                                 model$coef['partyid']*hk$partyid + 
                                 model$coef['bushiraq']*hk$bushiraq + 
                                 model$coef['ideol']*hk$ideol + 
                                 model$coef['white']*hk$white + 
                                 model$coef['female']*hk$female + 
                                 model$coef['age']*hk$age + 
                                 model$coef['educ1_7']*hk$educ1_7 + 
                                 model$coef['income']*hk$income)

hk$pprob_retecon_0 <- pnorm(model$coef['(Intercept)'] + 
                              model$coef['retecon']*0 + 
                              model$coef['partyid']*hk$partyid + 
                              model$coef['bushiraq']*hk$bushiraq + 
                              model$coef['ideol']*hk$ideol + 
                              model$coef['white']*hk$white + 
                              model$coef['female']*hk$female + 
                              model$coef['age']*hk$age + 
                              model$coef['educ1_7']*hk$educ1_7 + 
                              model$coef['income']*hk$income)

hk$pprob_effect <- hk$pprob_retecon_0 - hk$pprob_retecon_neg1
summary(hk$pprob_effect)		


## 
##	NOW LET'S GET CONFIDENCE INTERVALS FOR OUR EFFECT, USING OBSERVED VALUES
## 	note: below are three ways to do this
##

##
##########2. Iterative for loops (ie, slow way)----
## 

n.draws <- 2
set.seed(123)
sim.coefs <- rmvnorm(n.draws, model$coef, vcov(model)) 
sim.coefs
p.effect.mean <- numeric(n.draws)
p.high.mean <- numeric(n.draws)
p.low.mean	<- numeric(n.draws)
p.baseline 	<- numeric(n.draws)

n.obs <- length(hk[[1]])

for(i in 1:n.draws){ 
  
  # For the current set of coefficients, calculate a
  # latent probability for all observations using observed values
  
  # first, set up vectors to store our linear predictors
  Xb.baseline 	<- numeric(n.obs)
  Xb.high 	<- numeric(n.obs)
  Xb.low	 	<- numeric(n.obs)
  
  # second, for current set of coefs, loop through each observation
  # and calculate mean, high and low linear predictors
  for(j in 1:n.obs){
    Xb.baseline[j] <- sim.coefs[i,1] +
      sim.coefs[i,2]*hk$retecon[j] + 
      sim.coefs[i,3]*hk$partyid[j] + 
      sim.coefs[i,4]*hk$bushiraq[j] + 
      sim.coefs[i,5]*hk$ideol[j] + 
      sim.coefs[i,6]*hk$white[j] +       
      sim.coefs[i,7]*hk$female[j] +       
      sim.coefs[i,8]*hk$age[j] +     
      sim.coefs[i,9]*hk$educ1_7[j] + 
      sim.coefs[i,10]*hk$income[j] 
    
    
    Xb.high[j] <- sim.coefs[i,1] + 
      sim.coefs[i,2]*0 + 
      sim.coefs[i,3]*hk$partyid[j] + 
      sim.coefs[i,4]*hk$bushiraq[j] + 
      sim.coefs[i,5]*hk$ideol[j] + 
      sim.coefs[i,6]*hk$white[j] +       
      sim.coefs[i,7]*hk$female[j] +       
      sim.coefs[i,8]*hk$age[j] +     
      sim.coefs[i,9]*hk$educ1_7[j] + 
      sim.coefs[i,10]*hk$income[j]
    
    Xb.low[j] <- sim.coefs[i,1] + 
      sim.coefs[i,2]*-1 + 
      sim.coefs[i,3]*hk$partyid[j] + 
      sim.coefs[i,4]*hk$bushiraq[j] + 
      sim.coefs[i,5]*hk$ideol[j] + 
      sim.coefs[i,6]*hk$white[j] +       
      sim.coefs[i,7]*hk$female[j] +       
      sim.coefs[i,8]*hk$age[j] +     
      sim.coefs[i,9]*hk$educ1_7[j] + 
      sim.coefs[i,10]*hk$income[j]    
  }
  
  # third, transform linear predictors into probabilites
  predict.baseline	<- pnorm(Xb.baseline)
  predict.high	<- pnorm(Xb.high)
  predict.low 	<- pnorm(Xb.low)
  predict.effect  <- predict.high - predict.low
  
  # fourth, for current set of coefs, store the average mean, high and 
  # low probability across all observations in the data set 
  p.baseline[i]		<-mean(predict.baseline, na.rm=TRUE)
  p.high.mean[i]	<- mean(predict.high, na.rm=TRUE)
  p.low.mean[i]	<- mean(predict.low, na.rm=TRUE)
  p.effect.mean[i] <- mean(predict.effect,na.rm=TRUE) 
  
}

summary(p.effect.mean)
quantile(p.effect.mean, c(.025,.975))


##
#3. Vectorized way (ie, fast way)----
##

n.draws <- 1000
set.seed(123)
sim.coefs <- rmvnorm(n.draws, model$coef, vcov(model)) 

p.effect.mean <- numeric(n.draws)
p.high.mean <- numeric(n.draws)
p.low.mean	<- numeric(n.draws)
p.baseline 	<- numeric(n.draws)


# create the X matrices for linear predictor Xb (replace appropriate column with high/low values of effect variable)

X.matrix.baseline <- model.matrix(model, model$model)

head(X.matrix.baseline)
X.matrix.high <- X.matrix.low <- X.matrix.baseline
X.matrix.high[, 2] <- 0
X.matrix.low[, 2] <- -1

n.obs <- length(hk[[1]])

# loop through each set of coefficients
for(i in 1:n.draws){ 
  
  # For the current set of coefficients, calculate a
  # latent probability for all observations using observed values
  
  # first, set up vectors to store our linear predictors
  Xb.baseline 	<- numeric(n.obs)
  Xb.high 	<- numeric(n.obs)
  Xb.low	 	<- numeric(n.obs)
  
  # second, for current set of coefs (ie, 'b'), do matrix multiplication
  # with predictor matrices (ie, 'X')
  Xb.baseline <- X.matrix.baseline %*% sim.coefs[i,]
  Xb.high <- X.matrix.high %*% sim.coefs[i,]
  Xb.low <- X.matrix.low %*% sim.coefs[i,]
  
  # third, transform linear predictors into probabilites
  predict.baseline	<- pnorm(Xb.high)
  predict.high	<- pnorm(Xb.high)
  predict.low 	<- pnorm(Xb.low)
  predict.effect  <- predict.high - predict.low
  
  # fourth, for current set of coefs, store the average mean, high and 
  # low probability across all observations in the data set 
  p.baseline[i]		<-mean(predict.baseline, na.rm=TRUE)
  p.high.mean[i]	<- mean(predict.high, na.rm=TRUE)
  p.low.mean[i]	<- mean(predict.low, na.rm=TRUE)
  p.effect.mean[i] <- mean(predict.effect,na.rm=TRUE) 
  
}

mean(p.effect.mean)
quantile(p.effect.mean, c(.025,.975))



#4. FUNCTIONAL WAY----

mod <- obsval(bushvote~retecon+partyid+bushiraq+ideol+white+
                female+age+educ1_7+income,
              data = hk, 
              reg.model = "probit",
              n.draws = 1000,
              effect.var = "retecon", 
              effect.vals = c(-1,0), # lowest to mid
              verbose = TRUE)

# display model results
summary(mod$model)

# get mean effect of 'retecon'
mean(mod$effect.preds) 
mod$effect.mean
mean(mod$preds[, 2] - mod$preds[, 1])
names(mod)

head(mod$sim.coefs)

mod$effect.high.ci
mod$effect.low.ci
quantile(mod$effect.preds, c(0.025, 0.975))


##
#5.multiple values0----
##
mod <- obsval(bushvote~retecon+partyid+bushiraq+ideol+white+
                female+age+educ1_7+income,
              data = hk,
              reg.model = "probit",
              n.draws = 1000,
              effect.var = "retecon",
              effect.vals = c(-1,0,1), # lowest to mid
              verbose = TRUE)

# display model results
summary(mod$model)

# get mean effect of 'retecon'
mean(mod$effect.preds)
mod$effect.mean
mean(mod$preds[, 2] - mod$preds[, 1])
quantile(mod$preds[, 2] - mod$preds[, 1], c(0.025, 0.975))

mean(mod$preds[, 3] - mod$preds[, 1])
quantile(mod$preds[, 3] - mod$preds[, 1], c(0.025, 0.975))

# see the names of everything obsval returns
names(mod)

# verify that the sim.coefs are what they should be
head(mod$sim.coefs)


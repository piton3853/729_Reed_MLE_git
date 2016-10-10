library(lmtest)
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
m0<-glm(vote~1, family=binomial)
# display results
display(m, detail = TRUE)
m

#Deviances
nd<-2*(0 - logLik(m0))
nd
rd<-2*(0-logLik(m))
rd
nd-rd


#Akaike’s Information Criterion
length(coef(m))
AIC<- -2*(logLik(m))+2*(length(coef(m)))
AIC
#Bayesian Information Criterion
BIC<- -2*logLik(m)+log(n)*(length(coef(m))+1)
BIC

#Likelihood Ratio Tests
logLik(m0)
logLik(m)
lr.test = -2*(logLik(m0) - logLik(m))
lr.test
lrtest(m0,m)

stargazer(m0,m,type='text')



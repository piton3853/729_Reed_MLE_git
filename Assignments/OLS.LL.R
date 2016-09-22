rm(list=ls())
cat("\014")

X <- cbind(1,gdppcap08)
y <- votevap

theta.true<-c(2,3,1)
y<-X%*%theta.true[1:2] + rnorm(100)

ols.lf<-function(theta,y,X){
  n<-nrow(X)
  k<-ncol(X)
  beta<-theta[1:k]
  sigma2<-theta[k+1]
  e<-y-X%*%beta
  logl<- -.5*n*log(2*pi)-.5*n*log(sigma2)-
    ((t(e)%*%e)/(2*sigma2))
  return(-logl)
}

p<-optim(c(1,1,1),ols.lf,method="BFGS",hessian=T,y=y,X=X)
p


Fisher<-solve(p$hessian)
Fisher
sqrt(diag(Fisher))



summary(lm(y~X))

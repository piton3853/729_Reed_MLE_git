#Things go bad

x<-c(0,0,0,0,0,1,1,1,1,0)
y<-c(1,0,0,0,0,0,0,0,1,1)
z<- rnorm(10)

m1<-glm(y~x+z,family=binomial)

display(m1, detail = TRUE)

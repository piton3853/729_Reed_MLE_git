#Things go bad

x<-c(0,0,0,0,0,1,1,1,1,1)
y<-c(0,0,0,0,0,0,0,0,1,1)


m1<-glm(y~x+z,family=binomial)

display(m1, detail = TRUE)

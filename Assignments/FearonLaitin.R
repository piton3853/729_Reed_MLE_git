rm(list=ls())
cat("\014")
library(dplyr)
library(ggplot2)
library(tidyr)
library(foreign)
library(LOGIT)
library(stargazer)
setwd("/Users/Nick/Documents/GitHubRepo/729_Reed_MLE_git/Assignments")
#repdata <- read.dta("repdata.dta")
#save(repdata, file = "repdata.RData")
load("repdata.RData")
View(repdata)
summary(repdata$onset)
repdata <- repdata %>%
  filter(onset<4)
model.1 <- glm(onset~gdpen+gdpenl+lpop+lmtnest+ncontig+
                     Oil+nwstate+instab+polity2l+ethfrac+relfrac,
                     family = binomial(link = "logit"),
                     data = repdata); summary(model.1)
df.gdpen0 <- repdata
df.gdpen0$gdpen <- 0
repdata$gdpen0 <- predict(model.1,
                         type = "response",
                         newdata = df.gdpen0); summary(repdata$gdpen0)
df.gdpen1 <- repdata
df.gdpen1$gdpen <- 1
repdata$gdpen1 <- predict(model.1,
                         type = "response",
                         newdata = df.gdpen1); summary(repdata$gdpen1)
repdata$difference <- repdata$gdpen1 - repdata$gdpen0
mean(repdata$difference, na.rm = T)
summary(repdata$gdpen, na.rm = T)
sd(repdata$gdpen, na.rm = T)

model.1l <- glm(onset~gdpen+gdpenl+lpop+lmtnest+ncontig+
                 Oil+nwstate+instab+polity2l+ethfrac+relfrac,
               family = binomial(link = "logit"),
               data = repdata); summary(model.1l)
df.gdpen0l <- repdata
df.gdpen0l$gdpenl <- 1.38255
repdata$gdpen0l <- predict(model.1l,
                          type = "response",
                          newdata = df.gdpen0l); summary(repdata$gdpen0l)
df.gdpen1l <- repdata
df.gdpen1l$gdpenl <- 5.91945
repdata$gdpen1l <- predict(model.1l,
                          type = "response",
                          newdata = df.gdpen1l); summary(repdata$gdpen1l)
repdata$differencel <- repdata$gdpen1l - repdata$gdpen0l
mean(repdata$differencel, na.rm = T)
summary(repdata$gdpenl, na.rm = T)
sd(repdata$gdpenl, na.rm = T)
stargazer(model.1,model.1l, type = "text")

a <- 4.5369/2
m <- 3.651
m+a
m-a

model.3 <- glm(onset~gdpen+gdpenl+lpop+lmtnest+
                 ncontig+Oil+nwstate+instab+anocl+deml+
                 ethfrac+relfrac,
               family = binomial(link="logit"),
               data = repdata); summary(model.3)

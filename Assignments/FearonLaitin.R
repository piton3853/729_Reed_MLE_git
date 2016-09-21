rm(list=ls())
cat("\014")
library(dplyr)
library(ggplot2)
library(tidyr)
library(foreign)
setwd("/Users/Nick/Documents/GitHubRepo/729_Reed_MLE_git/Assignments")
#repdata <- read.dta("repdata.dta")
#save(repdata, file = "repdata.RData")
repdata <- load("repdata.RData")

model.logit <- glm(onset~warl+gdpenl+lpopl+lmtnest+ncontig+
                     Oil+nwstate+instab+polity2l+ethfrac+relfrac,
                   data = repdata,
                   binomial(link = ""))
View(repdata)
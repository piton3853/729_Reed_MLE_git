#RAND Data Analysis - Paths to Victory; Date of Analysis (begin) 3/28/2016
#Nick Thompson; R-Script
############
# Preamble #
############
rm(list=ls())
cat("\014")
### RESTART R CODE: ###
.rs.restartR()
savehistory(file="fearon_repdata.Rhistory")
if(T){
  setwd("~/Documents/data/15-622_Data+Sets/repdata")
  library(dplyr)
  library(ggplot2)
  library(gmodels)
  library(stargazer)
  library(data.table)
  library(readstata13)
}
if(T){
  fearon_repdata <- read.dta13("repdata_original.dta", nonint.factors=T)
  save(fearon_repdata, file = "fearon_repdata.RDdata")
  load("fearon_repdata.RDdata")
}
str(fearon_repdata)

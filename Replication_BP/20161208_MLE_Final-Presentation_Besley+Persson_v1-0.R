###############################
#####     Nick Thompson   #####
#####     12/08/2016      #####
#####  Final Presentation #####
#####     Replication     #####
#####   Besley &  Persson #####
##  Repression or Civil War? ##
###############################

# SOURCE:  Besley & Persson (2009);
# DATASET Source:  https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UN54HD

###############################
#####       PREAMBLE      #####
###############################
# Clear & Reset
if(T){rm(list=ls()); cat("\014")}

###############################
#####       PACKAGES      #####
if(F){
  install.packages("pacman", repos='http://cran.us.r-project.org')
  pacman::p_load("foreign", "stargazer", "dplyr", "ggplot2", "devtools",
                 "obsval","mvtnorm","arm","vioplot","lmtest","knitr","readstata13",
                 "moments","MASS","XLConnect","xlsx","plm","apsrtable","repmis","gridExtra")
}                #####
###############################

###############################
#####       LIBRARIES     #####
if(T){ 
# devtools::install_github("chrismeserole/obsval")
library(foreign)
library(ggplot2)
library(dplyr)
library(stargazer)
library(devtools)
library(obsval)
library(mvtnorm)
library(arm)
library(vioplot)
library(lmtest)
library(knitr)
library(readstata13)
library(moments)
library(MASS)
}
###############################

###############################
#####         DATA        #####
if(T){
  setwd("~/Documents/GitHubRepo/729_Reed_MLE_git/Replication_besley_persson")
  repcw01 <- read.table(file = "BP.formatted.tab", header = TRUE, sep = "\t")
  save(repcw01, file = "repcw01.RData")
  source_data("https://github.com/piton3853/2016F_888C_Final-Replication/blob/master/rossdata01.RData?raw=true")
  #write.xlsx(rossdata01, "~/Documents/GitHubRepo/2016F_888C_Final-Replication/ross/Replication data for The Oil Curse - Ross 2012.xlsx")
  #View(rossdata01)
}
###############################

###############################
#####   BUILD DATAFRAMES  #####
###############################



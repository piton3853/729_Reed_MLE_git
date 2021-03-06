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

################################################
###             PREAMBLE                     ###
################################################
if(T){                            # This collapses the PREAMBLE
# Clear & Reset
# Clear Environment
rm(list=ls())
# Clear Console
cat("\014")

###############################
#####       PACKAGES      #####
if(F){
  install.packages("pacman", repos='http://cran.us.r-project.org')
  pacman::p_load("foreign", "stargazer", "dplyr", "ggplot2", "devtools",
                 "obsval","mvtnorm","arm","vioplot","lmtest","knitr","readstata13",
                 "moments","MASS","XLConnect","xlsx","plm","apsrtable","repmis","gridExtra","Hmsic","RCurl","Zelig")
}                #####
###############################

###############################
#####       LIBRARIES     #####
if(T){ 
# devtools::install_github("chrismeserole/obsval")
library(foreign)      # Load Stata, .csv, .tbl
library(stargazer)    # Nice LaTeX Table Outputs
library(dplyr)        # data management
library(plyr)         # data management
library(ggplot2)      # nice graphs and plots
library(devtools)     #
library(obsval)       #
library(mvtnorm)      #
library(arm)          #
library(vioplot)      # violin plots
library(lmtest)       #
library(knitr)        # knit files
library(readstata13)  # read Stata 2013
library(moments)      #
library(MASS)         #
library(XLConnect)    #
library(xlsx)         # write .xlsx file
library(plm)          # create time series fixed effects models
library(apsrtable)    # Nice LaTeX Table Outputs
library(repmis)       # this is needed to read the .RData file from GitHub
library(gridExtra)    # this is the package Joel uses for putting two plots together
library(lmtest)       # this is the package Joel uses for the function he wrote
library(sandwich)     # this is the package Joel uses for the function he wrote
library(Hmisc)
library(RCurl)
library(Zelig)
}
###############################

###############################
#####         DATA        #####
if(T){
  setwd("~/Documents/GitHubRepo/729_Reed_MLE_git/Replication_BP")
  source_data("https://github.com/piton3853/729_Reed_MLE_git/raw/master/Replication_BP/repcw01.RData")
  source_data("https://github.com/piton3853/729_Reed_MLE_git/raw/master/Replication_BP/repcw_years.RData")
  #repcw_years <- read.table("bp_exact_for_analysis.tab",sep = "\t",header=T)
  #save(repcw_years,file = "repcw_years.RData")
}
###############################
}                             # This is the end of the PREAMBLE

###########################
###  BUILD DATAFRAMES   ###
###########################
# Create named factors for DV
#repcw01$rep_civwar_DV <- as.factor(repcw01$rep_civwar_DV)
#str(repcw01$rep_civwar_DV)
#str(repcw01$p_r_cw)


############################
###  MODEL REPLICATION   ###
############################
if(T){                    # This collapses the Model Replication Section
#repcw_years$rep_civwar_DV <- ordered(as.factor(repcw_years$rep_civwar_DV))
#repcw_years <- repcw_years %>%  
#  dplyr::mutate(p_r_cw = plyr::revalue(rep_civwar_DV, c("0" = "Peace","1" = "Repression","2" = "Civil War")))
#repcw_years$p_r_cw <- ordered(as.factor(repcw_years$p_r_cw))
#save(repcw_years, file = "repcw_years.RData")
repcw_years <- na.omit(repcw_years)
model.repCivWar.01 <- MASS::polr(rep_civwar_DV ~ logGDPpc + parliament + major_oil + major_primary + disaster,
             data = repcw_years,
             Hess=TRUE, 
             method="logistic")


summary(model.repCivWar.01)

coef <- c(model.repCivWar.01$coefficients,model.repCivWar.01$zeta)
se <- sqrt(diag(vcov(model.repCivWar.01)))
z <- coef/se
p <- 2*(1-pnorm(abs(z)))
odds01 <- exp(model.repCivWar.01$coefficients); odds01
xtable(cbind(coef,se,z,p),digits=4)
}                     # This is the end of the MODEL REPLICATION Section

############################
###    LOOK @ THE DATA   ###
############################
if(T){                      # This collapses the LOOK @ THE DATA Section
if(F){
ggplot(repcw_years, aes(x=logGDPpc, fill=p_r_cw)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")
} # this is a histogram plot

# View Data
cdplot(repcw_years$p_r_cw~repcw_years$logGDPpc,ylab = "Peace, Repression, & CivilWar",xlab = "logGDP")

hist(repcw_years02$disaster)
}                       # This is the end of the LOOK @ THE DATA Section

############################
###    OBSERVED VALUES   ###
############################

# Show the effect of moving `logGDPpc` from its minimum to maximum 
# value on the probability of observing the four categories of the 
# dependent variable, `p_r_cw` (Peace, Repression, or Civil War).  
# Use the observed value approach and violin plots to show the effect of `logGDPpc`.

# observed values for ordered logit -----------------------------------------
beta <- c(model.repCivWar.01$coefficients, model.repCivWar.01$zeta); head(beta)
model.repCivWar.01_vcov <- vcov(model.repCivWar.01)
n.draws <- 500
set.seed(42)
sim.coefs <- rmvnorm(n.draws, beta,model.repCivWar.01_vcov); head(sim.coefs)

#####reorder the columns - so the simulated coefficients are in the same order as the model#
sim.coefs <- sim.coefs[, c(names(model.repCivWar.01$zeta),names(model.repCivWar.01$coefficients))];head(sim.coefs)

p.1 <- numeric(n.draws)
p.2 <- numeric(n.draws)
p.3 <- numeric(n.draws)

n.obs <- length(repcw_years[[1]])


# Latent probability loop -
for(i in 1:n.draws){ 
  
  # For the current set of coefficients, calculate a
  # latent probability for all observations using observed values
  
  # first, set up vectors to store our linear predictors
  Xb.1 	<- numeric(n.obs)           # Cut Point 1
  Xb.2 	<- numeric(n.obs)           # Cut Point 2



# Observed Values for loop
  # second, for current set of coefs, loop through each observation
  # and calculate mean, high and low linear predictors
  for(j in 1:n.obs){
    ###reordered columns are now numbered consitently here ###
    ###   1   ###
    Xb.1[j] <-  sim.coefs[i,1]*1 +                               # Cutpoint 6   Identify these from sim.coefs
                sim.coefs[i,2]*0 -                               # Cutpoint 7   Identify these from sim.coefs
                  (sim.coefs[i,3]*as.numeric(repcw_years$logGDPpc[j]) +                              # 3
                    sim.coefs[i,4]*as.numeric(repcw_years$parliament[j]) +    # 4
                    sim.coefs[i,5]*as.numeric(repcw_years$major_oil[j]) +     # 5
                    sim.coefs[i,6]*as.numeric(repcw_years$major_primary[j]) + # 6
                    sim.coefs[i,7]*as.numeric(repcw_years$disaster[j]))       # 7
    ###   2   ###
    Xb.2[j] <-  sim.coefs[i,1]*0 +                               # Cutpoint 1   Identify these from sim.coefs
                sim.coefs[i,2]*1 -                               # Cutpoint 2   Identify these from sim.coefs
                  (sim.coefs[i,3]*as.numeric(repcw_years$logGDPpc[j]) +                              # 3
                    sim.coefs[i,4]*as.numeric(repcw_years$parliament[j]) +    # 4
                    sim.coefs[i,5]*as.numeric(repcw_years$major_oil[j]) +     # 5
                    sim.coefs[i,6]*as.numeric(repcw_years$major_primary[j]) + # 6
                    sim.coefs[i,7]*as.numeric(repcw_years$disaster[j]))       # 7
          } # end inner for loop
  # calculate probability of being in category 1-3 for each observation
  p1=plogis(Xb.1)
  p2=plogis(Xb.2)-plogis(Xb.1)
  p3=1-plogis(Xb.2)

  # average probability across all observations
  p.1[i]<-mean(p1)
  p.2[i]<-mean(p2)
  p.3[i]<-mean(p3)
} # end outer for loop


vioplot(p.1,p.2,p.3, names=c("Peace","Repression","Civil War"), 
        col="red")
title("Predicted Probabilities")


b2<-glm(factor(rep_civwar_DV) < 1 ~ logGDPpc + parliament + major_oil + major_primary + disaster, family="binomial", data = repcw_years)

b3<-glm(factor(rep_civwar_DV) == 1 ~ logGDPpc + parliament + major_oil + major_primary + disaster, family="binomial", data = repcw_years)

b4<-glm(factor(rep_civwar_DV) > 1 ~ logGDPpc + parliament + major_oil + major_primary + disaster, family="binomial", data = repcw_years)

d<-coef(b2) - coef(b3)
stargazer(d,type='html')

if(F){
  # white board
  
  <div align="center">
    <img src="image03.tif" width = 720 height = 651.6>
      </div>
      
}




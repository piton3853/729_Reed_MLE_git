clinton <- c(18,31,34,33,27,33,28,23,33,12,19,25,14)
trump <- c(11,22,27,29,24,29,25,26,38,14,23,31,20,6)
Y <- clinton
N <- clinton + trump
# Calculate Vote Share by State
Clinton / N
# Calculate Total Vote Share
sum(Y) / sum(N)

# Find p using MLE
bernoulli.lik <- function(p) {
  LL<-sum(Y*log(p)+(N-Y)*log(1-p))
}
# plot LL:
p.seq <- seq(0.01,0.99,0.01)
plot(p.seq,sapply(p.seq,bernoulli.lik),type="l")
#optimum:
optimize(bernoulli.lik,lower=0,upper=1,maximum=T)

# q1 ----
numbers<-1:4
numbers
# q2 ----
my.numbers<-rnorm(1000,10,5)
class(my.numbers)
sample(my.numbers,10)
hist(my.numbers)
rm(my.numbers)
# q3 ----
install.packages("foreign") #this will throw an error because it's already installed
library(foreign) #this throws a warning if a package doesn't exist, which will kill a function
require(foreign) #this throws a warning if a package doesn't exist, which will allow a function to continue running
# q4 ----
#get help - this is a command from the foreign package
help(read.dta)
#typing the name of a function without including the "()" afterward will print the code for that function. Useful if you need to get in to the details 
read.dta
#NOTE: use forward slashes here, not backslashes
DOCA<-read.dta("~/Documents/DOCA.dta")
#q5 ----
#Create a table that depicts the total number of events per year and assign that table to an R object
#the dollar sign can be used to access columns in a data frame by name
table(DOCA$evyy)
table(DOCA[,1]) #evyy is the first variable, so this would also work
#anything can be an object, including this table
my.table<-table(DOCA$evyy)
my.table
#the barplot cmd will plot a table
barplot(my.table)
#a two way table
my.table<-table(DOCA$viold,DOCA$evyy)
my.table
barplot(my.table)
barplot(table(DOCA$viold,DOCA$evyy), beside=TRUE, col=c("orange", "lightblue"))
#q6 ----
#there is always more than one way to do something, here are three methods: 
#by listing the variables (data frame only)
groups<-data.frame(DOCA$igrp1c1,DOCA$igrp1c2,DOCA$igrp2c1,DOCA$igrp2c2, DOCA$igrp3c1, DOCA$igrp3c2)
#by subsetting using a string vector (works with a named matrix or even a list)
DOCA[,1] # Gives first column
DOCA[1,] # gives first row
DOCA[1,1] # gives first cell
groups<-data.frame(DOCA[, c("igrp1c1","igrp1c2","igrp2c1","igrp2c2","igrp3c1","igrp3c2")])
#by a regular expression (same as above, but uses a regular expression to match the variable names)
groups<-DOCA[,which(grepl("igrp.", colnames(DOCA)))]
##detail ----
#variables in a data frame are accessed with a $ sign, but this doesn't work with matrices 
DOCA$evyy[1] #would return the first observation in evyy
as.matrix(DOCA)$evyy[1] #would return an error
##detail ----
#data frames and matrices can be referenced by row and column numbers
DOCA[1,] #returns row 1
DOCA[,1] #returns column 1
DOCA[c(1,10), ] #returns rows 1 and 10
DOCA[1:10, ] #returns 10 rows in a sequences
DOCA[,"igrp1c1"] #if there are column (or row) names, you can access them by name, and you can access multiple rows at once
##detail ----
#regular expressions can be used to match multiple strings
colnames(DOCA) #returns the column names
grepl("igrp.", colnames(DOCA)) #returns "true" if a column contains the letters igrp and "false" otherwise
#q7 ----

#plot creates a scatter plot
plot(DOCA$arrestex, DOCA$paragrph)

#lm is a Linear Model the DV is listed first, followed by a tilde (~) additional variables are added with a + sign. Models can be assigned to an R object
model<-lm(DOCA$paragrph~DOCA$arrestex)

#the summary command will give a full regression table
summary(model)

#plot model will plot regression diagnostics
plot(model)

#abline adds lines to an existing plot. 
abline(model, col="red")
#q8 ----
#writing as a csv
write.csv(DOCA, "C:/Users/Neil/Documents/DOCA.csv", row.names=FALSE)
#read it back in
DOCA<-read.csv("C:/Users/Neil/Documents/DOCA.csv")
#NOTE: Take a look at the style guide for some standards for writing code: https://google.github.io/styleguide/Rguide.xml
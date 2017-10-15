# myScript.R
#
# Write your notes and code experiments into this document. Save it
# from time to time - however I recommend that you do not _commit_
# your saved version.
#
# As long as you do not _commit_ this script to version control,
# you can _pull_ updated versions of the entire project from GitHub
# by using the RStudio version control interface. However, once
# you _commit_ any file in your local version, RStudio will require
# you to resolve conflicts before you can _pull_ updates.
#
# Journal for Greg Huang
# ====================================================================
###load packages, install if needed
install.packages("mice")
install.packages("VIM")
library(datasets)
library(VIM)
library(lattice)
library(mice)

#####Analyze a R dataset and use MICE for imputations#####

#####Analyze a GSRE4987 Dataset and use MICE for imputations#####


##########Complete case analysis(CC)##########
#Delete all cases with missing values
#Usually not recommended (lose a lot of data)
#Only use when MCAR (missing completely at random)


###Simple Random Imputation###
random.imp <- function(a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a.obs, n.missing, replace = TRUE)
  return (imputed)
}
synthetic <- read.csv("synthetic_example.csv", header = TRUE)
earnings.imp <- random.imp(synthetic$earnings)

###MICE: Multiple Imputations###

#Messing around MICE
#import airquality data
library(datasets)
library(mice)
library(lattice)
data <- airquality
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA
data <- data[-c(5,6)]
summary(data)
md.pattern(data)

pMiss <- function(x){
  sum(is.na(x))/length(x)*100
}

apply(data,2,pMiss)
apply(data,1,pMiss)

tempData <- mice(data, m=5, maxit = 50, method = 'pmm', seed = 5000)
summary(tempData)
completedData <- complete(tempData,1)

xyplot(tempData, Ozone ~ Wind+Temp+Solar.R, pch = 18, cex = .5)
densityplot(tempData)

modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
pool(modelFit1)
summary(pool(modelFit1))

tempData2 <- mice(data,m=50,seed=245435)
modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))
=======

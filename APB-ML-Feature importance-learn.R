# ___ID___.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the Assessing Feature Importance unit.
#
# Version:  0.1
#
# Date:     2017  MM  DD
# Author:   yuqing zou (yuqing.zou@mail.utoronto.ca)
#
# Versions:
#           0.1    (Describe ...)

#
# TODO:
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# = 1 Feature Selection with the Caret R Package
#Selecting the right features in your data can mean the difference between mediocre
#performance with long training times and great performance with short training times.
#The caret R package provides tools automatically report on the relevance and
#importance of attributes in your data and even select the most important features

# =1.1 install package first! you need mlbench and also caret
if (!require(mlbench, quietly = TRUE)) {
  install.packages("mlbench")
  library(mlbench)
}
if (!require(caret, quietly = TRUE)) {
  install.packages('DEoptimR')
  install.packages("pbkrtest", dependencies = TRUE)
  library(caret)
}
if (!require(e1071, quietly = TRUE)) {
  install.packages("e1071")
  install.packages("randomForest")
  library(e1071)
}


# = 1.2 Remove Redundant Features
#from original data, many features may highly correalted with each others. Those
#redundant fatures add no realevant information to your other fatures, since they
#can obtain from other features by some linear comnination.Basiclly having those
#feartures are not really helpful, which may increasing the computational cost.

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
load(file="./data/myGOExSet.RData")
#replace NA or jsut drop the row, otherwise can not caculate correlation matrix
myGOExSet[is.na(myGOExSet)] <- 0
head(myGOExSet)


# = 1.3 task1
#find the highly correlated features using two function that i recommand in unit page.
#cor() : to find the correlation matrix
#findCorrelation() :



# = 1.4 RFE auto-features-selection
#Recursive Feature Elimination is regard as wrapper methods, usually they are build in
#top of SVM or regression. RFE is based on the idea to repeatedly
#construct a model (eg.SVM) and choose best performing feature.
#All features in data set are applied for this process, and ranked features in the end.

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
load(file="./data/myGOExSet.RData")
#replace NA or jsut drop the row, otherwise can not caculate correlation matrix
myGOExSet[is.na(myGOExSet)] <- 0
head(myGOExSet)

# = 1.5 task2
#try
#cor() : to find the correlation matrix
#findCorrelation() :




# = 1.4  Task solutions for task1
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
load(file="./data/myGOExSet.RData")
#replace NA or jsut drop the row, otherwise can not caculate correlation matrix
myGOExSet[is.na(myGOExSet)] <- 0
head(myGOExSet)
# calculate correlation matrix
correlationMatrix <- cor(myGOExSet[,5:16])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


# = 1.4  Task solutions for task2
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
load(file="./data/myGOExSet.RData")
#replace NA or jsut drop the row, otherwise can not caculate correlation matrix
myGOExSet[is.na(myGOExSet)] <- 0
head(myGOExSet)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(myGOExSet[,5:15], myGOExSet[,16], sizes=c(1:12), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#that is everything here
# [END]

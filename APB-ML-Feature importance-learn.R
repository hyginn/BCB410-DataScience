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
# License:  GPL-3  https://www.gnu.org/licenses/gpl-3.0.en.html
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


# = 1 Section install package
# mlbench for sythetic data base
if (!require(mlbench, quietly = TRUE)) {
  install.packages("mlbench")
  library(mlbench)
}


# caret for mainly function
if (!require(caret, quietly = TRUE)) {
  install.packages('DEoptimR')
  install.packages("pbkrtest", dependencies = TRUE)
  library(caret)
}

# e1071 package to use random forest algorithmn
if (!require(e1071, quietly = TRUE)) {
  install.packages("e1071")
  install.packages("randomForest")
  library(e1071)
}


# = 2 Feature Selection with the Caret R Package
#Selecting the right features in your data can mean the difference between
#mediocre performance with long training times and great performance with short
#training times.The caret R package provides tools automatically report on the
#relevance and importance of attributes in your data and even select the most
#important features

# = 2.1 Remove Redundant Features
#The original data may contain some features that highly correalted with others.
#since they can obtain from other features by some linear comnination.
#this means if you train those redundant features with a learning module,
#you get no realevant information to your other fatures and extra features absolutly
#will cause more computational cost.


# = 2.1.1 explore the data set from iris data set
# Redundant features are correlated or they can be obtained by
# linear combination of other features.

# Note:
# simplely make a sythetc data set using linear combition on some random source data
# set is quit obvious, we will use a famous DataSet "iris" to show how powerful this
# method to find correlation in real life.


# load dataset, iris is used for predicting iris flower species from flower measurements.
data(iris)
iris

# The main feature we will taken is sepal.length/width and petal.length/width
# using density plots to visulized measurements distribution
par(mfrow=c(1,4))
for(i in 1:4) {
  plot(density(iris[,i]), main=names(iris)[i])
}

# this graph shows similar districution pattern from petal.width and petal.length
# We expect they have a high correlation. let's see whether Caret package will pick
# them as redundant featuresmeet and meet our expectation.


# = 1.2 Using Caret package to find redundant features

# Note:
# Generally we need make sure our features set has no NA value using
# is.na(MydataFrame). However iris data set has no NA value,
# but be careful in the lerning task 1.
is.na(iris)
# Calculate correlation matrix first! this can approach by cor(MydataFrame)
# Correlation matrix contain the correlation score (how features are correlated)
# of each features between themself and other features.
#(reference :https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/)

# If you know how redundant features are correlated with other features,
# (eg. linear correlation), you can specific method (method = pearson), otherwise
# make it default.
correlationMatrix <- cor(iris[,1:4], method = "pearson")

# You can find correlation score for each features are aganist themself and other
# features, The 1s are because everything is perfectly correlated with itself.
print(correlationMatrix)
# do you see the diagnoal of 1?
# Also do you find the Petal.Length and Petal.Width has the highest score 0.9628654,
# which is most likely to be removed.

# find attributes that are highly corrected
# Note:
# this function will return the index of highly coorelayed attributes
# Generally, you want to remove attributes with an absolute correlation of 0.75 or higher
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

# simply return the index
highlyCorrelated
# print highly correlated attributes
print(colnames(iris[,highlyCorrelated]))
# In this case the Petal.Length attribute is remove as it correlates highly with the Petal.Width attribute.
# In this case the Petal.Width attribute is remove as it correlates highly with the Petal.Length attribute.


# wait! Do you find anything is wired?
# take a look at correlation table again
print(correlationMatrix)

# there are so mant term that is grater than 0.75.
# what will happend, when the caret select all the features with a pearson score
# higer than 0.75?

# you will have no features left, what findCorrelation() do is find the highest
# correlation score term and select random one features from that. Then find
# second one....

# until rest of features has no correlation score higher then cut off !!!

# Note:
# You do not need remove those features now, why? you can find answer in the next part.


# ==============================================================================
# go back to wiki page.....

# = 1.3 task1
# Let's working with our own data set.
# In myGeneFeatures.RData, using coulumns t0~t120 as features.
# find pentially redundant features from that, if we removed them we may improve the
# perfoemance of downstream machine learning module.
# using two function that i recommand in unit page.
# cor() : to find the correlation matrix
# findCorrelation() :

# You may use this source code:

# ensure the results are repeatable
set.seed(7)
# load the library
library(caret)
# load the data
load(file="./data/myGOExSet.RData")
#replace NA or jsut drop the row, otherwise can not caculate correlation matrix
myGOExSet[is.na(myGOExSet)] <- 0

# todo: implement rest of step

# sample output:
# "t50" "t60" "t70" "t10" "t30"
# You can find complete code in wiki page. (Its hiden in the box)

# ==============================================================================

# = 2 RFE auto-features-selection
#remove redundant features can not improve the accuracy of prediction. sometimes, redundant
#features has a better perfomance on prediction.

#Recursive Feature Elimination is regard as wrapper methods, usually they are build in
#top of SVM or regression. RFE is based on the idea to repeatedly
#construct a model (eg.SVM), therefore check the predict accuracy (performance)
#and choose best performing feature.
#(reference :https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/)
#All features in data set are applied for this process, and ranked features in the end.

# =2.1 using caret R package to do auto-selection, and find important features from Iris DataSet
# ensure the results are repeatable
set.seed(7)

# For a specific model, a set of functions must be specified in rfeControl$functions.
# This function generates a control object that can be used to specify the details
# of the feature selection algorithms used in this package.

# Note:
# method: The resampling method for repeated training splits
# number: The number of folds or number of resampling iterations (idealy 10)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)


# rfe(feature, label,specific subset sizes that should be tested，rfeControl)
# In iris data column 1-4 contain the sepal.length/width and petal.length/width
# as features. column 5 contains species which is label.
# run the RFE algorithm
# this process takes a while, since it using recurence
results <- rfe(iris[,1:4], iris[,5], sizes=c(1:12), rfeControl=control)


# the results is a summaray report contains the predict accuracy for each variables
print(results)

# This function will tell you the best choosen features
predictors(results)
# is this same as what we got in the redundent features selection?
# recall findCorrelation() takes random features from the pairs of features with
# high correlation score. This is why I said you may do not need remove redundant
# features just after find them.

# visulized the result, in the graph we can see that just 2 attributes
# gives almost comparable results.
dev.off()
plot(results, type=c("g", "o"))
# The accuracy(cross validation) for each feature for REF are plotted.
# recall when we was finding the redundant features, findcorrleation()
# show these two has the highest correlation score. but from really module testing,
# they shows best performing on prediction module. In this cases, we will not
# remove the redundant features that we find in part2, but sometimes removing redundent
# features can decrased computational cost.

# ==============================================================================

# = 2.1 task2
# take a pratice on the code above:
# Using automatics feature selection on our new fatures data myGeneFeatures
# In myGeneFeatures.RData, using coulumns BP1-5 as features, t120 as label.
# Using RFE automatics feature selection with random forst algorithmn on each
# iteration to find most important feature to predict t120.

# You may use this source code:

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
load(file="./data/myGeneFeatures.RData")
#replace NA or jsut drop the row, otherwise some function will not works.
myGeneFeatures[is.na(myGeneFeatures)] <- 0

# todo: implement rest of step

# using rfeControl()
# using rfe()

# sample soultion:
# "BP5" "BP3" "BP1"
# You can find complete code in wiki page. (Its hiden in the box)

# ==============================================================================

# =3 task solution

# =3.1 task1 sample solution

# ensure the results are repeatable
set.seed(7)
# load the library
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
print(colnames(myGOExSet[,highlyCorrelated]))

# =3.2 task2 sample solution

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
load(file="./data/myGeneFeatures.RData")
#replace NA or jsut drop the row, otherwise can not caculate correlation matrix
myGeneFeatures[is.na(myGeneFeatures)] <- 0
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
print(myGeneFeatures[,18:22])
print(myGeneFeatures[,17])
results <- rfe(myGeneFeatures[,18:22], myGeneFeatures[,17], sizes=c(1:12), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)


# [END]


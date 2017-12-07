# APB-ML-RandomForests.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the APB-ML-RandomForests unit.
#
# Version:  0.1
#
# Date:     2017  10  11
# Author:   LeonXu (leon.xu@mail.utoronto.ca)
#
# Versions:
#           0.1    (Describe ...)
#
# License: GPL-3 (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
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

# = 1 Demos

# = 1.1 Decision Tree Demo

# Load required packages.

if (!require(rpart.plot, quietly = TRUE)) {
  install.packages("rpart.plot")
  library(rpart.plot)
}

if (!require(rpart, quietly = TRUE)) {
  install.packages("rpart")
  library(rpart)
}

# Let's make some synthetic data. We'll have 500 data points, each with 3
# features. There will be two classes.

syn_data <- data.frame(array(data=0, dim=c(500, 4)))
colnames(syn_data) <- c("F1", "F2", "F3", "Class")

# We'll randomly select each feature of each datapoint.
# Each class will have feature values that slightly overlap with the other.

for (i in 1:500) {
  class <- sample(1:2, 1)
  if (class == 1) {
    syn_data[i, 1] <- sample(1:2, 1)
    syn_data[i, 2] <- sample(2:3, 1)
    syn_data[i, 3] <- sample(1:3, 1)
    syn_data[i, 4] <- class
  } else {
    syn_data[i, 1] <- sample(2:3, 1)
    syn_data[i, 2] <- sample(1:2, 1)
    syn_data[i, 3] <- sample(3:5, 1)
    syn_data[i, 4] <- class
  }
}

# Create some training/test data sets by partitioning our whole synthetic data.

n <- nrow(syn_data)
trainIndex <- sample(1:n, size = round(0.7*n), replace=FALSE)
train <- syn_data[trainIndex ,]
test <- syn_data[-trainIndex ,]

# This is how simple it is to create a decision tree in R.
# Simply feed it a formula, what to train on, and a method.
# "class" is used for classification, "anova" is used for regression.

fit <- rpart(Class ~ F1 + F2 + F3, data=train, method="class")

# Graph the resulting decision tree!

rpart.plot(fit)

# Find out the accuracy of your model by running it on the test set!

predictions1 <- predict(fit, test, type="class")
evaluations1 <- predictions1 == test$Class
evaluations1 <- data.frame(evaluations1)
accuracy1 <- sum(evaluations1$evaluations1 == TRUE) / length(
  evaluations1$evaluations1)

# We can see here that the most important features are split on earlier than
# the less important. From that, we can infer that F3 was the most important
# function. This makes sense, since F3 was the one where we gave the widest
# unique range for each class (i.e. two unique values per class instead of one).

# = 1.2 Random Forest Demo

# More required packages.

if (!require(rpart, quietly = TRUE)) {
  install.packages("rpart")
  library(rpart)
}

if (!require(randomForest, quietly = TRUE)) {
  install.packages("randomForest")
  library(randomForest)
}

# We'll use the same method for synthetic data as last time.

syn_data <- data.frame(array(data=0, dim=c(500, 4)))
colnames(syn_data) <- c("F1", "F2", "F3", "Class")

for (i in 1:500) {
  class <- sample(1:2, 1)
  if (class == 1) {
    syn_data[i, 1] <- sample(1:2, 1)
    syn_data[i, 2] <- sample(2:3, 1)
    syn_data[i, 3] <- sample(1:3, 1)
    syn_data[i, 4] <- class
  } else {
    syn_data[i, 1] <- sample(2:3, 1)
    syn_data[i, 2] <- sample(1:2, 1)
    syn_data[i, 3] <- sample(3:5, 1)
    syn_data[i, 4] <- class
  }
}

# And for training/test splitting.

n <- nrow(syn_data)
trainIndex <- sample(1:n, size = round(0.7*n), replace=FALSE)
train <- syn_data[trainIndex ,]
test <- syn_data[-trainIndex ,]

# Instead of calling on rpart, we use the randomForest command instead!
# Setting importance to TRUE lets us create a plot of feature importance
# later on. ntree is simply the number of trees to generate in the
# forest.

fit <- randomForest(as.factor(train$Class) ~ F1 + F2 + F3,
                    data=train, importance=TRUE, ntree=2000,
                    na.action=na.exclude)

# Use varImpPlot() to plot out the accuracy and gini (entropy) gains made by
# splitting on each feature!

varImpPlot(fit)

# We can see that F3 was again the feature that led to the greatest decrease
# in both accuracy and entropy.

# And once again, evaluate our model on test data.

predictions2 <- predict(fit, test, type="class")
evaluations2 <- predictions2 == test$Class
evaluations2 <- data.frame(evaluations2)
accuracy2 <- sum(evaluations2$evaluations2 == TRUE) / length(
  evaluations2$evaluations2)

# The randomForest object contains many useful pieces of information regarding
# the performance of our model.

print(fit)

# Looks like both decision trees and random forests work well on our synthetic
# data, why don't you give it a try with more complex biological data!

# = 2  Task solutions

# Don't forget to set a seed for reproducibility!

set.seed(410)

# = 2.1  Task 1: Decision Trees

# Installing some necessary packages

if (!require(rpart.plot, quietly = TRUE)) {
  install.packages("rpart.plot")
  library(rpart.plot)
}

if (!require(rpart, quietly = TRUE)) {
  install.packages("rpart")
  library(rpart)
}

# Create the data sets. Load up myGeneFeatures.RData, and split into training
# and test sets.

n <- nrow(myGeneFeatures)
trainIndex <- sample(1:n, size = round(0.7*n), replace=FALSE)
train <- myGeneFeatures[trainIndex ,]
test <- myGeneFeatures[-trainIndex ,]

# Create a decision tree based on default values

fit <- rpart(termName ~ t10 + t20 + t30 + t40 + t50 + t60 + t70 + t80 + t90
             + t100 + t110 + t120 + BP1 + BP2 + BP3 + BP4 + BP5, data=train,
             method="class")

# Plot the decision tree.

rpart.plot(fit)

# Each box has three lines. The first line represents the predicted class.
# The second number is the predicted probability of the main response (in this
# case, the GO term ID). Finally, the third number is the percentage of total
# data points in this node.

# Create a decision tree without any pruning methods active and plot it.
# cp controls how much a split must decrease the overall complexity to be
# attempted.
# minsplit controls how large a node must be to attempt a split.

fit <- rpart(termName ~ t10 + t20 + t30 + t40 + t50 + t60 + t70 + t80 + t90
             + t100 + t110 + t120 + BP1 + BP2 + BP3 + BP4 + BP5, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))

rpart.plot(fit)

# Now use your tree to evaluate the test set.

predictions3 <- predict(fit, test, type="class")
evaluations3 <- predictions3 == test$termName
evaluations3 <- data.frame(evaluations3)
accuracy3 <- sum(evaluations3$evaluations3 == TRUE) / length(
  evaluations3$evaluations3)

# Now you have the proportion of correct predictions!

# You can also manually play around with other control parameters.
# Alternatively, you can manually trim a tree using the following:

snipped.fit <- prp(fit, snip=TRUE)$obj

# It will save a new tree based on your pruning of an interactive
# decision tree in the plot tab. After you are done, press "Quit"
# and it will save a version of that tree in the new variable.
# Try seeing what the new proportions of correct predictions are!

# = 2.2  Task 2: Random Forests

# Installing some necessary packages

if (!require(rpart, quietly = TRUE)) {
  install.packages("rpart")
  library(rpart)
}

if (!require(randomForest, quietly = TRUE)) {
  install.packages("randomForest")
  library(randomForest)
}

# Create the data sets. We'll use the sample gene features dataset again.

n <- nrow(myGeneFeatures)
trainIndex <- sample(1:n, size = round(0.7*n), replace=FALSE)
train <- myGeneFeatures[trainIndex ,]
test <- myGeneFeatures[-trainIndex ,]

# Create a random forest based on some parameter values.

fit <- randomForest(as.factor(train$termName) ~ t10 + t20 + t30 +
                      t40 + t50 + t60 + t70 + t80 + t90 +
                      t100 + t110 + t120 + BP1 + BP2 + BP3 + BP4 +
                      BP5, data=train, importance=TRUE, ntree=2000,
                    na.action=na.exclude)

# Plot the variable importance. This shows you how much each variable
# affected the accuracy and entropy. You can use this information to
# create a more effective forest.

varImpPlot(fit)

# Use your forest to evaluate the test set.
predictions4 <- predict(fit, test)
evaluations4 <- predictions4 == test$termName
evaluations4 <- data.frame(evaluations4)
accuracy4 <- sum(na.exclude(evaluations4)$evaluations4 == TRUE) / length(
  na.exclude(evaluations4)$evaluations4)

# You can also access various performance metrics through the fit object.

print(fit)
fit$err.rate # Prints the oob error of each specific tree.
fit$confusion # Prints the confusion matrix.
fit$votes # Prints actual vote numbers for each class.

# And more! Try typing "fit$" to see what's available!

# You might not get too much of an improvement over a single decision
# tree, but this is a very small data set.
# Random Forests are much more powerful on larger data sets than a single decision tree. But try playing around
# with the parameters (such as ntree, nodesize, sampsize, mtry) to see if you can get a larger improvement,
# or some feature engineering you've learned from other modules!

# Finally, if you end up with a good fitting Random Forest, you can save it as an .RData file, and load it up
# again later to predict other data. E.g.
# save(fit, file='demoRF.RData')
# fit2 <- load('demoRF.RData')
# predictions5 <- predict(fit2, test)

# [END]

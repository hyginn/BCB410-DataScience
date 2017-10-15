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

# = 1  Task solutions


# = 1.1  Task 1: Decision Trees

# Installing some necessary packages
if (!require(MASS, quietly = TRUE)) {
  install.packages("MASS")
  library(MASS)
}

if (!require(rpart.plot, quietly = TRUE)) {
  install.packages("rpart.plot")
  library(rpart.plot)
}

if (!require(rpart, quietly = TRUE)) {
  install.packages("rpart")
  library(rpart)
}

# Create the data sets. We'll use the Pima Indians Diabetes
# data set from MASS.

train <- Pima.tr
test <- Pima.te

# Create a decision tree based on default values
fit <- rpart(type ~ npreg + glu + bp + skin + bmi + ped + age, data=train, method="class")

# Plot the decision tree
# Each box has three lines. The first line represents the predicted
# class. The second number is the predicted probability of
# the main response (in this case, having diabetes).
# Finally, the third number is the percentage of total data points
# in this node. This is a tree with a binary response, but rpart
# can be used to create multiclass classifications just as
# easily.
rpart.plot(fit)

# Create a decision tree without any pruning methods active and
# plot it.
# cp controls how much a split must decrease the overall
# complexity to be attempted.
# minsplit controls how large a node must be to attempt a
# split.
fit <- rpart(type ~ npreg + glu + bp + skin + bmi + ped + age, data=train, method="class", control=rpart.control(minsplit=2, cp=0))

rpart.plot(fit)

# Now use your tree to evaluate the test set.
predictions <- predict(fit, test, type="class")
evaluations <- predictions == test$type
evaluations <- data.frame(evaluations)
sum(evaluations$evaluations == TRUE) / length(evaluations$evaluations)

# Now you have the proportion of correct predictions!

# You can also manually play around with other control parameters.
# Alternatively, you can manually trim a tree using the following:
snipped.fit <- prp(fit, snip=TRUE)$obj

# It will save a new tree based on your pruning of an interactive
# decision tree in the plot tab. After you are done, press "Quit"
# and it will save a version of that tree in the new variable.
# Try seeing what the new proportions of correct predictions are!

# = 1.2  Task 2: Random Forests

# Installing some necessary packages
if (!require(MASS, quietly = TRUE)) {
  install.packages("MASS")
  library(MASS)
}

if (!require(rpart, quietly = TRUE)) {
  install.packages("rpart")
  library(rpart)
}

if (!require(randomForest, quietly = TRUE)) {
  install.packages("randomForest")
  library(randomForest)
}

# Create the data sets. We'll use the Pima Indians Diabetes
# data set from MASS again.

train <- Pima.tr
test <- Pima.te

# Create a random forest based on some parameter values.
fit <- randomForest(as.factor(train$type) ~ npreg + glu + bp + skin + bmi + ped + age, data=train, importance=TRUE, ntree=2000)

# Plot the variable importance. This shows you how much each variable
# affected the accuracy and entropy. You can use this information to
# create a more effective forest.
varImpPlot(fit)

# Use your forest to evaluate the test set.
predictions <- predict(fit, test)
evaluatons <- predictions == test$type
evaluations <- data.frame(evaluations)
sum(evaluations$evaluations == TRUE) / length(evaluations$evaluations)

# You can also access various performance metrics through the fit object.
fit$err.rate
fit$confusion
fit$votes

# And more! Try typing "fit$" to see what's available!

# You might not get too much of an improvement over a single decision tree, but this is a very small data set.
# Random Forests are much more powerful on larger data sets than a single decision tree. But try playing around
# with the parameters (such as ntree, nodesize, sampsize, mtry) to see if you can get a larger improvement,
# or some feature engineering you've learned from other modules!

# [END]

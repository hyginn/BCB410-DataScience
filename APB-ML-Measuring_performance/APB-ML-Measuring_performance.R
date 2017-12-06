# ___ID___.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the APB-ML-Measuring_performance unit.
#
# Version:  0.1
#
# Date:     2017  10  09
# Author:   Aobo Cui (albert.cui@mail.utoronto.ca)
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


# = 0 Initial setup

# = 0.1 References
# Any code related to the caret package is based on its documentation website
# found here (http://topepo.github.io/caret)

# This is a very concise tutorial on how to use the caret package for some
# standard metrics for classification and regression.
# Please read the documentation accompanied in the Rmarkdown file for a full
# understand.

library(caret)    # A library for machine learning
library(ggplot2)
library(mlbench)  # For loading some bench marking datasets
library(plotROC)  # A helper library for plotting ROC curve

set.seed(42)      # Set seed for reproducibility

# = 0.2 Helper functions

# Reference: This function is based on the caret documentation 
# on measuring (found here http://topepo.github.io/caret/measuring-performance.html)
gen_data_bin_class <- function(nsamples, balance, cutoff) {
  # This function tries to simulate the test_set of a trained binary classifier
  # and its predictions. It can split the dataset into various proportions of
  # class1 and class2. Then it generates the actual probabilities of both classes
  # using a two different beta distributions.
  # Inputs:
  #   nsamples: the number of samples
  #   balance: a vector of probabilities, where the first is the probability of
  #            one class, and the second is the the prob of the second class
  # Outputs:

  # Randomly sample from a PMF where Pr(Class1) = 0.2 and Pr(Class2) = 0.8
  true_class <- factor(sample(
    paste0("Class", 1:2),
    size = nsamples,
    prob = balance,
    replace = TRUE
  ))
  true_class <- sort(true_class)
  # Sample from a beta distribution for the probabilities produced by a "model"
  class1_probs <- rbeta(sum(true_class == "Class1"), 4, 1)
  class2_probs <- rbeta(sum(true_class == "Class2"), 1, 2.5)
  test_set <- data.frame(obs = true_class,
                         Class1 = c(class1_probs, class2_probs))
  test_set$Class2 <- 1 - test_set$Class1
  test_set$pred <- factor(ifelse(test_set$Class1 >= cutoff, "Class1", "Class2"))
  
  test_set
}

# ==============================================================================

# = 1 Metrics for Classification
# First generate some data for binary classifiers with 20% Class1 and 80$ Class2 imbalance
## @knitr testSet
test_set <- gen_data_bin_class(1000, c(0.2, 0.8), 0.5)


# Plot the distribution of Class 1 probabilities
# Reference: Caret documentation
ggplot(test_set, aes(x = Class1)) +
  geom_histogram(binwidth = .05) +
  facet_wrap(~ obs) +
  xlab("Distribution of Probabilities for Class #1")

# = 1.1 Accuracy and Kappa
# Generate Accuracy and Kappa statistics
# In the caret package, the go to method for evaluating a model's predictions
# is the postResample(pred, obs) method, which takes in a vector of predicted values
# and a vector of observed values
postResample(pred = test_set$pred, obs = test_set$obs)

# = 1.2 Confusion Matrix
# In addition to postResample, you can use the confusionMatrix function from caret
# to generate a confusion matrix. Note this is not the same as the default
# confusion matrix in R standart library
confusionMatrix(data = test_set$pred, reference = test_set$obs)

# = 1.3 Log-Loss
# Compute logloss
# caret provides a function for the Log-Loss metric
mnLogLoss(test_set, lev = levels(test_set$obs))

# = 1.4 AUC
# Use ggplot2 and plotROC (an extension to ggplot2 for ROC) to plot an ROC curve
ggplot(test_set, aes(d = obs, m = Class2)) +
  geom_roc(n.cuts = 5) + style_roc() + 
  geom_rocci(sig.level = .01)

# AUC only works with binary classes. For this case, the twoClassSummary function
# is helpful for computing AUC, sensitivity, and specifity scores
# Somethings to note:
#    - you can specify which levels to use in the "lev" parameter
#    - so if you want to set the true class to Class2, use:
#      twoClassSummary(test_set, lev = c("Class2", "Class1"))
twoClassSummary(test_set, lev = levels(test_set$obs))

# = 1.5 Precision/Recall

# Caret supports Precision/Recall as well, with the prSummary() function
# Note: the AUC reported is the AUC under the Precision/Recall curve, and NOT
# the ROC curve!
prSummary(test_set, lev = levels(test_set$obs))

# Get the confusion matrix w.r.t prec_recall
confusionMatrix(data = test_set$pred, reference = test_set$obs, mode = "prec_recall")

# = 1.6 Summary
# We went over a number of standard metrics for classification such as
# the accuracy and Kappa statistics using the predicted classes
# the Log-Loss metric for the predicted probabilities, the
# precision/recall, sensitivity, and specificity metrics, as well as
# the area under the ROC curve, etc.
# You can view all of them using the a convenient summary function called
# multiClassSummary()
multiClassSummary(test_set, lev = levels(test_set$obs))

# ==============================================================================

# = 2 Metrics for Regression

# First prepare the data. We can use any dataset with continuous number. But
# lets just use the BostonHousing dataset
set.seed(24)
data(BostonHousing)

# 2.1 RMSE and R-squared
# First split the data into training and test set 70/30 split
train_p <- 0.70
test_p <- 0.30
bh_index <- createDataPartition(BostonHousing$medv, p = train_p, list = FALSE)
bh_tr <- BostonHousing[ bh_index, ]   # The training set
bh_te <- BostonHousing[-bh_index, ]   # The test set
# Train a simple linear regression model using the training set
# and use the RMSE metric as error for model fitting
lm_fit <- train(medv ~ . + rm:lstat,
                data = bh_tr,
                method = "lm",
                metric = "RMSE")
bh_pred <- predict(lm_fit, bh_te)

lm_fit

# The postResample() function is not just for classification.
# Here, this function can get you the RMSE, R-squared, and MAE
postResample(pred = bh_pred, obs = bh_te$medv)

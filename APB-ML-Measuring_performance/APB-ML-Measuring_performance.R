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


# = 0 Load packages
library(caret)
library(mlbench)  # for loading some bench marking datasets


# = 1 Metrics for classification where predictions are classes

# = 1.1 Generating some data to play with
set.seed(42)
# Randomly sample from a PMF where Pr(Class1) = 0.2 and Pr(Class2) = 0.8
true_class <- factor(sample(
  paste0("Class", 1:2),
  size = 1000,
  prob = c(.2, .8),
  replace = TRUE
))
true_class <- sort(true_class)
# Sample from a beta distribution for the probabilities produced by a "model"
class1_probs <- rbeta(sum(true_class == "Class1"), 4, 1)
class2_probs <- rbeta(sum(true_class == "Class2"), 1, 2.5)
test_set <- data.frame(obs = true_class,
                       Class1 = c(class1_probs, class2_probs))
test_set$Class2 <- 1 - test_set$Class1
test_set$pred <-
  factor(ifelse(test_set$Class1 >= .5, "Class1", "Class2"))

# Plot the distribution of Class 1 probabilities
ggplot(test_set, aes(x = Class1)) +
  geom_histogram(binwidth = .05) +
  facet_wrap(~ obs) +
  xlab("Probability of Class #1")

# Compute the confusion matrix
confusionMatrix(data = test_set$pred, reference = test_set$obs)

# Or get the confusion matrix w.r.t Precision/Recall
confusionMatrix(data = test_set$pred,
                reference = test_set$obs,
                mode = "prec_recall")

# Generate Accuracy and Kappa statistics
postResample(pred = test_set$pred, obs = test_set$obs)

# ==============================================================================

# = 2 Metrics for classification where predictions are probabilities

# Compute the area under the ROC curve and the specificity and sensitivity under 
# the 50% cutoff.
# Note:
#   - this function uses the first class level to define the “event” of interest.
#     To change this, use the lev option to the function
#   - There must be columns in the data for each of the class probabilities
#     (named the same as the outcome’s class levels)
twoClassSummary(test_set, lev = levels(test_set$obs))

# A similar function can be used to get the analugous precision-recall values
# and the area under the precision-recall curve:
# This requires MLmetrics
prSummary(test_set, lev = levels(test_set$obs))

# Compute logLoss
mnLogLoss(test_set, lev = levels(test_set$obs))

# A number of relevant metrics
# the overall accuracy and Kappa statistics using the predicted classes
# the negative of the multinomial log loss (if class probabilities are available)
# averages of the “one versus all” statistics such as sensitivity, specificity, 
# the area under the ROC curve, etc.
multiClassSummary(test_set, lev = levels(test_set$obs))

# ==============================================================================

# = 3 Visualizations

# = 3.1 ROC
# A helper library for plotting ROCs
# library(ggplot2)
# library(plotROC)
# 
roc <- ggplot(test_set, aes(d = obs, m = Class2)) + geom_roc(n.cuts = 5)
styledplot <- roc + style_roc()
styledplot + geom_rocci(sig.level = .01)


# = 3.2 Calibration Curves
# Calibration curves can be used to characterisze how consistent the predicted
# class probabilities are with the observed event rates.

# Other functions in the gbm package, the rms package (and others) can also produce calibrartion curves.
# The format for the function is very similar to the lift function:
cal_obj <- calibration(Class ~ FDA + LDA + C5.0,
                       data = lift_results,
                       cuts = 13)

# Notice the confidence intervals for the proportions inside of the subsets:
ggplot(cal_obj)

# ==============================================================================

# = 4 Metrics for regression
# First load packages and sample data
# You can choose any dataset with continous data as labels
data(BostonHousing)
# data(longley)

dataset <- BostonHousing

set.seed(280)
bh_index <- createDataPartition(dataset$medv, p = .75, list = FALSE)
bh_tr <- dataset[bh_index, ]
bh_te <- dataset[-bh_index, ]

set.seed(7279)
lm_fit <- train(medv ~ . + rm:lstat,
                data = bh_tr,
                method = "lm")
bh_pred <- predict(lm_fit, bh_te)

lm_fit

# Get the RMSE, R-squared, and MAE
postResample(pred = bh_pred, obs = bh_te$medv)

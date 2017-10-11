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
true_class <- factor(sample(paste0("Class", 1:2), 
                            size = 1000,
                            prob = c(.2, .8), replace = TRUE))
true_class <- sort(true_class)
class1_probs <- rbeta(sum(true_class == "Class1"), 4, 1)
class2_probs <- rbeta(sum(true_class == "Class2"), 1, 2.5)
test_set <- data.frame(obs = true_class,
                       Class1 = c(class1_probs, class2_probs))
test_set$Class2 <- 1 - test_set$Class1
test_set$pred <- factor(ifelse(test_set$Class1 >= .5, "Class1", "Class2"))

# Plot the distribution of Class 1 probabilities
ggplot(test_set, aes(x = Class1)) + 
  geom_histogram(binwidth = .05) + 
  facet_wrap(~obs) + 
  xlab("Probability of Class #1")

# Generating the predicted classes based on the typical 50% cutoff for the probabilities, 
# we can compute the confusion matrix, which shows a cross-tabulation of the observed and predicted classes. 
# The confusionMatrix function can be used to generate these results:
confusionMatrix(data = test_set$pred, reference = test_set$obs)

# Or get the confusion matrix w.r.t Precision/Recall
confusionMatrix(data = test_set$pred, reference = test_set$obs, mode = "prec_recall")

# Generate Accuracy and Kappa statistics
postResample(pred = test_set$pred, obs = test_set$obs)







# = 2 Metrics for classification where predictions are probabilities

# Compute the area under the ROC curve and the specificity and sensitivity under the 50% cutoff.
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
# averages of the “one versus all” statistics such as sensitivity, specificity, the area under the ROC curve, etc.
multiClassSummary(test_set, lev = levels(test_set$obs))


# = 3 Visualizations

# = 3.1 ROC
# A helper library for plotting ROCs
library(plotROC)

# Lets just use the Sonar dataset
data(Sonar)
control <- trainControl(method="cv", 
                        summaryFunction=twoClassSummary, 
                        classProbs=T,
                        savePredictions = T)
fit <- train(Class ~ ., data=Sonar, 
             method="rf", preProc=c("center", "scale"), 
             metric="ROC", trControl=control)

# Select a parameter setting
selectedIndices <- fit$pred$mtry == 2
# Plot:
ggplot(fit$pred[selectedIndices, ], 
       aes(m = R, d = factor(obs, levels = c("R", "M")))) +
       geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()


# = 3.2 Lift Curves
set.seed(2)
lift_training <- twoClassSim(1000)
lift_testing  <- twoClassSim(1000)

ctrl <- trainControl(method = "cv", classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(1045)
fda_lift <- train(Class ~ ., data = lift_training,
                  method = "fda", metric = "ROC",
                  tuneLength = 20,
                  trControl = ctrl)
set.seed(1045)
lda_lift <- train(Class ~ ., data = lift_training,
                  method = "lda", metric = "ROC",
                  trControl = ctrl)

set.seed(1045)
c5_lift <- train(Class ~ ., data = lift_training,
                 method = "C5.0", metric = "ROC",
                 tuneLength = 10,
                 trControl = ctrl,
                 control = C5.0Control(earlyStopping = FALSE))

## Generate the test set results
lift_results <- data.frame(Class = lift_testing$Class)
lift_results$FDA <- predict(fda_lift, lift_testing, type = "prob")[,"Class1"]
lift_results$LDA <- predict(lda_lift, lift_testing, type = "prob")[,"Class1"]
lift_results$C5.0 <- predict(c5_lift, lift_testing, type = "prob")[,"Class1"]
head(lift_results)

# The lift function does the calculations and the corresponding plot function is used to plot
# the lift curve (although some call this the gain curve). The value argument creates reference lines
lift_obj <- lift(Class ~ FDA + LDA + C5.0, data = lift_results)
ggplot(lift_obj, values = 60)

# = 3.3 Calibration Curves
# Calibration curves can be used to characterisze how consistent the predicted 
# class probabilities are with the observed event rates.

# Other functions in the gbm package, the rms package (and others) can also produce calibrartion curves. 
# The format for the function is very similar to the lift function:
cal_obj <- calibration(Class ~ FDA + LDA + C5.0,
                       data = lift_results,
                       cuts = 13)

# Notice the confidence intervals for the proportions inside of the subsets:
ggplot(cal_obj)

# = 4 Metrics for regression
# First load packages and sample data
# You can choose any dataset with continous data as labels
data(BostonHousing)
# data(longley)

dataset <- BostonHousing

set.seed(280)
bh_index <- createDataPartition(dataset$medv, p = .75, list = FALSE)
bh_tr <- dataset[ bh_index, ]
bh_te <- dataset[-bh_index, ]

set.seed(7279)
lm_fit <- train(medv ~ . + rm:lstat,
                data = bh_tr, 
                method = "lm")
bh_pred <- predict(lm_fit, bh_te)

lm_fit

# Get the RMSE, R-squared, and MAE
postResample(pred = bh_pred, obs = bh_te$medv)


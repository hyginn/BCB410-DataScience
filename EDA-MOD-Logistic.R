# ___ID___.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the MOD_EDA_Logistic_Regression unit.
#
# Version:  0.2
#
# Date:     2017  10  14
# Author:   Ian Shi (ian.shi@mail.utoronto.ca)
#
# Versions:
#           0.1    First draft, performing logsitic regression on myGOExSet to find potential 
#                   relationship between expression and function.
#
#           0.2    Added synthetic initial example, which works well.
#                  Added myGeneFeatures data exercise
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# Set seed for all random operations
set.seed(410)
# Disclaimer: Because of small sample sizes, accuracy may fluctuate with different seeds.
#             All percentages are contingent on seed 410.

# Comment out these below lines before running.

#load(file = "./data/myGOExSet.RData")
#load(file = "./data/myGeneFunction.RData")

# ==== 0 Generation of Simulated Logistic Regression Data ====

# We first will generate synthetic datasets for use in further analysis.
# An extremely simple but suitable dataset is proposed:

# We will randomly generate 2 clusters of data points in a 2D plane.
# These data points will have two independent variables, X-coord, and Y-coord.
# There will be a response variable, R, which is determined by which cluster each
# coordinate belongs to.

# == 0.1 Generating Dataset == 

# Generate two gaussian distributions in 2D, centered around (2, 2) and (-2, -2).
x_1 <- c(rnorm(100, 2, 2))
y_1 <- c(rnorm(100, 2, 2))
r_1 <- c(rep(1, 100))
x_2 <- c(rnorm(100, -2, 2))
y_2 <- c(rnorm(100, -2, 2))
r_2 <- c(rep(0, 100))
synth_data <- data.frame(x = c(x_1, x_2), y = c(y_1, y_2), r = c(r_1, r_2))


# ==== 1 Basic Logistic Regression on Synthetic Dataset ====
# We will now perform logistic regression on these datapoints to classify them.
# Because our dataset is simplistic, minimal preparation is required other than to split the dataset.

# == 1.1 Splitting Train/Test sets ==
# To train a logistic regression, we will split our data up into training and test sets. 

# We chose a 70/30 split for our data.
split_size <- ceiling(0.7 * nrow(synth_data))

# We ensure random sampling to allow logistic regression to work.
s_train_indices <- sample(seq_len(nrow(synth_data)), size = split_size)

s_train <- synth_data[s_train_indices, ]
s_test <- synth_data[-s_train_indices, ]

# == 1.2 Perform Logistic Regression ==
# Now, we can run logistic regression on our data. We will use the glm method.

# Because R is our response variable, we will set it as an output of all independent variables in the model.
# We run our model on our train set, holding out the test set.
s_log <- glm(r ~ ., family=binomial(link='logit'), data=s_train)

# == 1.3 Logistic Regression Analysis ==

# We can view the results of the logistic regression by using summary().
summary(s_log)
# Note that the coefficients yield high significance, as should be expected for our very simple data.

# == 1.4 Prediction on Held Out Data ==

# We now run our trained model on test data.
s_predict <- predict(s_log, s_test)

# We set our decision threshold at 0.5, and view the accuracy of prediction by comparison to the true values.
s_results <- ifelse(s_predict > 0.5, 1, 0)
s_error <- mean(s_results != s_test$r)
print(s_error)
# Results in ~ 95% prediction accuracy, which is actually close to perfect because of our gaussian sd.

# ==== 2 Exercises ====

# == 2.1 Predicting myGoExSet termName from expression ==

# It may be possible to determine the function of a protein from its expression timing.
# In this exercise, try to see if a logistic regression can classify between any two functions
# using t0-t120 levels.

# Sample Solution: 

# == 2.1.1 Data Pre-Processing ==

# Because we are using binomial discrete outputs, lets trim the data to only those of the
# function of interest, say DNA replication and osmotic stress response.
data_2_1 <- subset(myGOExSet, termName == "DNA replication" | termName == "response to osmotic stress")

# We will remove any data points without a complete expression profile.
data_2_1 <- data_2_1[complete.cases(data_2_1), ]
# Select only relevant rows
data_2_1 <- subset(data_2_1, select=c(3, 5:17))

# Finally, we need to reduce our response variable to a 0/1 output.
# We will let 0 equate to DNA replication, and 1 to osmotic stress
for (i in 1:length(data_2_1$termName)) {
  if (data_2_1$termName[i] == "DNA replication") {
    data_2_1$termName[i] <- 0
  } else {
    data_2_1$termName[i] <- 1
  }
}

data_2_1$termName <- as.numeric(as.character(data_2_1$termName))

# Next, we will sample data to create an training and testing set with a 70/30 split. 
split_size_2_1 <- ceiling(0.7 * nrow(data_2_1))
training_indices_2_1 <- sample(seq_len(nrow(data_2_1)), size = split_size_2_1)

train_2_1 <- data_2_1[training_indices_2_1, ]
test_2_1 <- data_2_1[-training_indices_2_1, ]
test_2_1 <- test_2_1[sample(nrow(test_2_1)),]

# == 2.1.2 Logistic Regression ==

# Now for the fun part! We will perform logistic regression using the glm package.
# We set termName to be the dependent variable.
log_2_1 <- glm( formula = termName ~ ., family=binomial(link='logit'), data=train_2_1)

summary(log_2_1)
# Unfortunately, it does not look like good predictors are found.

# == 2.1.3 Prediction ==

# Using our trained classifier, we can try to predict the test data.
results_2_1 <- predict(log_2_1, test_2_1, type='response')
# We choose 0.5 to be the decision boundary
results_2_1 <- ifelse(results_2_1 > 0.5, 1, 0)
error_2_1 <- mean(results_2_1 != test_2_1$termName)
print(error_2_1)

# This unfortunately yields a poor result of only ~70% predictive accuracy, which given the
# population frequency of each class indicates it is only slightly better than guessing.

# == 2.2 Predicting myGeneFeatures termName from principle components ==

# Given the principle components in the myGeneFeatures dataset, we will again try to 
# classify function, but using principle components BP1-5 exclusively.

# Sample Solution: 

# == 2.2.1 Data Pre-processing ==
# We do the same pre-processing for the myGOExSet
data_2_2 <- subset(myGeneFeatures, termName == "DNA replication" | termName == "response to osmotic stress")

# We will remove any data points without a complete expression profile.
data_2_2 <- data_2_2[complete.cases(data_2_2), ]
# Select only relevant rows
data_2_2 <- subset(data_2_2, select=c(3, 18:22))

# Finally, we need to reduce our response variable to a 0/1 output.
# We will let 0 equate to DNA replication, and 1 to osmotic stress
for (i in 1:length(data_2_2$termName)) {
  if (data_2_2$termName[i] == "DNA replication") {
    data_2_2$termName[i] <- 0
  } else {
    data_2_2$termName[i] <- 1
  }
}

data_2_2$termName <- as.numeric(as.character(data_2_2$termName))

#We again sample data to create an training and testing set with a 70/30 split. 
split_size_2_2 <- ceiling(0.7 * nrow(data_2_2))
training_indices_2_2 <- sample(seq_len(nrow(data_2_2)), size = split_size_2_2)

train_2_2 <- data_2_2[training_indices_2_2, ]
test_2_2 <- data_2_2[-training_indices_2_2, ]
test_2_2 <- data_2_2[sample(nrow(test_2_2)), ]

# == 2.2.2 Logistic Regression ==

log_2_2 <- glm( formula = termName ~ ., family=binomial(link='logit'), data=train_2_2)

summary(log_2_2)
# This time, it seems better predictors are found

# == 2.2.3 Prediction ==

# Using our trained classifier, we can try to predict the test data.
results_2_2 <- predict(log_2_2, test_2_2, type='response')
# We choose 0.5 to be the decision boundary
results_2_2 <- ifelse(results_2_2 > 0.5, 1, 0)
error_2_2 <- mean(results_2_2 != test_2_2$termName)
print(error_2_2)
# We see that the error rate is less than 10% in this classification, which is a decent result!

# == 2.2.4 Extension ==
# You can also experiment with predicting other functions in the dataset!

# [END]
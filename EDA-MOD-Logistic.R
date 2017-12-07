# ___ID___.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the MOD_EDA_Logistic_Regression unit.
#
# Version:  1.0
#
# Date:     2017  12 06
# Author:   Ian Shi (ian.shi@mail.utoronto.ca)
#
# Versions:
#           0.1    First draft, performing logsitic regression on myGOExSet to find potential 
#                   relationship between expression and function.
#
#           0.2    Added synthetic initial example, which works well.
#                  Added myGeneFeatures data exercise
#
#           1.0    Added visualizations for all data which are in low dimension.
#                  Added better variable naming schemes, and also better interpretation of results.
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

load(file = "./data/myGOExSet.RData")
load(file = "./data/myGeneFeatures.RData")

# ==== 0 Generation of Simulated Logistic Regression Data ====

# We first will generate synthetic datasets for use in further analysis.
# An extremely simple but suitable dataset is proposed:

# We will randomly generate 2 clusters of data points in a 2D plane.
# These data points will have two independent variables, X-coord, and Y-coord.
# There will be a response variable, R, which is determined by which cluster each
# coordinate belongs to.

# == 0.1 Generating Dataset == 

# Generate two gaussian distributions in 2D, centered around (2, 2) and (-2, -2).
synth_x_1 <- c(rnorm(100, 2, 2))
synth_y_1 <- c(rnorm(100, 2, 2))
synth_labels_1 <- c(rep(1, 100))
synth_x_2 <- c(rnorm(100, -2, 2))
synth_y_2 <- c(rnorm(100, -2, 2))
synth_labels_2 <- c(rep(0, 100))
synth_data <- data.frame(
  x = c(synth_x_1, synth_x_2), y = c(synth_y_1, synth_y_2), 
  r = c(synth_labels_1, synth_labels_2))

# Lets visualize our data
synth_data$c[synth_data$r == 1] <- "red"
synth_data$c[synth_data$r == 0] <- "blue"
plot(synth_data$x, synth_data$y, col = synth_data$c)

# Generate a second data set which contains more overlap
tight_synth_x_1 <- c(rnorm(100, 0.0, 0.5))
tight_synth_y_1 <- c(rnorm(100, 0.5, 0.5))
tight_synth_labels_1 <- c(rep(1, 100))
tight_synth_x_2 <- c(rnorm(100, -0, 0.5))
tight_synth_y_2 <- c(rnorm(100, -0.5, 0.5))
tight_synth_labels_2 <- c(rep(0, 100))
tight_synth_data <- data.frame(
  x = c(tight_synth_x_1, tight_synth_x_2), y = c(tight_synth_y_1, tight_synth_y_2), 
  r = c(tight_synth_labels_1, tight_synth_labels_2))

# Visualizing our data again
tight_synth_data$c[tight_synth_data$r == 1] <- "red"
tight_synth_data$c[tight_synth_data$r == 0] <- "blue"
plot(tight_synth_data$x, tight_synth_data$y, col = tight_synth_data$c)

# ==== 1 Basic Logistic Regression on Synthetic Dataset ====
# We will now perform logistic regression on these datapoints to classify them.
# Because our dataset is simplistic, minimal preparation is required other than to split the dataset.

# == 1.1 Splitting Train/Test sets ==
# To train a logistic regression, we will split our data up into training and test sets. 

# We chose a 70/30 split for our data.
split_size <- ceiling(0.7 * nrow(synth_data))

# We ensure random sampling to allow logistic regression to work.
s_train_indices <- sample(seq_len(nrow(synth_data)), size = split_size)
ts_train_indices <- sample(seq_len(nrow(tight_synth_data)), size = split_size)

s_train <- synth_data[s_train_indices, ]
s_test <- synth_data[-s_train_indices, ]

ts_train <- synth_data[ts_train_indices, ]
ts_test <- synth_data[-ts_train_indices, ]

# == 1.2 Perform Logistic Regression ==
# Now, we can run logistic regression on our data. We will use the glm method.

# We obtain the glm (general linear model) function from the default stats package.
# A reference can be found here: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html

# Because R is our response variable, we will set it as an output of all independent variables in the model.
# We run our model on our train set, holding out the test set. We do this for both synthetic data sets
s_log <- glm(r ~ . - c , family = binomial(link='logit'), data = s_train)
ts_log <- glm(r ~ . - c, family = binomial(link='logit'), data = ts_train)

# == 1.3 Logistic Regression Analysis ==

# We can view the results of the logistic regression by using summary().
summary(s_log)
summary(ts_log)
# Note that the coefficients yield high significance, as should be expected for our very simple data.

# == 1.4 Prediction on Held Out Data ==

# We now run our trained model on both our test data.
s_predict <- predict(s_log, s_test)
ts_predict <- predict(ts_log, ts_test)

# We set our decision threshold at 0.5, and view the accuracy of prediction by comparison to the true values.
s_results <- ifelse(s_predict > 0.5, 1, 0)
ts_results <- ifelse(ts_predict > 0.5, 1, 0)

s_acc <- mean(s_results == s_test$r)
ts_acc <- mean(ts_results == ts_test$r)

print(s_acc)
print(ts_acc)
# Results in ~ 95% prediction accuracy, which is actually close to perfect because of our gaussian sd.
# Our tighter distribution performs slightly worse at 93%, but still offers a good accuracy.

# == 1.5 Visualizing Logistic Regression Model ==

# We will now visualize the actual classification of test points, and decision boundary
# We can extract the coefficients of the logistic regression using the coef function

s_coef <- coef(s_log)
print(s_coef)
ts_coef <- coef(s_log)

# We see that we are given weights for each parameter, and a bias term. We can use these to plot
# the decision boundaries for each model.
# We also visualize each prediction by using colour to denote class. We fill a data point solid
# if it is misclassified.
s_test$c <- ifelse(s_test$r == 1, "red", "blue")
s_test$s <- ifelse(s_test$r == s_results, 1, 16)
plot(s_test$x, s_test$y, col = s_test$c, pch = s_test$s)
abline(s_coef[1]+1, -s_coef[2]/s_coef[3])


ts_test$c <- ifelse(ts_test$r == 1, "red", "blue")
ts_test$s <- ifelse(ts_test$r == ts_results, 1, 16)
plot(ts_test$x, ts_test$y, col = ts_test$c, pch = ts_test$s)
abline(ts_coef[1]+1, -ts_coef[2]/ts_coef[3])

# Cool! I hope the synthetic data illustrates the performance of logistic regression given
# perfect data. This will not be the usual case, as we will see below.

# ==== 2 Exercises ====

# == 2.1 Predicting myGoExSet termName from expression ==

# It may be possible to determine the function of a protein from its expression timing.
# In this exercise, try to see if a logistic regression can classify between any two functions
# using t0-t120 levels.

# Sample Solution: 

# == 2.1.1 Data Pre-Processing ==

# Because we are using binomial discrete outputs, lets trim the data to only those of the
# function of interest, say DNA replication and osmotic stress response (negative control).
myGoEx_subset <- subset(myGOExSet, 
                           termName == "DNA replication" | termName == "response to osmotic stress")

# We will remove any data points without a complete expression profile.
myGoEx_subset <- myGoEx_subset[complete.cases(myGoEx_subset), ]
# Select only relevant rows
myGoEx_subset <- subset(myGoEx_subset, select=c(3, 5:17))

# Finally, we need to reduce our response variable to a 0/1 output.
# We will let 1 equate to DNA replication, and 0 to osmotic stress
for (i in 1:length(myGoEx_subset$termName)) {
  if (myGoEx_subset$termName[i] == "DNA replication") {
    myGoEx_subset$termName[i] <- 1
  } else {
    myGoEx_subset$termName[i] <- 0
  }
}

myGoEx_subset$termName <- as.numeric(as.character(myGoEx_subset$termName))

# Next, we will sample data to create an training and testing set with a 70/30 split. 
myGoEx_split <- ceiling(0.7 * nrow(myGoEx_subset))
myGoEx_indices <- sample(seq_len(nrow(myGoEx_subset)), size = myGoEx_split)

myGoEx_train <- myGoEx_subset[myGoEx_indices, ]
myGoEx_test <- myGoEx_subset[-myGoEx_indices, ]
myGoEx_test <- myGoEx_test[sample(nrow(myGoEx_test)),]

# == 2.1.2 Logistic Regression ==

# Now for the fun part! We will perform logistic regression using the glm package.
# We set termName to be the dependent variable.
myGoEx_logit <- glm( formula = termName ~ ., family=binomial(link='logit'), data=myGoEx_train)

summary(myGoEx_logit)

# Unfortunately, it does not look like good predictors are found.
# Why is this? We can explore the data by visualizing the average expression levels for
# DNA replication proteins.

myGoEx_dna <- subset(myGoEx_subset, termName == 1)
myGoEx_osmtc <- subset(myGoEx_subset, termName == 0)

dna_mean <- colMeans(myGoEx_dna)
osmtc_mean <- colMeans(myGoEx_osmtc)
plot(dna_mean, col="red", pch=16)
points(osmtc_mean)

# I'm not very convinced there is a significant distinguishing features in these time series.
# Regardless, we will proceed with the model to see what it looks like when Logistic Regression
# doesn't find a good fit.

# == 2.1.3 Prediction ==

# Using our trained classifier, we can try to predict the test data.
myGoEx_results <- predict(myGoEx_logit, myGoEx_test, type='response')
# We choose 0.5 to be the decision boundary
myGoEx_results <- ifelse(myGoEx_results > 0.5, 1, 0)
myGoEx_acc <- mean(myGoEx_results == myGoEx_test$termName)
print(myGoEx_acc)

# This unfortunately yields a poor result of only ~65% predictive accuracy, which given the
# population frequency of each class indicates it is only slightly better than guessing.

# == 2.2 Predicting myGeneFeatures termName from principle components ==

# Given the principal components in the myGeneFeatures dataset, we will again try to 
# classify function, but using principle components BP1-5 exclusively.

# Sample Solution: 

# == 2.2.1 Data Pre-processing ==
# We do the same pre-processing for the myGOExSet
gene_feat_subset <- subset(myGeneFeatures, 
                   termName == "DNA replication" | termName == "response to osmotic stress")

# We will remove any data points without a complete expression profile.
gene_feat_subset <- gene_feat_subset[complete.cases(gene_feat_subset), ]
# Select only relevant rows
gene_feat_subset <- subset(gene_feat_subset, select=c(3, 18:22))

# Finally, we need to reduce our response variable to a 0/1 output.
# We will let 0 equate to DNA replication, and 1 to osmotic stress
for (i in 1:length(gene_feat_subset$termName)) {
  if (gene_feat_subset$termName[i] == "DNA replication") {
    gene_feat_subset$termName[i] <- 1
  } else {
    gene_feat_subset$termName[i] <- 0
  }
}

gene_feat_subset$termName <- as.numeric(as.character(gene_feat_subset$termName))

#We again sample data to create an training and testing set with a 70/30 split. 
gene_feat_split <- ceiling(0.7 * nrow(gene_feat_subset))
gene_feat_training_indices <- sample(seq_len(nrow(gene_feat_subset)), size = gene_feat_split)

gene_feat_train <- gene_feat_subset[gene_feat_training_indices, ]
gene_feat_test<- gene_feat_subset[-gene_feat_training_indices, ]
gene_feat_test <- gene_feat_subset[sample(nrow(gene_feat_test)), ]

# == 2.2.2 Logistic Regression ==

gene_feat_logit <- glm( formula = termName ~ ., family=binomial(link='logit'), data=gene_feat_train)

summary(gene_feat_logit)
# This time, it seems better predictors are found. In particular, BP2, BP3 show high significance.

# == 2.2.3 Prediction ==

# Using our trained classifier, we can try to predict the test data.
gene_feat_results <- predict(gene_feat_logit, gene_feat_test, type='response')
# We choose 0.5 to be the decision boundary
gene_feat_results <- ifelse(gene_feat_results > 0.5, 1, 0)
gene_feat_acc <- mean(gene_feat_results == gene_feat_test$termName)
print(gene_feat_acc)
# We see that the accuracy rate is 88% in this model, which is a decent result!

# == 2.2.4 Extension ==
# You can also experiment with predicting other functions in the dataset!

# [END]

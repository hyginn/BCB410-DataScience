# APB-ML-Support_Vector_Machines.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the APB-ML-Support_Vector_Machines unit.
#
# Version:  0.4
#
# Date:     2017  10  11
# Author:   Hari Sharma (hari.sharma@mail.utoronto.ca)
#
# Versions:
#           0.1    Creation of Task 1 - Brief Intro to SVMs
#           0.2    Included parts to manipulate yeast expression profile data.
#           0.3    Completed learning unit.
#           0.4    Completed revised learning unit.

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


# ==================== PART 1 - INTRO TO SVMs IN R ====================

# In the first part of this unit, we will build a Support Vector Machine that
# finds the optimal one-dimensional hyperplane (line) that categorizes two
# classes of data points, such that the margin between the classes is
# maximized. Gradient descent will be used to find the minimum of the regulizer
# term in our objective function.
#
# Prerequisite: Before proceeding with this part, read through the following
#               sections in the accompanying APB-ML-Support_Vector_Machines
#               wiki unit page:
#               -- The What, How, and Why of Support Vector Machines --
#               -- Using R to Determine Optimal One Dimensional Hyperplane in 2-D Space --


# === PART 1.1 - DEFINING OUR DATA ===

# We will use seven input data points to demonstrate how our support vector
# machine creates a hyperplane to categorize the points into two classes.
# In this example, three points will belong to a negative class (indicated by
# a red square on our plots) and four points will belong to a positive class
# (indicated by a blue square on our plots).

#Training data points that are linearly seperable.
# (xCoord, yCoord, bias term) (bias term adopted from source [2] in wiki.)
point1 <- c(-1, 1, -1)
point2 <- c(-1, 3, -1)
point3 <- c(1, 3, -1)
point4 <- c(1, 6, -1)
point5 <- c(4, 5, -1)
point6 <- c(4, 2, -1)
point7 <- c(2, 4, -1)

X <- list(point1, point2, point3, point4, point5, point6, point7)

# We will use the following output labels for mathematical convenience when
# calculating our objective function. These output labels correspond to our
# training data points, such that if the point is in our positive class, it is
# given a label of +1, and if the point is in our negative class, it is given
# a label of -1.
Y <- c(-1, -1, -1, 1, 1, 1, 1)

# We can plot these points onto a two-dimensional graph

xCoordsPos <- c()
yCoordsPos <- c()
xCoordsNeg <- c()
yCoordsNeg <- c()

index <- 0
for (point in X){
  index <- index + 1
  if (Y[index] == 1) {
    xCoordsPos <- c(xCoordsPos, point[1])
    yCoordsPos <- c(yCoordsPos, point[2])
  } else {
    xCoordsNeg <- c(xCoordsNeg, point[1])
    yCoordsNeg <- c(yCoordsNeg, point[2])
  }
}

plot(xCoordsPos,
     yCoordsPos,
     xlab="x",
     ylab="y",
     main="Training Data Points",
     xlim=c(-2,5),
     ylim=c(0, 7),
     pch=15,
     col = "blue")
points(xCoordsNeg,
       yCoordsNeg,
       pch=15,
       col="red")

# For these seven points, a potential hyperplane that categorizes the two
# classes of data can be a line passing through points (0, 6) and (3, 0),
# represented by the equation y = -2x + 6.

abline(a=6,
       b=-2,
       lwd=3,
       col="green")

# However, is there a more optimal hyperplane that maximizes the margin between
# the two classes of data points? If there is, our support vector machine can
# find such a hyperplane.


# == PART 1.2 - PERFORMING STOCHASTIC GRADIENT DESCENT TO DETERMINE AN OPTIMAL HYPERPLANE ===

# Our support vector machine will perform stochastic gradient descent to learn
# the seperating hyperplane between our negative class and our positive class.

# We first need to initialize our weight vector. This vector consists of three
# values, representing the three coefficients of the model we are trying to
# approximate using the support vector machine.
W <- c(0, 0, 0)

# We set the learning rate to 1
lrate <- 1

# We want to initially train for 100000 iterations
iterations <- 100000

# A list of errors to store misclassification events.
# This list is plotted after the training, so that we can observe the decrease in
# the error rate as the support vector machine learns.
errors <- c()

# A progress bar to show progress of learning.
pb = txtProgressBar(min = 0, max = iterations, initial = 0)
step <- 0

# Training algorithm, using stochastic gradient descent
for (iteration in 1:iterations) {
  step <- step + 1
  error <- 0
  i <- 0
  for (point in X) {
    i <- i + 1

    # This case will occur if our misclassification condition is met.
    # That is, if Y_i(X_i * w) < 1 (adopted from source [2] on wiki page).
    # In this case, we update our weight vector
    # using the gradients of both the regulizer and the loss term.

    if ((Y[i] * (X[[i]] %*% W)) < 1) {
      W <- W + lrate * ((X[[i]] * Y[[i]]) + (-2 * (1/iteration) * W))
      error <- 1

      # This case will occur if our misclassification condition is not met.
      # In this case, we update our weight vector using only the gradient
      # of the regulizer (adopted from source [2] on wiki page).
    } else {
      W <- W + lrate * (-2 * (1/iteration) * W)
    }
  }

  # Append number of errors from previous iteration
  errors <- c(errors, error)

  # Update progress bar
  setTxtProgressBar(pb,step)
}

# Now that the training is complete, we can plot the classification errors that
# occured. You should notice that the error rate decreases as the number of
# iterations increases.
pos <- which(errors == 1)

plot(pos,
     errors[errors == 1],
     xlim=c(0,iterations),
     ylim=c(0,2),
     main="Error Rate During Learning",
     xlab="Iteration",
     ylab="Misclassification",
     col="orange")


# == TASK 1 - MANIPULATING PARAMETERS THEN ANALYZING ERROR RATE ==

# For this task, you are required to re-run section 1.2, but with different
# parameter values. What happens to the misclassification rate when:
#
#     TASK 1.1 - Regularizer term is too low (i.e. the number of iterations is too low)
#     TASK 1.2 - Regularizer term is too high (i.e. the number of iterations is too high)
#
# Record your findings in your course journal.



# ==================== PART 2 - PREPARING YEAST EXPRESSION DATA FOR SUPERVISED LEARNING ====================

# To begin our exploration of SVMs in bioinformatics studies, we will create
# a set of training data and a validation set that can be used to determine
# the accuracy of our SVM once training is complete. The data that is used
# consists of yeast genes annotated with expression profiles and five
# biological processes. This data contains genes from the GSE3635 expression
# data from the NCBI, and was earlier prepared for this unit.
#
# For this unit, the data must exist at the relative path:
# "./data/myGeneFeatures.RData"

# Load the gene expression profile data
load(file = "./data/myGeneFeatures.RData")

myData <- myGeneFeatures
head(myData)

myData$termName <- as.factor(myData$termName)

# Only select the termName and expression profile data columns
myData <- myData[, c(3, 5:22)]
head(myData)

# Remove any rows that contain "NA" values
myData <- myData[complete.cases(myData),]

# We then need to generate our training and validation sets.
# We will only use 85% of the data provided to train our SVM (15% randomly
# removed)
# We will then have 15% of the data for validation, since truth values are known.

set.seed(123456)
N <- nrow(myData)
rowSelection <- sample(1:N, round(N * 0.15))
validSet <- myData[rowSelection,]
trainSet <- myData[-rowSelection,]

nrow(myData)
nrow(validSet)
nrow(trainSet)



# ==================== PART 3 - CLASSIFYING GENES BY GOSlim TERMS ====================

# === PART 3.1 - USING CARET TO CLASSIFY GENES ===

# In this first part, we will used the train() method, part of the caret
# package, to classify our data.

if (! require(caret, quietly = TRUE)) {
  install.packages("caret", dependencies = TRUE)
  library(caret)
}

# We will perform 10-fold cross validation as our sampling method.
# In this sampling method, our training data is randomly partitioned into
# 10 equal sized subsamples. Of the 10 subsamples, a single subsample is
# used as the validation data for testing the model, and the remaining 9 are
# used as training data. This is repeated 10 times, with each of the 10 subsamples
# used once as the validation data.
ctr10 <- trainControl(method="cv",
                      number=10)

# We will use accuracy as our target method, as this will allow us to
# compare the number of correct predictions to the total data set.
svmMetric <- "Accuracy"

# Now we train our SVM.
fit.caret_svm <- train(termName~.,
                       data=trainSet,
                       method="svmRadial",
                       metric=svmMetric,
                       trControl=ctr10)

fit.caret_svm

# How did the SVM perform? Was it able to correctly descriminate between
# classes of the training set?

# Let us now use our validation set to make a prediction using the model
# we generated earlier.
predict.caret_svm <- predict(fit.caret_svm, validSet)

# Then we can observe the predictions made vs. the known data points in
# a confusion matrix.
confusionMatrix(predict.caret_svm, validSet$termName)

# Based on the confusion matrix, was the SVM accurate? What could be done to
# improve the accuracy of this SVM?


# === PART 3.2 - USING e1071 TO CLASSIFY GENES ===

# In this second part, we will use the svm() method, part of the e7101
# package, to classify our data.

if (! require(e1071, quietly = TRUE)) {
  install.packages("e1071")
  library(e1071)
}

# We will train our SVM, using a cost of 10.
# By increasing the value of this cost, we can reduce the amount of training
# error; however, as we do this, the risk of overfitting our training data
# increases.
fit.e1071_svm <- svm(termName~.,
                     data=trainSet,
                     kernel="radial",
                     cost=10)

summary(fit.e1071_svm)

# A matrix, in which any value not on the diagonal accounts for training
# errors.
table(fit.e1071_svm$fitted, trainSet$termName)

# Let us now use our validation set to make a prediction using the model
# we generated earlier.
predict.e1071_svm <- predict(fit.e1071_svm, validSet)

# Again, we can observe the predictions made vs. the known data points in
# a confusion matrix.
confusionMatrix(predict.e1071_svm, validSet$termName)

# Is the accuracy of the model generated by the svm() function better/worse
# than the accuracy of fit.caret_svm?



# ==================== PART 4 - TUNING PARAMETERS FOR BETTER CLASSIFICATION ====================

# In this section we will better tune our SVMs by modifying parameters passed
# to the training functions. We will also try to use SVMs that develope
# different types of boundaries (linear, radial, polynomial), to see if that
# changes the accuracy of our models.


# === Part 4.1 - USING DIFFERENT KERNELS/METHODS FOR TRAINING ===


# == PART 4.1.1 - USING DIFFERENT METHODS WITH CARET ==

ctr10 <- trainControl(method="cv",
                      number=10)

# Here, we use a linear SVM to classify our training data.
fit.caret_svm_linear <- train(termName~.,
                              data=trainSet,
                              method="svmLinear",
                              metric=svmMetric,
                              trControl=ctr10)

fit.caret_svm_linear

predict.caret_svm_linear <- predict(fit.caret_svm_linear, validSet)

confusionMatrix(predict.caret_svm_linear, validSet$termName)


# == PART 4.1.2 - USING DIFFERENT KERNELS WITH e1071 ==

# Again, we use a linear SVM to classify our training data.
fit.e1071_svm_linear <- svm(termName~.,
                            data=trainSet,
                            kernel="linear",
                            cost=10)

summary(fit.e1071_svm_linear)

table(fit.e1071_svm_linear$fitted, trainSet$termName)

predict.e1071_svm_linear <- predict(fit.e1071_svm_linear, validSet)

confusionMatrix(predict.e1071_svm_linear, validSet$termName)


# == TASK 2 - USING POLY KERNELS/METHODS TO AFFECT ACCURACY ==

# For this task, you are required to analyze the differences in accuracy
# between the SVM models developed thus far in parts 3 and 4. Specifically,
# compare accuracies between the following models:
#
#       TASK 2.1 - Compare accuracies of fit.caret_svm and fit.caret_svm_linear
#       TASK 2.2 - Compare accuracies of fit.e1071_svm and fit.caret_svm_linear
#
# Then, perform parts 4.1.1 and 4.2.2, but instead, use polynomial
# methods/kernels.
#
#       TASK 2.3 - Record accuracies using "svmPoly" for caret and "poly" for e1071.
#       TASK 2.4 - Compre accuracies of all radial, linear, and poly methods for
#                  caret and e1071.
#
# Record your findings in your course journal.


# === Part 4.2 - USING A TUNING GRID TO IMPROVE TRAINING WITH CARET ===

# In this section we use a tuning grid to determine which misclassification
# cost value results in the lowest cross-validation error rate.

ctr10 <- trainControl(method="cv",
                      number=10)

tuningGrid <- data.frame(C=c(0.0001, 0.001, 0.01, 1))

fit.tuned_svm <- train(termName~.,
                       data=trainSet,
                       method="svmLinear",
                       metric=svmMetric,
                       trControl=ctr10,
                       tuneGrid=tuningGrid)

fit.tuned_svm

# Which value of C was used for the final model? Is this value an extreme
# from the list of provided costs in tuningGrid? If not, why do you think
# this is the case?


# [END]

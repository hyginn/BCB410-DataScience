# APB-ML-Support_Vector_Machines.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the APB-ML-Support_Vector_Machines unit.
#
# Version:  0.1
#
# Date:     2017  10  11
# Author:   Hari Sharma (hari.sharma@mail.utoronto.ca)
#
# Versions:
#           0.1    Creation of Task 1 - Brief Intro to SVMs

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
# (xCoord, yCoord, bias term)
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
    # That is, if Y_i(X_i * w) < 1. In this case, we update our weight vector
    # using the gradients of both the regulizer and the loss term.

    if ((Y[i] * (X[[i]] %*% W)) < 1) {
      W <- W + lrate * ((X[[i]] * Y[[i]]) + (-2 * (1/iteration) * W))
      error <- 1

    # This case will occur if our misclassification condition is not met.
    # In this case, we update our weight vector using only the gradient
    # of the regulizer.
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

# This shows that our support vector machine is indeed learning following
# consequtive iterations. That is, fewer points are being misclassified as
# the number of iterations increases.


# == TASK 1 - MANIPULATING PARAMETERS THEN ANALYZING ERROR RATE ==

# For this task, you are required to re-run section 1.2, but with different
# parameter values. What happens to the misclassification rate when:
#
#     TASK 1.1 - Learning rate is too low (between 0 and 1)
#     TASK 1.2 - Learning rate is too high (above 1)
#     TASK 1.3 - Regularizer term is too high (i.e. the number of iterations is too low)
#     TASK 1.4 - Regularizer term is too low (i.e. the number of iterations is too high)
#
# Note whether the error rate ever reduces to zero, as we observed in section
# 1.2. That is, do we still observe convergence in these cases?

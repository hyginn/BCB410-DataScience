# LinearRegressionUnit.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the linear regression unit.
#
# Version:  0.1
#
# Date:     2017  10  09
# Author:   Vicky Lu (vicky.lu@mail.utoronto.ca)
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

# = 1 Linear Regression
# = 1.1 Considerations and Assumptions

# = 2 Linear Model

# = 3 Correlation coefficients

# = 4 Interpretation
# = 4.1 Prediction and Confidence Limits

# = 1.1 Subsection


# =======================================================
# Module 1: Linear Regression
# ======================================================

# A regression analysis is used to explore the relationships between variables.
# Here we are trying to understand the independent variable relative to the
# dependent variable in a linear way.
# It is the simplest type of regression and most commonly used for
# predictive analysis because the outcome is continuous.

# === Considerations and Assumptions ===

# There are three considerations that we should keep in mind:
# 1. We often think of causality when looking at correlation.
#     A cause and effect relationship. However, it does not always mean that
#     one variable is the cause of the change in another variable.
# 2. There may be a confounding factor where something could affect both variables
#     but they don't have a direct relationship.
# 3. When looking at linear regression and the relationship between the two
#     variables. It could be due to random chance where each individual data
#     set has its own trend going in one direction and together it looks like
#     they correlate with one another but truth be told there is no relationship.

# Assumptions in linear regression:
# 1. There are only two important variables, the independent and dependent
#     variable that has a linear relationship
# 2. There are no confounding factor as part of the equation
# 3. Standard deviation is constant and independent of x
# 4. Errors are independent of each other


# ====================================================================
# Module 2: Linear model
# ====================================================================

# A linear model has the formula y = mx + b + e
# where...

# y is the dependent variable, also known as the response variable, regressand,
# measured variable, observed variable, output variable. It is determined by the
# independent variable plus some multiplicative or additive factor and noise or
# errors.

# x is the independent variable, also known as the predictor variable, regressor,
# controlled variable, manipulated variable, explanatory variable, input variable.

# m is our parameter variable that looks at the slope which is done by
# calculating two points of the line. It is also known as the regression
# coefficient.

# b is our other parameter variable and it describes the y-intercept which
# tells us where the line is in our model.


# You can look at functionTemplate.R to see the the function of a
# linear model


# ===================================================================
# Module 3: Coefficient of Correlation
# ===================================================================

# Correlation of coefficient is often recorded with linear regression.
# It takes the x and y coordinates and tells us whether our distributed linear
# model is applicable to analyze our dataset. It gives us information to evaluate
# whether the model is good enough for future predictions. In a biological
# analysis, the question of "why" the model is this way is often asked, and
# the coefficient of correlation is one way that could allow us to gather the
# information and make a hypothesis about why the variables are expressed.


# Note that if we check correlation coefficient on a different statistical model
# that is not linear, the coefficient may be different from what you are used to
# seeing in linear models, but it doesn’t mean the model is not coordinated, it
# is just not coordinated under linear regression and under the wrong assumption,
# therefore giving us something that isn’t meaningful to what we are looking for.

# Here is how you could generate 50 random values as our independent variable.
# The (1-r) gives us the noise value and using that to multiply with our
# random 50 values would give us additive noise. Together with the fraction of
# what we observe, we use it to calculate the dependent variable.

# x <- rnorm(5)
# r <- 0.99
# y <- (r * x) + ((1-r) * rnorm(50)));
# plot(x, y)
# cor(x, y)


# ==========================================================================
# Module 4: Interpretation
# ==========================================================================

# Are the estimated parameters any good?
# Is the model good enough?
# Which two are most highly correlated?
# Which are the two that is most similar in their response?
# What do the coefficient and intercept mean?
# We know that the key parameters are the slope and intercept since they
# generate the y value given the x value.
# In R, a function called lm(x ~ y) is provided that runs the linear model to
# calculate where one is dependent on the other. We use this to compare our
# parameters to the expected parameters at beginning of the function.

# lm(x ~ y)

# You could test whether the numbers are close by changing the noise level
# to rnorm(n, 0, 5) and run the funciton again.
# Is the correlation closer to our expected results?
# If yes, then we could say that random noise adds error.
# If the outcome was close, we could also say that the model captures the
# parameters that informed our model to begin with. So the intercept and the
# slope characterizes the regression line (abline).


# What is regression line?

# In R -> abline(lm(x ~ y), col="firebrick", lwd=3)
# If we were to take a variable x, we could observe what the y variable is
# because of the line of regression
# It represents the change of y when x changes
# It test the significance between the x and y value

# What are residuals?

# Plotting residuals usually help examine whether the linear model is
# appropriate or not
# The funciton in R is resid(lm(x ~ y))
# It is the difference between the observed and predicted value of y (Observe - predicted)
# Usually when we plot the residuals on the vertical axis and the independent
# variable on the horizontal axis if a random pattern of residuals is scattered,
# it most likely supports a linear model data.

# What are idealized values?

# In linear model, these are the values that tell how well the model would predict
# the y-value given x values in the data set.
# The function in R is fitted(lm(x ~ y))

# Ploting residuals and fitted graph is to help analyze or detect any non-linearity,
# or any outliers.
# You would be able to see the same pattern for the points that appear in the
# residuals vs fit and the fitted line (points deviate from the estimated regression line)
# Points that fall on the regression line has a residual of 0.

# === Prediction and Confidence Limits ===
# Prediction and confidence limits will help us measure whether the model is
# adequate or not. It gives us the certainty that the regression line explains
# the data we observe correctly. You would be able to tell within those boundaries
# that most data plotted is correct and those that are outside of the boundaries
# are outliers.

# Prediction limits describe how good the model is in taking new data and sets
# boundaries on future observations. It also tells us at which points it could
# potentially be outliers.

# Confidence limits tell us how accurate our mean is given the noise and data.
# How confidence is the parameters in describing our data.

# To calculate the prediction and confidence limits, it would be wise to sort
# them on x values, then plotting the lines to give a linear line.

#o <- order(HW[,1])
# HW2 <- HW[o,]

# pp<-predict(lm(HW[,2] ~ HW[,1]), int="p")
# pc<-predict(lm(HW[,2] ~ HW[,1]), int="c")
# prints out the fitted line with the upper and lower bounds
# head(pc)
# head(pp)

# plot(HW2, xlab="Height (m)", ylab="Weight (kg)", ylim=range(HW2[,2], pp))
# matlines(HW2[,1], pc, lty=c(1,2,2), col="slategrey")
# matlines(HW2[,1], pp, lty=c(1,3,3), col="firebrick")




# = 99  Task solutions

# Task 1 solutions
r <- 0.99; y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
r <- 0.8;  y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
r <- 0.6;  y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
r <- 0.4;  y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
r <- 0.01; y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)

# Task 2 solutions
#x <- <?>
#y <- <?>
cor(x, y)
plot(x, y)
abline(0, 1, col="skyblue")
lm(y ~ x)
abline(lm(y ~ x), col="red")

# Task 3 solutions

load("../BCB410-DataScience-master/data/<?>")

HW <- matrix(nrow = nrow(myGOExSet), ncol = 2) # make a matrix
#HW[ ,1] <- <?>                  # extract the data into
#HW[ ,2] <- <?>           #   the matrix

# Now plot pp and pc limits
# 1: sort on values in the first column
o <- order(HW[,1]) # o is an index vector
HW2 <- HW[o,]

# 2: compute pp, pc in sorted order
pc<-predict(lm(HW2[,2] ~ HW2[,1]), int="c")
pp<-predict(lm(HW2[,2] ~ HW2[,1]), int="p")

plot(HW2, xlab="x", ylab="y", ylim=range(x$VALUE, na.rm=TRUE))
 matlines(HW2[,1], pc, lty=c(1,2,2), col="slategrey")
 matlines(HW2[,1], pp, lty=c(1,3,3), col="firebrick")

# 3: plot the points
plot(HW2,
     col=densCols(HW2[, 1], HW2[, 2]),
     xlab = "ID",
     ylab = "t50",
     ylim=range(HW2[,2], pp),
     pch=16,
     cex=1)

# 4: add the lines of the confidence limits
matlines(HW2[,1], pc, lty=c(1,2,2), col="slategrey")
matlines(HW2[,1], pp, lty=c(1,3,3), col="firebrick")




# [END]




# = 99.1  Task 1: ...

# Plot out a graph where the x and y values are 99% identical and 1% random noise
# ...try 20% , 40%, 60%, 99% noise..

# How would you think the model will look like for each trial of different noise?
# Is it more scattered?
# How would the correlation of coefficient be affected?
# What if you removed the outliers, how would the coefficient change, how would
#   the model look?


# = 99.2  Task 2: ...

# calculate the coefficient of correlation between _____ and _____ and plot
# the values as a scatterplot


# = 99.23  Task 3: ...

# Plot a regression model with confidence and prediction intervals
# of ______ and _____ expression enrichment values.

# [END]

# EDA-MOD-Linear.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-MOD-Linear unit.
#
# Version:  0.1
#
# Date:     2017  10  09
# Author:   Vicky Lu (vicky.lu@mail.utoronto.ca)
#
# Versions:
#           0.1    Brief Intro to Linear Regression

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


# ====================================================================
# Part 1: Creating a simple linear model
# ====================================================================

# Linear models are used to help make future predictions after
# exploring the relationships between the x and y variables. We
# are trying to understand the independent variable relative to
# the dependent variable. The goal at the end of all this is to
# retrieve the parameters of our synthetic data, which are the
# slope and the y-intercept.

# We will generate observations that come from measuring age and risk of
# prevaling diseases as our synthetic sample.

# First, we will create a linear model function so that we could reuse
# it later on for the next parts.

# The function will take in a sample number, the minimum and maximum
# of the independent variable x, and a ratio of x to y.
sampleData <- function(sampleSize, min = 0, max = 75, ratio = 1.5) {

  # We will create a seed for the random generator to help generate
  # random x values
  set.seed(123)

  # Generate random uniformed x values in the interval (min, max)
  x <- runif(sampleSize, min, max)

  # We will generate hypothetical y values according to a simple linear
  # equation.
  y <- ratio * x + 1

  # Add errors/noise to the y values to play around by adding normalized
  # value with a mean and standard deviation
  y <- y + rnorm(sampleSize, mean = 0, sd = 15)
  # y <- y + rnorm(sampleSize, 0, 99)

  # Here we create the matrix of n rows and two variables our x and y
  dataDf <- data.frame(x=x, y=y)

  return(dataDf)
}

# Create your synthetic model with a sample size of 50
synthmod <- sampleData(50)
# Plot your model
plot(synthmod, xlab="Age", ylab="Risk of Prevalent Diseases")
title("Synthetic Data")

# Now you know how to create a linear model

# Go back to wiki to continue your reading on the coefficient of correlation...
# http://steipe.biochemistry.utoronto.ca/abc/students/index.php/EDA-MOD-Linear#Coefficient_of_Correlation

# ===================================================================
# Part 2: Coefficient of Correlation
# ===================================================================

# It takes the x and y coordinates and tells us whether our distributed linear
# model is applicable to analyze our dataset.

# To continue with our model, calculate the correlation
cor(synthmod$x, synthmod$y)

# What does this number mean?...

# Looks like the older you get the more risk of having prevalent diseases...
# The two variables may correlate with each other and could be used in analyzing
# future predictions.

# Let's look at the correlation again when you add even more noise to the output.
# Go back to the function and change the y variable rnorm to rnorm(sampleSize, 0, 99)
# and run the function again.

# We could see that random noise adds error and that the model now would not be
# a good model to analyze our dataset.

# Let's observe the coefficient on a new linear line graph and see what happens
# when you add noise to it.

# note x and y lengths cannot differ
x <- rnorm(50)
r <- 0.99 # 1% noise
y <- (r * x) + ((1-r) * rnorm(50))
plot(x, y)
cor(x, y)

# Here we have a graph where the x and y values are 99% identical and 1% random noise
# ...try 20% , 40%, 60%, 99% noise.. and observe how the correlation changes

# How would the coefficient of correlation be affected?
# How would the model look like when noise is added?

# Continue from the wiki page and read about
# Analyzation and Interpretation of Linear Regression in R


# ==========================================================================
# Part 3: Analyze Linear Regression
# ==========================================================================

# Our goal is to retrieve our parameters that informed our model to begin with.

# Replot our synthetic data
plot(synthmod, xlab="Age", ylab="Risk of Prevalent Diseases")

# R has a linear model function that describes the model
# Save it to a variable so that we could use in subsequent calculations
# without having to retyle the entire lm() function.
synthregressionline <- lm(synthmod[,2] ~ synthmod[,1])
# You could look at whether the parameters are close to what we have at the
# beginning by displaying the linear model
synthregressionline

# So the intercept and the slope characterizes the regression line.
# You have retrieved the complete regression equation since you have the parameters
# What does the equation tell you?
# ...

# Let's plot our regression line in our model with a function provided by R.
abline(synthregressionline, col="firebrick", lwd=3)
# ...
# The predicted risk of having prevalent disease will increase by the observed
# slope for every time you age. We could simply observe what the y variable
# is on the regression line given the x variable.

# It represents the change of y when x changes.

# Can look at the summary of the linear regression
summary(synthregressionline)



# === Part 3.1: Residuals and Fitted values ====


# Plotting residuals to help examine whether the linear model is
# appropriate or not
res <- resid(lm(synthregressionline))

# Plotting idealized values to help examine how well the model would predict
# the y-value given x values in the data set.
fit <- fitted(synthregressionline)


# Plot differences between the points to the line of best fit
segments(synthmod[,1], synthmod[,2], synthmod[,1], fit, col="#AA000044")

# Ploting residuals and fitted graph is to help analyze or detect any non-linearity
plot (fit, res)
cor(fit, res)

# What do you see? Patterns?
# What does the pattern mean?...
# Is the moddle adequate for your dataset?


# === Part 3:2: Prediction and Confidence Limits ===


# Prediction limits describe how good the model is in taking new data and sets
# boundaries on future observations.

# Confidence limits tell us how accurate our mean is given the noise and data.
# How confidence is the parameters in describing our data.

# To calculate the prediction and confidence limits, it would be wise to sort
# them on x values, then plotting the lines to give a linear line.

# Sort x values in order
o <- order(synthmod[,1])
newsynthmod <- synthmod[o,]

# Compute prediction and confidence values in sorted order
climit<-predict(lm(newsynthmod[,2] ~ newsynthmod[,1]), int="c")
plimit<-predict(lm(newsynthmod[,2] ~ newsynthmod[,1]), int="p")

# prints out the fitted line with the upper and lower bounds
head(climit)
head(plimit)

# Plot the limits
plot(newsynthmod, xlab="Age", ylab="Risk of Prevalent Diseases", ylim=range(newsynthmod[,2], plimit))
matlines(newsynthmod[,1], climit, lty=c(1,2,2), col="slategrey")
matlines(newsynthmod[,1], plimit, lty=c(1,3,3), col="firebrick")

# How does it look? How good is the model?
# Do you see any outliers?





# Back to the wiki page.





# ==============================================================================
# Part 4: Applying to Biological Datasets
# ==============================================================================

# We want to test correlation with growth rate to see which genes have significant
# expression profile of a linear response to growth.

# === Task 1: Calculate the correlation of E.coli control
# of the cell cycle of Saccharomyces cerevisiae and plot the variables
# ===

load("../BCB410-DataScience-master/data/GSE3635.RData")
info <- as.data.frame(GSE3635)
ecoli <- as.data.frame(info$E..coli.control) # extract the data into matrix
ecoli <- as.vector(as.matrix(ecoli))
time <- seq(0,120, by = 10)
dataDf <- data.frame(x=time, y=ecoli)
plot(dataDf, xlab="time (min)", ylab="E.coli control", main="Cell growth of GSE3635 with E.coli control")
cor(time, ecoli)
abline(lm(ecoli ~ time))

# What is the correlation of coefficient?
# What does it mean?

# ...
# The correlation of coefficient is -0.3957835
# E.coli control shows a negative linear response to growth as time increases..

summary(lm(ecoli ~ time))
# The p-value and the r-squared value is close 0, this may suggest that
# the points may not associate with another and that this model
# does not fit well with the dataset therefore we cannot observe
# any future predictions.


# === Task 2: Plot a regression model with confidence and prediction intervals
# of E.coli control and Human control of the cell cycle of Saccharomyces cerevisiae
# expression enrichment values. Do they coregulate?
# Determine a linear regression model equation to represent this data.
# Collect the info in your Journal and analyze.
# Go through the questions under the "Analyzation and Interpretation"
# section of the wiki.
# ===

load("../BCB410-DataScience-master/data/GSE3635.RData")
s <- as.data.frame(GSE3635)
x <- as.data.frame(s$E..coli.control)
y <- as.data.frame(s$Human.Control)
x <- as.vector(as.matrix(x))
y <- as.vector(as.matrix(y))
cor(x, y)
M <- data.frame(x=x, y=y)
plot(M)
rl <- lm(M$y ~ M$x)
abline(rl, col="red")

o <- order(M$x)
newM <- M[o,]

climit<-predict(lm(newM$y ~ newM$x), int="c")
plimit<-predict(lm(newM$y ~ newM$x), int="p")

head(plimit)
head(climit)

plot(newM, xlab="E.coli ctrl", ylab="Human ctrl", ylim=range(newM[,2], plimit))
matlines(newM$x, climit, lty=c(0,2,2), col="slategrey")
matlines(newM$x, plimit, lty=c(1,3,3), col="firebrick")

summary(rl)
# The linear regression equation is Human ctrl = 0.57885 * E.coli ctrl + 0.06
# The correlation of coefficient is 0.834122 and squaring this would give us
# a value of approximately 0.6681. This suggests that approx 66% of the variation
# in Human ctrl is explained by the Ecoli ctrl variable.
# It looks like these two variables show common expression in the cell cycle...
# and is somewhat of a good fit.
# The summary shows a low p-value which indicates that changes in the predictors
# are related to changes in the response variable.


# === Task 3: ===

# Find genes that have a significant trend that shows response.
# Find genes who have expression that increases with time where expression is
# greater in each of the time points
# Figure out if linear model works with each gene dataset.
# Which genes are the most simliar in their response?
# Which genes are most highly correlated?
# Again, collect the info in your Journal and analyze.
# Go through the questions under the "Analyzation and Interpretation"
# section of the wiki as you are working through this task.

# ================





# === Sample Solution to Task 3 ===

# We will start with the response to osmotic stress gene, YOL116W, in myGoExSet

load("../BCB410-DataScience-master/data/myGOExSet.rData")
# we will grab the expression values of YOL116W time scale at column 5 to 17
gene <- (myGOExSet[257, 5:17])
gene <- as.vector(as.matrix(gene))
x <- seq(0,120, by = 10)
d <- data.frame(x=x, y=gene)
plot(d)
cor(x, gene)
abline(lm(gene~x))

# looks like we have an increase growth expression as time increase
# Correlation is 0.698
# but how sure are we?
summary(lm(gene~x))

o <- order(x)
new <- d[o,]

c<-predict(lm(new[,2] ~ new[,1]), int="c")
p<-predict(lm(new[,2] ~ new[,1]), int="p")

plot(new, xlab="time (min)", ylab="gene", ylim=range(new[,2], p))

matlines(new[,1], c, lty=c(0,2,2), col="slategrey")
matlines(new[,1], p, lty=c(1,3,3), col="firebrick")

#...
# all points are within the boundaries
# Summary shows that there is a relationship between the two variables
# however r-squared value says that 48% of the expression values is explained
# by the time variables.



# use the geneGrowth function to continue observing the rest of the genes
source("EDA-MOD-Linear_functions.R")
geneGrowth(152)
# Housekeeping gene at 152 has a negative correlation


# check if two genes one from DNA replication and one from cell budding, correlate, are they similar?
YDR489W <- (myGOExSet[41, 5:17])
YDR489W <- as.vector(as.matrix(YDR489W))
YBR109C <- (myGOExSet[139, 5:17])
YBR109C <- as.vector(as.matrix(YBR109C))
d <- data.frame(x=YDR489W, y=YBR109C)
plot(d, xlab="YDR489W", ylab="YBR109C")
cor(d$x, d$y)
abline(lm(d$y ~ d$x))

# These two genes have a negative correlation.
summary(lm(d$y ~ d$x))
# ...




# Maybe another statistical model is more suitable for this dataset...


# [END]

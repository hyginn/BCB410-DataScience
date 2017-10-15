# EDA-MOD-Linear_functions.R
#
# Purpose:  Template for a single function that would e.g. be
#            sourced from .utilities.R or become one of the components
#            of an R package
#
# Version:  0.2
# Date:     2017 10 09
# Author:   Vicky Lu (vicky.lu@mail.utoronto.ca)
#
# Dependencies:
#           <List dependencies and preconditions>
#
# License: GPL-3 (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
# Version history:
#    <number>  Status / changes
#
# ToDo:
#    <list ...>
#
# ==============================================================================

sampleData <- function(n = 50, min = 0, max = 100, ratio = 40) {
	# Purpose:
	#     Linear models are used to help make future predictions after
  #     exploring the relationships between the x and y variables. We
  #     are trying to understand the independent variable relative to
  #     the dependent variable. The goal at the end of all this is to
  #     retrieve the parameters of our synthetic data, which are the
  #     slope and the y-intercept.
  #
  #
  # Parameters:
	#     regression coefficient:   slope   Represents the rate of the
  #                                       variable y as a function of x.
  #
	#     intercept:    y-intercept         Tells us where the line is on
  #                                       our model.
	# Details:
	#     The goal at the end of this is to retrieve the parameters of our
  #     data, which are the slope and the y-intercept. Because
  #     these are the key point in generating our y values given an x value.
  #
  #     The function will take in a sample number, the minimum, and maximum
  #     of the independent variable x, and a ratio of x to y
  #
  #     Create a seed for the random generator. This will help generate
  #     random x values
  #
  #     We will calculate hypothetical y values according to a simple linear
  #     equation.
  #
  #     First, create a matrix of n rows and 2 columns
  #     Generate a column of x values (HW[,1]) in the interval
  #     Then, generate a column of y values, (HW[, 2]) with a linear model.
  #     Do this by taking the first column of n numbers in the function and
  #     multiply by the ratio of x to y plus one. Remember that this is the
  #     key point in generating our y values, the ratio and the plus 1.
  #     Add errors/noise to the y values to play around. To do this you add
  #     numbers normally distributed with a mean of 0 and standard deviation.
	#
	# Value:
	#     <type, structure etc. of the single return value. Or:
	#     NA - function is invoked for its side effect of ... <describe>. >
  #     The function will return a matrix of the x and y variables.

	# <code ...>

  set.seed(83)
  M <- matrix(nrow = n, ncol = 2)
  M[,1] <- runif(n, min, max)
  M[,2] <- ratio * M[,1] + 1
  M[,2] <- M[,2] + rnorm(n, 0, 15)
	return(M)
}





# You may use this in testing out the different genes at different rows
# in the myGoExSet data set

geneGrowth <- function(row){

  load("../BCB410-DataScience-master/data/myGOExSet.rData")

  gene <- (myGOExSet[row, 5:17])
  gene <- as.vector(as.matrix(gene))
  x <- seq(0,120, by = 10)
  d <- data.frame(x=x, y=gene)
  plot(d)
  abline(lm(gene~x))

  o <- order(x)
  new <- d[o,]

  c<-predict(lm(new[,2] ~ new[,1]), int="c")
  p<-predict(lm(new[,2] ~ new[,1]), int="p")

  plot(new, xlab="time (min)", ylab="gene", ylim=range(new[,2], p))

  matlines(new[,1], c, lty=c(0,2,2), col="slategrey")
  matlines(new[,1], p, lty=c(1,3,3), col="firebrick")

  return(cor(x, gene))
}





# ====  TESTS  =================================================================
# Enter your function tests here...

testSample <- sampleData(50)
plot(testSample, xlab="X values", ylab="Y values")


geneGrowth(1)
geneGrowth(2)
geneGrowth(257)

# [END]

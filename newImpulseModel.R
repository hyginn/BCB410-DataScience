# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-MOD-NonLinearRegression- unit.
#
# Version:  1.1
#
# Date:     2017  12  06
# Author:   Denitsa Vasileva (denitsa.vasileva@mail.utoronto.ca)
#           Parts of code adapted from Dr. Steipe's BCH2024/30 example
#
# Versions:
#           0.1    Has the basic underpinnings of the NonLinear Regression
#                  Unit
#           1.1    Added more detailed referencing and motivation
#
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
# The impulse model generally shows a gene's response to changes in the cellular environment
# Oftentimes when a cell experiences sharp changes in the extracellular environments,the
# expression level of a gene either (1) increases or decreases temporarily before (2)
# settling into a new "steady state"
# In the impulse model developed by Chechik and Koller these two stages of gene expression
# are modelled by 2 logistic equations
# Model developed in : Chechik et al. 2009. Timing of Gene Expression Responds to Environmental Changes.
# Journal of Computational Biology.16(2):279-290.
#
# The motivation for this exercise is to show that non-linear regression can be used to model
# yeast gene expression profiles.
#
# We start off this exercise by writing a function to
# embody the Impulse Cycle model. The function is
# dependent on 6 vbariable and defines the
# 2 logistic functions
# to model the 2 phases of gene expression (Chechik, 2009).

impulseCycle<- function (h0,h1, h2, t1, t2, beta, x){
  # the first 3 parameters- h0, h1 and h2 represent the initial amplitude, the peak amplitude and the
  # steady state amplitude respectively
  # t1 represents the time the gene expression level rapidly changes for the first time following
  # the sharp environmental change
  # t2 marks the time gene expression transitions to the new steady state
  # beta is the slope of the two transitions.
  

  f <-
           (h0 + (h1 - h0) / (1+exp(-beta * (x-t1))) ) *
           (h2 + (h1 - h2) / (1+exp( beta * (x-t2)))) / h1
  return(f)
}


impulseCycleWithNoise<- function (h0,h1, h2, t1, t2, beta,x, err){
  # Now we need to add random noise to the impulseCyclefunction
  # Since we would eventually want to use non-linear regression on this function
  #The noise has to follow a normal distribution
    f <- impulseCycle(h0,h1,h2,t1,t2,beta,x) + (1-err) * rnorm(length(x))
  return(f)
}




# Plot the function impulseCycle
plotModel <- function(h0, h1, h2, t1, t2, beta, x, thisCol = "#CC0000", plt = TRUE) {

  ex <- impulseCycle(h0, h1, h2, t1, t2, beta, x)
  if (plt) {
    plot(x, ex, col = thisCol, type = "l",
         xlab = "t (min.)", ylab = "expression log-ratio",
         main = "Model",
         sub = sprintf("ho: %5.3f, h1: %5.3f, h2: %5.3f, t1: %5.3f, t2: %5.3f, beta: %5.3f", h0, h1, h2, t1, t2, beta)
    )
    #abline(h =  0, col = "#DDEEFF")
    #abline(v = 60, col = "#DDEEFF")
  } else {
    points(x, ex, col = thisCol, type = "l")
  }
}


# In this model, the first change in environment causes the
# gene expression to go down hence h1 is lower than h0 while
# the second (Chechik, 2009)
plotModel(4.8, -8, 10.8, 19.8, 30, 1, x)

# Plot the model without error
x <- 1:50
# In this example, the first change in the environment causes the gene expression
#to go up (amplitude increases) and then goes settles down to a new steady state.
plotModel(1, 10, 5, 20, 30, 1, x)


par(new=TRUE)

y <- impulseCycleWithNoise (1, 10, 5, 20, 30, 1, x, 0.5)
plot(x, y, type = "b")

# Run non-linear reression to give us the regression model
# which finds the coefficients of the model
# initial parameters--> it changes the initial parameters until it converges to
# the minimum of non-linear least squares
myFit <- nls(y ~ impulseCycle(h0, h1, h2, t1, t2, beta, x),
             start = list(h0 = 6,
                          h1 = -8,
                          h2 = 0,
                          t1 = 18,
                          t2 = 32,
                          beta = 1.3 ),  control = list(maxiter = 5000))

plotModel(x = x, h0 = coef(myFit)["h0"],
          h1 = coef(myFit)["h1"],
          h2 = coef(myFit)["h2"],
          t1 = coef(myFit)["t1"],
          t2 = coef(myFit)["t2"],
          beta  = coef(myFit)["beta"],
          thisCol = "#00FF00", plt = FALSE)



#END


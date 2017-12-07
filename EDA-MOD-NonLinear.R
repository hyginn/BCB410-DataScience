# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-MOD-NonLinearRegression- unit.
#
# Version:  1.2
#
# Date:     2017  12  06
# Author:   Denitsa Vasileva (denitsa.vasileva@mail.utoronto.ca)
#           Parts of code adapted from Dr. Steipe's BCH2024/30 example
#
# Versions:
#           0.2    Resolved issues as per Dr. Steipe's feedback
#           0.1    Has the basic underpinnings of the NonLinear Regression
#                  Unit
#
#
# TODO:   Parameter Distribution
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# Loading the dataset needed for this exercise which is myGOExSet.RData
load("./data/myGOExSet.RData")
if (!require(robustbase)) {
  install.packages("robustbase")
  library(robustbase)
}

set.seed(20160227)

# Pick a random gene from the myGOExSet database
# to examine its expression profile. In this case,
# we will work with "YAL021C".
# Once you have understood this exercise
# you can try to repeat it with other genes from the database.

yal021c <-t (myGOExSet["YAL021C", 5:17])


# Time is measured in minutes in 10 minute intervals from 0 min to 120 min.

cont_time <- seq (0,120) # continuous time for analytical functions

# create a data frame
dfYAL021C <- data.frame(seq(0,120, by = 10), yal021c)
# set column names
colnames(dfYAL021C) <- c ("time","expression")
# use this data frame as default (i.e. we can access its columns without prefix)
attach(dfYAL021C)

# plot the expression profile data for gene with ID YAL021C
plot (time, expression,
      xlab = "time (min.)",
      ylab = "expression log-ratio",
      ylim= c (1.1 * min(expression), 1.1 * max(expression)),
      col = "#DD99CC",
      type = "b"
      )


#  Detect heteroscedasticity. In simple terms heteroscedasticity refers to the change in the residual
#  across different values of the dependent variable.
#  Lack of heteroscedasticity is an important assumption in linear and non-linear models.
# (the variance of residuals should not increase with fitted values of response variable)
#  cited from: https://datascienceplus.com/how-to-detect-heteroscedasticity-and-rectify-it/ [1]

# First, let's fit linear model
lmya1021c <- lm (expression ~ time, data = dfYAL021C)
lmya1021c
cat (gettextf("lm(): expression = %s * time + %s \n", coef(lmya1021c)["time"], coef(lmya1021c)["(Intercept)"]))

summary (lmya1021c)

#There are two ways to test for heterosedasticity:
#   - Graphically
#   - Through statistical tests
# A. Graphical method
# init 4 charts in 1 panel
par(mfrow=c(2,2))
plot(lmya1021c, pch=21)

#  We are interested in the chart of residuals vs fitted values (top-left)
#  and standardized residuals on Y axis (bottom left). According to [1], if there is absolutely no heteroscedastity,
#  we should see a completely random, equal distribution of points throughout the range of X axis and a flat red line.
#  In our case, the red line on the top left chart is slightly curved and but the residuals does not seem to
#  increase significantly as the fitted Y values increase. So - we can't conclude whether heteroscedasticity exists.
#  Therefore we apply statistical test.

# B. Statistical tests
# [1] suggests that we can perform 2 tests: The Breush-Pagan test and the NCV (Non-constant Variance) Score Test

lmtest::bptest(lmya1021c)  # requires package lmtest to be installed
# Result:
#        studentized Breusch-Pagan test
#  data:  lmya1021c
#  BP = 0.034485, df = 1, p-value = 0.8527

car::ncvTest(lmya1021c)
# Result:
#
# Non-constant Variance Score Test
# Variance formula: ~ fitted.values
# Chisquare = 0.01861079    Df = 1     p = 0.8914882

# With a p-value of 0.8527 (larger that a  that a significance level of 0.05), we cannot reject the null hypothesis
# (that variance of residuals is constant) and therefore infer that the residuals are homoscedastic.

# Now, let's try to fit a non linear function to the data
# The most basic way to estimate such parameters is to use a non-linear least squares approach (function nls in R)
# which basically approximate the non-linear function using a linear one and iteratively try
# to find the best parameter values
# cited from: https://datascienceplus.com/first-steps-with-non-linear-regression-in-r/


# As per Dr. Steipe's feedback: "Can you set up your nls() as a plain linear model, then run your
# comparison to lm() in the same framework and with the same output?"

nlsLm<-nls(expression ~ a * time + b, start = list ( a = 0.25, b = 0.25),  control = list(maxiter = 5000, tol = 1e-05, printEval = TRUE) )
cat (gettextf("best fit linear model: expression = %s * time + %s \n", coef(nlsLm)[1], coef(nlsLm)[2]))

# The two models seems exactly the same
cor(expression, predict(nlsLm))
cor(expression, predict(lmya1021c))

# Graphical comparison
par(mfrow=c(1,1))
plot(expression ~ time, pch=21, bg="gold", type="b")
lines(time,predict(nlsLm),lty=1,col="red",lwd=1)
lines(time,predict(lmya1021c),lty=2,col="yellow",lwd=1)

# None of the linear models seems to be good fit for the yeast gene
# To model the yeast gene cycle we rather use a cyclic function
# with parameters time (t), amplitude (Ampl), phase (Phase) and frequency(Freq)
cosFun <- function(t, Ampl, Phase, Freq, offst) {
     Ampl * (cos((((t - Phase) * 2 * pi) / 60) / Freq) ) + offst
}

# helper to check model fit based on R squared for the above function
checkModelFitRsq <- function (model) {
  prediction <- cosFun(time, Ampl=coef(model)["Ampl"], Phase=coef(model)["Phase"], Freq=coef(model)["Freq"], offst=coef(model)["offst"])
  SSE <- sum((expression - prediction) ^ 2)
  SST <- sum((expression - mean(expression)) ^ 2)
  return (1 - SSE/SST)
}

# Let's create the first model - let's try to eyeball some of the attributes
guess_amplitude = (max(expression) - min (expression))/2  # 0.0694
guess_offst =  mean(expression) # -0.074
# from the graph the frequency is 1 cycle for 60min
guess_freq <- 1
# the phase is ~30min
guess_phase <- 30

# Let's plot it with the guessed values
lines(cont_time, cosFun(cont_time, Ampl=guess_amplitude, Phase=guess_phase, Freq=guess_freq, offst=guess_offst), col="green")

# Finding the correlation between the
# plot of gene YAL021C's expression profile
# and our cyclic Model with the guessed parameters
cor (expression, cosFun(time, Ampl=guess_amplitude, Phase=guess_phase, Freq=guess_freq, offst = guess_offst)) #cor=0.736


# Let's use the nls() function to find the best parameters for the cosFun() model.
nlFit <- nls(expression ~ cosFun(time, Ampl, Phase, Freq, offst),
             start = list(Ampl = guess_amplitude,
                          Phase = guess_phase,
                          Freq = guess_freq,
                          offst = guess_offst), control = list(maxiter = 5000) )

# Show the graph with the fitted partameters
lines(cont_time, cosFun(cont_time, Ampl=coef(nlFit)["Ampl"], Phase=coef(nlFit)["Phase"], Freq=coef(nlFit)["Freq"], offst=coef(nlFit)["offst"]), col="red")
cor(expression,  cosFun(time, Ampl=coef(nlFit)["Ampl"], Phase=coef(nlFit)["Phase"], Freq=coef(nlFit)["Freq"], offst=coef(nlFit)["offst"]))
# 0.834311 - slightly better than the initially guessed attributes

# check model fit based on R2
checkModelFitRsq(nlFit)
# 0.696 - not a great result

# let's compare the models using Chi squared
anova(nlFit, nlsLm,  test ="Chisq")

#Model 1: expression ~ cosFun(time, Ampl, Phase, Freq, offst)
#Model 2: expression ~ a * time + b
#Res.Df Res.Sum Sq Df    Sum Sq F value   Pr(>F)
#1      9  0.0050794
#2     11  0.0164113 -2 -0.011332  10.039 0.005105 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

save(nlFit, file = "nlFit.mdl")

# Read the model from file
#load ("nlFit.mdl")

# let's improve our fitting strategy as suggested by Dr. Steipe in BCH2024 - Models
#    (I) Try different parameters and select the best result

# We'll adopt modified version of Dr. Steipe's 'bestFirSimple()' function.
# The difference is that instead of listing the initial values for the parameter
# we will parametrize those in ranges and will combine all possible scenarious
# Note - contrarty to what we expect - it takes ~10sec to execute

# helper function which draws model to existing graph
addModelToGraph <- function(t, m, c) {
  lines(t, cosFun(t, coef(m)["Ampl"], Phase=coef(m)["Phase"], Freq=coef(m)["Freq"], offst=coef(m)["offst"]),col=c)
}

bestFitSimple <- function(df, Ampl_mn, Ampl_mx, Phase_mn, Phase_mx, Freq_mn, Freq_mx, Offst_mn, Offst_mx) {
  # Tries different parameter settings for nls() and returns the best
  # fit object.
  nlsFits <- list()
  nlsCors <- numeric()
  # Split each interval X_mn..X_mx in k-ranges (let's start with k=10)
  k <- 10
  Ampl_range <- seq (Ampl_mn, Ampl_mx, by = (Ampl_mx-Ampl_mn) / k )
  Phase_range <- seq (Phase_mn, Phase_mx, by = (Phase_mx-Phase_mn) / k )
  Freq_range <- seq (Freq_mn, Freq_mx, by = (Freq_mx-Freq_mn) / k )
  Offst_range <- seq (Offst_mn, Offst_mx, by = (Offst_mx - Offst_mn) / k)

  # Lets visualize the fitting model. First, we refresh the graph
  plot (df$time, df$expression,
        xlab = "time (min.)",
        ylab = "expression log-ratio",
        ylim= c (1.1 * min(expression), 1.1 * max(expression)),
        col = "#DD99CC",
        type = "b"
  )

  bestFitSoFar <- list()
  currentFit <- list()
  firstRun <- TRUE
  numberValidatedModel <- 0
  # combinatorics - this may take time
  for (A in 1:k) {
    for (P in 1:k) {
      for (Fr in 1:k) {
        for (O in 1:k) {
          # the nls() will be called k^^4 times.
          # cat (sprintf("Calling nls() with Ampl=%s Phase=%s Freq=%s Offst=%s", Ampl_range[A], Phase_range[P],Freq_range[Fr],Offst_range[O]  ))
          try(currentFit <- nls(df$expression ~ cosFun(time, Ampl, Phase, Freq, offst), data = df,
                           start = list(Ampl =  Ampl_range[A],
                                        Phase = Phase_range[P],
                                        Freq =  Freq_range[Fr],
                                        offst = Offst_range[O])
          ),
          silent = TRUE)
          if (length(currentFit) > 0) {
            numberValidatedModel <- numberValidatedModel + 1
            currentCor = cor(df$expression, predict(currentFit))
            if (firstRun || (currentCor > bestCorSoFar)) {
              print ("Better model fitted.")
              bestFitSoFar <- currentFit
              bestCorSoFar <- currentCor
              firstRun <- FALSE
              addModelToGraph(cont_time, currentFit, "honeydew3")
            }
          }
        }
      }
    }
  }
  if (numberValidatedModel > 0) {
    cat(sprintf("Total number of fitted models:%s", numberValidatedModel))
    return(bestFitSoFar)}
  else {
    # seems nls() did not fit a model. let's try nlrob
    myLower <= c (Ampl_mn, Phase_mn, Freq_mn,  Offst_mn)
    myUpper <= c (Ampl_mx, Phase_mx, Freq_mx,  Offst_mx)
    names(myLower) <- c("Ampl", "Phase", "Freq", "offst")
    names(myUpper) <- c("Ampl", "Phase", "Freq", "offst")

    bestFitSoFar <- nlrob(df$expression ~ cosFun(time, Ampl, Phase, Freq, offst),
                    data = df,
                    method = "mtl",
                    lower = myLower,
                    upper = myUpper)
    return(bestFitSoFar)
  }

}

#lightslategray, lightsteelblue, lightsteelblue1, lightyellow, lightyellow1, lightyellow2, lightyellow3
#

# We can define reasonable ranges for each parameter looking at the diagram
# Let's call the function (takes ~25sec)
bestFitStep1 <- bestFitSimple(df = dfYAL021C,
                         Ampl_mn = 0,
                         Ampl_mx = abs(1.2 * max(expression)),
                         Phase_mn = -120,
                         Phase_mx = 120,
                         Freq_mn = 0,
                         Freq_mx = 3,
                         Offst_mn = -0.3,
                         Offst_mx = 0)
# otline the model
addModelToGraph(cont_time, bestFitStep1, "hotpink4")


# how good is this model?
cor (expression, predict(bestFitStep1))
# 0.834311 - same as whart we already found

# compare the parameters of the automatically fitted model (bestFit function) and the guessed model
cat(sprintf("bestFitStep1() model parameters: Ampl=%s, Phase=%s, Freq=%s, Offst=%s", coef(bestFitStep1)["Ampl"], Phase=coef(bestFitStep1)["Phase"], Freq=coef(bestFitStep1)["Freq"], offst=coef(bestFitStep1)["offst"]))
cat(sprintf("guessed model parameters: Ampl=%s, Phase=%s, Freq=%s, Offst=%s", coef(nlFit)["Ampl"], Phase=coef(nlFit)["Phase"], Freq=coef(nlFit)["Freq"], offst=coef(nlFit)["offst"]))
# They differ in phase and Freq
# bestFitStep1() model parameters: Ampl=-0.0416755160489038, Phase=-3.35321245415, Freq=-0.976015257449566, Offst=-0.0702513587093953
# guessed model        parameters: Ampl=0.041675512106027,  Phase=25.927218616127, Freq=0.976016191897029,  Offst=-0.070251382875593

# lets zoon-in and run another itteration with the bestFit() function.
# This time we know the approximate values of the parameters that fit best and we want to var +/- 15%
bestFit <- bestFitSimple(df = dfYAL021C,
                              Ampl_mn = -0.0416755 - 0.15 * 0.0416755,
                              Ampl_mx = -0.0416755 + 0.15 * 0.0416755,
                              Phase_mn = -3.353 - 0.15 * 3.353,
                              Phase_mx = -3.353 + 0.15 * 3.353,
                              Freq_mn = -0.976 - 0.15 * 0.976,
                              Freq_mx = -0.976 + 0.15 * 0.976,
                              Offst_mn = -0.07025 - 0.15 * 0.07025,
                              Offst_mx = -0.07025 + 0.15 * 0.07025)

cat(sprintf("bestFit() model parameters: Ampl=%s, Phase=%s, Freq=%s, Offst=%s", coef(bestFit)["Ampl"], Phase=coef(bestFit)["Phase"], Freq=coef(bestFit)["Freq"], offst=coef(bestFit)["offst"]))
# how good is this model?
cor (expression, predict(bestFit))
# 0.834311 - same as whart we already found
addModelToGraph(cont_time, bestFit, "orange3")

#
# bestFit is the best model we can fit with that cosFun() function and offset
# let's save it first. One of Dr. Steipe's feedback comments was to
# "add instructions how to save parameters of a successful model".
save (bestFit, file = "best-fit.m")

# load the model if needed
load ("best-fit.m")

# as per Dr. Steipe's example - let's add damping term exp (-kt)
# the new function should be as folows:

cosFunExp <- function(t, Ampl, Phase, Freq, offst, ex) {
  exp (-ex*t) * Ampl * (cos((((t - Phase) * 2 * pi) / 60) / Freq) ) + offst
}

# helper
addExpModelToGraph <- function(t, m, c) {
  lines(t, cosFunExp(t, coef(m)["Ampl"], Phase=coef(m)["Phase"], Freq=coef(m)["Freq"], offst=coef(m)["offst"], ex=coef(m)["ex"]),col=c)
}

# helper to check model fit based on R squared for the above exp function
checkExpModelFitRsq <- function (model) {
  prediction <- cosFunExp(time, Ampl=coef(model)["Ampl"], Phase=coef(model)["Phase"], Freq=coef(model)["Freq"], offst=coef(model)["offst"], ex=coef(model)["ex"])
  SSE <- sum((expression - prediction) ^ 2)
  SST <- sum((expression - mean(expression)) ^ 2)
  return (1 - SSE/SST)
}

# Let's rewrite bestFit so it can fit the additional parameter and the new function
bestFitExp <- function(df, Ampl_mn, Ampl_mx, Phase_mn, Phase_mx, Freq_mn, Freq_mx, Offst_mn, Offst_mx, ex_mn, ex_mx) {
  # Tries different parameter settings for nls() and returns the best
  # fit object.
  nlsFits <- list()
  nlsCors <- numeric()
  # Split each interval X_mn..X_mx in k-ranges (let's start with k=8)
  k <- 8
  Ampl_range <- seq (Ampl_mn, Ampl_mx, by = (Ampl_mx-Ampl_mn) / k )
  Phase_range <- seq (Phase_mn, Phase_mx, by = (Phase_mx-Phase_mn) / k )
  Freq_range <- seq (Freq_mn, Freq_mx, by = (Freq_mx-Freq_mn) / k )
  Offst_range <- seq (Offst_mn, Offst_mx, by = (Offst_mx - Offst_mn) / k)
  ex_range <- seq (ex_mn, ex_mx, by = (ex_mx - ex_mn) / k)

  # Lets visualize the fitting model. First, we refresh the graph
  plot (df$time, df$expression,
        xlab = "time (min.)",
        ylab = "expression log-ratio",
        ylim= c (1.1 * min(expression), 1.1 * max(expression)),
        col = "#DD99CC",
        type = "l"
  )

  # let's get some initial values
  bestFitSoFar <- list()
  currentFit <- list()
  firstRun <- TRUE
  numberValidatedModel <- 0

  # combinatorics - this may take time
  for (A in 1:k) {
    for (P in 1:k) {
      for (Fr in 1:k) {
        for (O in 1:k) {
          for (E in 1:k) {
            # the nls() will be called k^^5 times.
            # cat (sprintf("Calling nls() with Ampl=%s Phase=%s Freq=%s Offst=%s", Ampl_range[A], Phase_range[P],Freq_range[Fr],Offst_range[O]  ))
            try(currentFit <- nls(df$expression ~ cosFunExp(time, Ampl, Phase, Freq, offst, ex), data = df,
                                  start = list(Ampl =  Ampl_range[A],
                                               Phase = Phase_range[P],
                                               Freq =  Freq_range[Fr],
                                               offst = Offst_range[O],
                                               ex = ex_range[E])
            ),
          silent = TRUE)
          if (length(currentFit) > 0) {
            numberValidatedModel <- numberValidatedModel + 1
            currentCor = cor(df$expression, predict(currentFit))
            if  (firstRun || (currentCor > bestCorSoFar)) {
                print ("Better model fitted.")
                bestFitSoFar <- currentFit
                bestCorSoFar <- currentCor
                firstRun <- FALSE
                addModelToGraph(cont_time, currentFit, "grey")
            }
          }
         }
        }
       }
      }
  }
  if (numberValidatedModel >0 ) {
    cat(sprintf("Total number of fitted models:%s", numberValidatedModel))
    return(bestFitSoFar) }
  else {
    # if nls() did not return at least one model - we use robust nls
    # nlrob()
    myLower <- c (Ampl_mn, Phase_mn, Freq_mn,  Offst_mn, ex_mn)
    myUpper <- c (Ampl_mx, Phase_mx, Freq_mx,  Offst_mx, ex_mx)
    names(myLower) <- c("Ampl", "Phase", "Freq", "offst", "ex")
    names(myUpper) <- c("Ampl", "Phase", "Freq", "offst", "ex")
    bestFitSoFar <- nlrob(df ~ cosFunEx(time, Ampl, Phase, Freq, offst, ex),
                          data = df,
                          method = "mtl",
                          lower = myLower,
                          upper = myUpper)
    return (bestCorSoFar)
    }
}

# fit the extended model (takes ~35 sec)
bestFitExpModel <- bestFitExp(df = dfYAL021C,
                              Ampl_mn = 0,
                              Ampl_mx = abs(1.2 * max(expression)),
                              Phase_mn = -120,
                              Phase_mx = 120,
                              Freq_mn = 0,
                              Freq_mx = 3,
                              Offst_mn = -0.3,
                              Offst_mx = 0,
                              ex_mn = 0.01,
                              ex_mx = 0.1
                              )
cor (expression, predict(bestFitExpModel))
# 0.8345373 - minor improvement

anova(bestFitExpModel, bestFit)
#Analysis of Variance Table
# Model 1: df$expression ~ cosFunExp(time, Ampl, Phase, Freq, offst, ex)
#Model 2: df$expression ~ cosFun(time, Ampl, Phase, Freq, offst)
#Res.Df Res.Sum Sq Df      Sum Sq F value Pr(>F)
#1      8  0.0050731
#2      9  0.0050794 -1 -6.3107e-06    0.01  0.923

# The addition of exp() did not improve significantly the model

checkModelFitRsq(bestFit)
# 0.6960748

checkExpModelFitRsq(bestFitExpModel)
# 0.6964524 - minor improvement

# Note: we could fit the model once again with initial parameters
# close to the coefficients of the current best fit model
# but this will not make difference



# ===============================


# Create a function to plot a profile and its fitted curve
# The function is cited from Dr. Steipe's BCH2024
checkFit <- function(ID, fit) {
  t <- seq(0, 120, by = 10)
  y <-t (myGOExSet[ID, 5:17])
  y [is.na(y)] <- mean(y, na.rm=TRUE)

  rw <- myGOExSet[which (myGOExSet$ID == ID), ]
  plot(t, y, col = "red", type = "b",
       xlab = "t (min.)", ylab = "expression log-ratio",
       main = sprintf("%s: %s (%s)",
                      ID,rw$name,rw$termName ))
  mtext(sprintf("Parameters: cor: %5.3f, %s",
                cor(y, predict(fit)),
                paste(names(coef(fit)),
                      sprintf("%5.3f", coef(fit))
                      , sep = ": ", collapse = ", ")),
        col = "lightsteelblue4", side = 1, line = 4)
  p <- cosFunExp(cont_time, Ampl=coef(fit)["Ampl"],
                            Phase=coef(fit)["Phase"],
                            Freq=coef(fit)["Freq"],
                            offst=coef(fit)["offst"],
                            ex=coef(fit)["ex"])
  points(cont_time, p, col = "paleturquoise4", type = "l")
}

# Let's generate models for all expression profiles

N <- nrow(myGOExSet)
nlsResults <- data.frame(Ampl = numeric(N),
                         Phase = numeric(N),
                         Freq = numeric(N),
                         offst = numeric(N),
                         ex = numeric(N),
                         cor = numeric(N))
myFit <- list()
t <- seq(0, 120, by = 10)


for (i in 1:N) {
  y <- myGOExSet[i,5:17]
  try(myFit <- nls(y ~ cosFunExp(t, Ampl, Phase, Freq, offst, ex),
                   start = list(Ampl = coef(bestFitExpModel)["Ampl"] ,
                                Phase = coef(bestFitExpModel)["Phase"],
                                Freq =  coef(bestFitExpModel)["Freq"],
                                offst = coef(bestFitExpModel)["offst"],
                                ex = coef(bestFitExpModel)["ex"]),
                    silent = FALSE))
  if (length(myFit) > 0) {
    nlsResults$Ampl[i] <- coef(myFit)["Ampl"]
    nlsResults$Phase[i] <- coef(myFit)["Phase"]
    nlsResults$Freq[i] <- coef(myFit)["Freq"]
    nlsResults$offst[i] <- coef(myFit)["offst"]
    nlsResults$ex[i] <- coef(myFit)["ex"]
    nlsResults$cor[i] <- cor(y, predict(myFit))
  }
}

# good correlations:
plot(nlsResults$Ampl, nlsResults$cor)
( sel <- which(nlsResults$Ampl > 0.3 & nlsResults$cor > 0.75) )

#END

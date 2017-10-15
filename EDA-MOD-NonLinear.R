# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-MOD-NonLinearRegression- unit.
#
# Version:  1.1
#
# Date:     2017  10  11
# Author:   Denitsa Vasileva (denitsa.vasileva@mail.utoronto.ca)
#           Parts of code adapted from Dr. Steipe's BCH2024/30 example
#
# Versions:
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

#Pick a random gene from the myGOExSet database
#to examine its expression profile. In this case,
# we will work with "YAL021C".
# Once you have understood this exercise
# you can try to repet it with other genes from the database.

yal021c <-t (myGOExSet["YAL021C", 5:17])

# Time is measured in minutes in 10 minute intervals from 0 min to 120 min.
t <- seq (0,120,by = 10)

par(mar = rep(2, 4))
# plot the expression profile data for gene with ID YAL021C
plot (t, yal021c,
      xlab = "t (min.)",
      ylab = "expression log-ratio",
      ylim= c (-0.2,0.2),
      col = "#DD99CC",
      type = "b"
      )

# To model the yeast gene cycle we use a cyclic function
# with parameters time (t), amplitude (Ampl), phase (Phase) and frequency(Freq)
cosFun <- function(t, Ampl, Phase, Freq) {
    Ampl * (cos((((t - Phase) * 2 * pi) / 60) / Freq) )
}

#Allows us to draw plots on top of existing plots
par(new=TRUE)

# A sample of the  cyclic model with values
# of amplitude, phase and frequency which have been set
# after trial-and-error
plot (t, cosFun(t, Ampl=0.05, Phase=30, Freq=1),
      xlab = "t (min.)",
      ylim=c(-0.2, 0.2),
      ylab = "expression log-ratio",
      col = "#3311FF",
      type = "b"
)

# Finding the correlation between the
# plot of gene YAL021C's expression profile
# and our cyclic Model
cor (yal021c, cosFun(t, Ampl=-0.2, Phase=30, Freq=1)) #cor=0.7359

# We use the nls() function to find the best parameters for the cosFun function.
myFit <- nls(yal021c ~ cosFun(t, Ampl, Phase, Freq),
             start = list(Ampl = 0.05,
                          Phase = 30,
                          Freq = 1.0), control = list(maxiter = 500) )

# We plot the new model obtained by myFit.
plotModel <- function(t, Ampl,Phase, Freq, thisCol = "#000000", plt = TRUE) {

    ex <- cosFun(t, Ampl, Phase, Freq)
    if (plt) {
        plot(t, ex, col = thisCol, type = "l",
             xlab = "t (min.)", ylab = "expression log-ratio",
             ylim=c (-0.2,0.2),
             main = "Model",
             sub = sprintf("A: %5.3f, f: %5.3f, phi: %5.3f", Ampl, Freq, Phase)
        )
    } else {
        points(t, ex, col = thisCol, type = "l")
    }
}

#Run plotModel with the improved parameters calculated by nls()
plotModel(t, Ampl = coef(myFit)["Ampl"],
          Phase = coef(myFit)["Phase"],
          Freq = coef(myFit)["Freq"],
          thisCol = "#CC0000", plt = FALSE)
myFit
# Determine the correlation between the gene expression  plot
#of YAL021C and the model developed by the nls() function.

cor (yal021c, predict (myFit))

checkFit <- function(ID, fit) {
    t <- seq(0, 120, by = 10)
    y <-t (myGOExSet[ID, 5:17])
    y [is.na(y)] <- mean(y, na.rm=TRUE)
    plot(t, y, col = "red", type = "b",
         xlab = "t (min.)", ylab = "expression log-ratio",
         main = sprintf("%s: %s (%s)",
                        ID,
                        "",
                        "" ))
    mtext(sprintf("Parameters: cor: %5.3f, %s",
                  cor(y, predict(fit)),
                  paste(names(coef(fit)),
                        sprintf("%5.3f", coef(fit))
                        , sep = ": ", collapse = ", ")),
          col = "#DD99CC", side = 1, line = 4)
    t2 <- seq(0, 120, by = 1)
    y2 <- data.frame(t = t2)
    points(t2, predict(fit, newdata = y2), col = "#DD99CC", type = "l")
}
#You can trying out the function  with a different gene
# to check whether it  works as an exercise

#yar007c <- t (myGOExSet["YAR007C", 5:17])
#checkFit("YAR007C", myFit)

#cor (yar007c, predict (myFit))

#Using nls() to calculate how well our model fits the rest of the
#expression profiles and then trying to find parameters in the
#data that may be of interest and may be useful features

NumRow <- nrow(myGOExSet)
nlsResults <- data.frame(Ampl = numeric(NumRow),
                         Phase = numeric(NumRow),
                         Freq = numeric(NumRow),
                         cor = numeric(NumRow))


for (i in 1:NumRow) {
    y <- t (myGOExSet[i,5:17])

    y [is.na(y)] <- mean(y, na.rm=TRUE)

    try(myFit <- nls(y ~ cosFun(t, Ampl, Phase,Freq),
                     start = list(Ampl = 0.05,
                                  Phase = 30,
                                  Freq = 1.0) ), silent = TRUE)

    if (length(myFit) > 0) {
        nlsResults$Ampl[i] <- coef(myFit)["Ampl"]
        nlsResults$Phase[i] <- coef(myFit)["Phase"]
        nlsResults$Freq[i] <- coef(myFit)["Freq"]
        nlsResults$cor[i] <- cor(y, predict(myFit))
    }
}


#Plots points with high amplitude and moderate to high correlation
plot(nlsResults$Ampl, nlsResults$cor)
( sel <- which(abs(nlsResults$Ampl) > 0.2 & nlsResults$cor > 0.8) )
myGOExSet[sel, c("name", "termName")]
# most of the genes taht have a high amplitude and high correlation are involved in
# DNA replication and some in cell budding
# only one is involved in response to osmotic stress

# === Reviewing the Model  ===

# Checking for profiles that have a high amplitude but a low correlation.
sel <- which(nlsResults$Ampl > 0.2 & abs(nlsResults$cor) < 0.4)
nlsResults[sel,]
# We now see that we have 5 genes with correlation that is less than 0.4
# This means that they don't fit our model very well.
# Hence, we now try to plot a new function with
# the current fit from nls() without parameters
# and then the new fit with parameters.

plotFit <- function(i, myAmpl, myPhase, myFreq) {
  t <- seq(0, 120, by = 10)
  y <-myGOExSet[i,5:17]

  origCor <- nlsResults$cor[i]
  origAmpl <- nlsResults$Ampl[i]
  origPhase <- nlsResults$Phase[i]
  origFreq <- nlsResults$Freq[i]
  plot(t, y, type="b",
       xlab = "", ylab = "log-ratio",
       main = sprintf("%d: %s (%s)",
                      i,
                      myGOExSet$ID[i],
                    myGOExSet$Name[i]))


  points(t, cosFun(t, origAmpl, origPhase, origFreq), type="l", col="#AA0000")
  if (! missing(myAmpl)) { # Try a new fit with these parameters
    myFit <- nls(y ~ cosFun(t, Ampl, Phase, Freq),
                 start = list(Ampl = myAmpl,
                              Phase = myPhase,
                              Freq = myFreq),
                 control = nls.control(maxiter = 200))

    points(t, cosFun(t,
                        coef(myFit)["Ampl"],
                        coef(myFit)["Phase"],
                        coef(myFit)["Freq"]),
           type="l", col="#00DD88")

  mtext(sprintf("New fit:  cor: %5.3f, Ampl: %5.3f, Phase: %5.3f, Freq: %5.3f",
                  cor(y, predict(myFit)),
                  coef(myFit)["Ampl"],
                  coef(myFit)["Phase"],
                 coef(myFit)["Freq"]),
         col = "#00DD88", side = 1, line = 4)
  }
}

# Now we can run our new function plotFit with some of the
# models with low correlation that we previously identified
# in line 185 to check if the correlation improves.

i  <- 7  # sel[7], when we ran the code in line 185
plotFit(i)

#END

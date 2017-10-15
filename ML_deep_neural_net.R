# ___ID___.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the ML_Deep_Neural_Networks unit.
#
# Version:  0.1
#
# Date:     2017  10  11
# Author:   Jenny Yin (jyjenny11@gmail.com)
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

# = 1 Preparation
# Prepare R libraries and data

# = 1.1 Library imports

if (!require(utils, quietly = TRUE)) {
  install.packages("utils")
  library(utils)
}

if (!require(grid, quietly = TRUE)) {
  install.packages("grid")
  library(grid)
}

if (!require(grDevices, quietly = TRUE)) {
  install.packages("grDevices")
  library(grDevices)
}

if (!require(stats, quietly = TRUE)) {
  install.packages("stats")
  library(stats)
}

if (!require(utils, quietly = TRUE)) {
  install.packages("utils")
  library(utils)
}

if (!require(neuralnet, quietly = TRUE)) {
  install.packages("neuralnet")
  library(neuralnet)
}

if (!require(nnet, quietly = TRUE)) {
  install.packages("nnet")
  library(nnet)
}

if (!require(caret, quietly = TRUE)) {
  install.packages("caret")
  library(caret)
}

# = 1.2 Import and clean data

load(file = "./data/myGOExSet.RData")
rawData <- myGOExSet
input <- rawData[,-(1:4)]
output <- class.ind(factor(rawData$termName)) # one hot encoding
colnames(output) <- make.names(colnames(output)) # remove spacing from column names
data <- data.frame(input, output)
data <- na.omit(data)
N <- dim(data)[1]

# = 2 Fit neural network

# = 2.1 model 1
namesOutput <- paste(colnames(output), collapse=" + ")
namesInput <- paste(colnames(input), collapse = " + ")
f <- as.formula(paste(namesOutput, namesInput, sep=" ~ "))
modelNeuralNet <- neuralnet(f ,trainingdata, hidden=10, threshold=0.01)
print(modelNeuralNet)

#Test the neural network on some training data
#testdata <- as.data.frame(data[(s + 1):N,])
#net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
#ls(net.results)

#Lets see the results
#print(net.results$net.result)

#Lets display a better version of the results
#cleanoutput <- cbind(testdata,sqrt(testdata),
#                     as.data.frame(net.results$net.result))
#colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
#print(cleanoutput)



# = 2.2 model 2






# = 99  Task solutions


# = 99.1  Task 1: ...

# = 99.2  Task 2: ...


# [END]

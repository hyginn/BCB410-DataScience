# ML_deep_neural_net.R
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

if (!require(graphics, quietly = TRUE)) {
  install.packages("graphics")
  library(graphics)
}


# = 1.2 Import and clean data

load(file = "./data/myGOExSet.RData")
rawData <- myGOExSet
input <- rawData[,-(1:4)]
output <- class.ind(factor(rawData$termName)) # one hot encoding
colnames(output) <- make.names(colnames(output)) # remove spacing from column names
data <- data.frame(input, output)
data <- na.omit(data)

N <- dim(data)[1] # total number of samples
nFeatures <- dim(input)[2]
nClasses <- dim(output)[2]

# report
print("============Data Summary============")
print("Features: ")
print(colnames(input))
print("Target classes: ")
print(colnames(output))
print(paste("Total number of rows: ", N))
print("====================================")

# = 2 K fold cross validation for differnt neural network architectures

idx <- sample.int(N)
K <- 10
size <- N %/% K
namesOutput <- paste(colnames(output), collapse=" + ")
namesInput <- paste(colnames(input), collapse = " + ")
f <- as.formula(paste(namesOutput, namesInput, sep=" ~ "))

errorsPerNLayers <- c()
for(i in nFeatures:nClasses) {
  neuronLayers = nFeatures:i
  nLayers = length(neuronLayers)
  errorsPerFold <- c()

  for(fold in 1:K) {
    # split into test and train data
    start <- (fold-1) * size
    end <- start + size
    idxTest <- idx[(start + 1):end]
    idxTrain <- idx[-((start + 1):end)]
    test <- data[idxTest,]
    train <- data[idxTrain,]

    # train
    nn <- neuralnet(f, train, hidden=neuronLayers, threshold=0.01, act.fct = "logistic", linear.output = FALSE)
#    if(fold == 1) {
#      plot(nn)
#    }

    # test
    target <- max.col(test[,14:dim(test)[2]])
    prediction <- max.col(compute(nn, test[,1:13])$net.result)
    errorsPerFold[fold] <- mean(target == prediction)

    print(paste("Layers: ", nLayers, ", Fold: ", fold, ", Error: ", errorsPerFold[fold]))
  }

  errorsPerNLayers[nLayers] <- mean(errorsPerFold)
  print("-----------------------------------------------------------------")
  print(paste("Layers: ", nLayers, ", Architecture: ", paste(neuronLayers, collapse=" ")))
  print(paste("Mean error: ", errorsPerNLayers[nLayers]))
  print("-----------------------------------------------------------------")

}

print("=====================SUMMARY=====================")
x=1:length(errorsPerNLayers)
y=errorsPerNLayers
# hmmmmmm.........
print(paste(x, y, sep=": "))
plot(x, y, type="l", main="NN Performance", xlab="# of layers", ylab="error")

# [END]

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
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# ====  PACKAGES  ==============================================================

# for training neural networks with backprop
if (!require(neuralnet, quietly = TRUE)) {
  install.packages("neuralnet")
  library(neuralnet)
}
# library(help="neuralnet")


# ====  Section 1: Loading sample data  ========================================

# We will use a well-processed and understood database, Intercountry Life-Cycle
# Savings Data, already included in the R package datasets

data("LifeCycleSavings")
summary(LifeCycleSavings)

# visualize our data
# code snippet from the R help page on the LifeCycleSavings dataset
require(stats); require(graphics)
pairs(LifeCycleSavings, panel = panel.smooth,
      main = "LifeCycleSavings data")
fm1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
summary(fm1)


# ====  Section 2: Fit NN model on sample data  ================================

set.seed(1)

# define the model formula
f <- sr ~ pop15 + pop75 + dpi + ddpi

# train the model with default parameters, namely:
#     hidden = 1           - one hidden neuron in each layer
#     threshold = 0.01     - stop training when partial derivatives of loss function
#                            is 0.01
#     stepmax = 1e+05      - stop training when exceed 1e+05 steps
#     act.fct = "logistic" - use logistic activation function
#     linear.output = TRUE - choose to apply the activation function on output neurons
#     algorithm = "rprop+" - use resilient backpropagation with weight backtracking
#                            to train the network
nn <- neuralnet(formula = f, data = LifeCycleSavings)

# visualize
print("nn weights: ")
print(nn$weights)
plot(nn)
# our error is 491.814125, which is very high!
# this is because the predictions of our model are on the interval of [0, 1], so
# we need to scale our targets

# process our data, scale everything to [0, 1]
dat <- apply(LifeCycleSavings,
             2,
             function(x) { (x - min(x)) / (max(x) - min(x)) })

nn2 <- neuralnet(formula = f,
                 data = dat)
plot(nn2)
# error is 0.755478, so much better!

# how about if we used a different architecture?
# with more layers, training takes longer. For only 3 layers, this might
# not be noticible
nn3 <- neuralnet(formula = f,
                 data = dat,
                 hidden = c(5, 3, 5))
# ... although plotting will take a few seconds longer
plot(nn3)
# error is 0.326466, even better!



# ====  Section 3: Fit NN model on biological data  ============================

# from nnet documentation:
# Fit single-hidden-layer neural network, possibly with skip-layer connections.
# so not really relevant for our task
# we just want to use its class.ind function for one-hot encoding
if (!require(nnet, quietly = TRUE)) {
  install.packages("nnet")
  library(nnet)
}


# = 3.1 Import and clean data

load(file = "./data/myGOExSet.RData")
rawData <- myGOExSet
summary(rawData)

# the features are [-2, 2] at most, so we don't really need to normalize
input <- rawData[,-(1:4)]

# visualize the data first
plot(data.frame(input, rawData$termName))

# now split up the target column for one-hot encoding
output <- class.ind(factor(rawData$termName))
# remove spacing from column names
colnames(output) <- make.names(colnames(output))

data <- data.frame(input, output)
data <- na.omit(data) # omit nas

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


# = 3.2 K fold cross validation for different neural network architectures

set.seed(1)

idx <- sample.int(N)
K <- 10
size <- N %/% K
namesOutput <- paste(colnames(output), collapse=" + ")
namesInput <- paste(colnames(input), collapse = " + ")

# the formula for our NN
f <- as.formula(paste(namesOutput, namesInput, sep=" ~ "))

errorsPerNLayers <- c()
bestWeightsPerNLayers <- c()

# originally I tried up to 9 layers. This took a very long time, and the error rate didn't
# realy improve after 2 layers. So we'll just run with up to 3 layers here, not actually
# very deep...
for(i in 8:6) {
  neuronLayers = 8:i
  nLayers = length(neuronLayers)
  errorsPerFold <- c()
  weightsPerFold <- c()

  for(fold in 1:K) {
    # split into test and train data
    start <- (fold-1) * size
    end <- start + size
    idxTest <- idx[(start + 1):end]
    idxTrain <- idx[-((start + 1):end)]
    test <- data[idxTest,]
    train <- data[idxTrain,]

    # train
    nn <- neuralnet(f, train,
                    hidden=neuronLayers,
                    threshold=0.01,
                    act.fct = "logistic",
                    linear.output = FALSE,
                    startweights = rep(1, sum(neuronLayers)))

    # test
    target <- max.col(test[,14:dim(test)[2]])
    prediction <- max.col(compute(nn, test[,1:13])$net.result)
    errorsPerFold[fold] <- mean(target == prediction)
    weightsPerFold[fold] <- nn$weights

    print(paste("Layers: ", nLayers, ", Fold: ", fold, ", Error: ", errorsPerFold[fold]))
  }

  errorsPerNLayers[nLayers] <- mean(errorsPerFold)
  bestWeightsPerNLayers[nLayers] <- weightsPerFold[which(errorsPerFold == min(errorsPerFold))[1]]
  print("-----------------------------------------------------------------")
  print(paste("Layers: ", nLayers, ", Architecture: ", paste(neuronLayers, collapse=" ")))
  print(paste("Mean error: ", errorsPerNLayers[nLayers]))
  print("-----------------------------------------------------------------")

}

print("=====================SUMMARY=====================")
x=1:length(errorsPerNLayers)
y=errorsPerNLayers

print("mean error per layer: ")
print(paste(x, y, sep=": "))
plot(x, y, type="l", main="NN Performance", xlab="# of layers", ylab="error")

# we didn't try many layers, but I can verify the error only increases with more layers
# this suggests that our data doesn't really have high-order relationships
# maybe it's better to use linear regression on this one!

# anywho, let's see the best model:
bestLayer <- errorsPerNLayers == min(errorsPerNLayers)
minError <- errorsPerNLayers[bestLayer]
nnBest <- neuralnet(f, data=train[1,], threshold=0.01,
                    act.fct = "logistic",
                    linear.output = FALSE,
                    startweights = bestWeightsPerNLayers[bestLayer])
print("Best model:")
cat("error: ", minError, "\n")
print("weights:")
print(nnBest$weights)
plot(nnBest)

# [END]

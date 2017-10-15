# APB-ML-h2o.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the APB-ML-h2o unit.
#
# Version:  0.1
#
# Date:     2017  10  15
# Author:   Farzan Taj (farzantaj@outlook.com)
#
# Versions:
#           0.1    (Initial Submission)

#
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# ==== Part 1: Package Installation and Introduction ====
# H2O is an open-source platform focused on machine learning and artificial
# intelligence. It is continuously optimized for parallel and GPU computing.
# The following is an introduction to H2O for R.

# = 1.1 Installation
# Ensure that a compatible JDK is installed on your computer: currently versions
# 1.6-1.8 are supported. (i.e. not JRE 8 or 9)
# http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html

# Install h2o's depencencies from CRAN
pkgs <- c("statmod","RCurl","jsonlite")
install.packages(pkgs)

# The following two lines remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# The is a direct download from the h2o repository, which is usually 1-2
# releases ahead of CRAN. The installation may take a few mintues dependent on
# connection...
repos <- c("https://h2o-release.s3.amazonaws.com/h2o/rel-turing/9/R",
           getOption("repos"))
install.packages("h2o", type = "source", repos = repos)
library(h2o)

# = 1.2 Setup
# After running library(h2o), the package is only loaded into the ram.
# In order to use the package, one must first initialize the engine; you can
# specify the amount of computational resources you wish to allow h2o to access.
?h2o.init

# nthreads = -1 uses all threads.
# Note that apparently in h2o, multicore processes are not quite reproducible,
# thus we'll set nthreads = 1 for now.
h2o.init(nthreads = 1, max_mem_size = '512M', port = 50001)

# Here a port is manually selected to avoid IPv6 issues (uncomment to test)
# h2o.init(nthreads = -1, max_mem_size = '512M', port = 50001)

# ===== Dependency Check =====
# In order to demonstrate the use of h2o's deep learning algorithm, we begin by
# testing it on widely used sample datasets. You can see a list of them here:
data()
# We will first begin by using a multi-class dataset
data("iris")
?iris
# We have sepal and petal length and width for 50 flowers from each of 3 species
# of iris.
# Although this unit is not focuses on this matter, but visualization of the
# data prior to training (or any data science project) can give us an great
# overview:
# Fist we must reshape our data for proper plotting
library(data.table)
tempIris = as.data.table(iris)
# Turn the data into "long" form
moltenIris = melt.data.table(data = tempIris, id.vars = "Species")
# Plot
irisPlot = ggplot(data = moltenIris,
       mapping = aes(x = Species, y = value, fill = Species)) +
  geom_bar(stat = "identity") +
  facet_grid(facets = ~variable) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Iris data overview")
irisPlot
# We can refer to this plot during training; its interpretation is currently not
# necessary

# ==== Part 2: Deep Learning ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/deep-learning.html


# We have already loaded the iris data, let's first have a look at what the
# data looks like
class(iris)
typeof(iris)
head(iris)
# As you can see, our learning problem is to use the 4 independent variable
# columns Sepal.Length, Sepal.Width, Petal.Length and Petal.Width to predict the
# dependent variable Species. Our features are of type double whereas our target
# is categorical; therefore this is a multi-class classification problem.
predictors = colnames(iris)[1:4]
target = colnames(iris)[5]

# Here are the parameters we specify for our deep learning model:
## 1. As discussed on the wiki, we can assign a seed so that the results of our
# training can be reproducible.

## 2. As we have 50 observations for each class, we do not need to balance our
# classes.

## 3. Since the dataset is reasonably small, maximum runtime or early stopping
# do not need to be specified.

## 4. In order to ensure the model does not overfit the training data, we will
# use cross validation. Multiple fold values will be used for demonstration.

## 5. We can check the mean and variance of our data:
mean(iris$Sepal.Length); var(iris$Sepal.Length)
mean(iris$Sepal.Width); var(iris$Sepal.Width)
mean(iris$Petal.Length); var(iris$Petal.Length)
mean(iris$Petal.Width); var(iris$Petal.Width)
# Since we don't have zero mean and unit variance, we better standardize the data,
# especially since this is a classification problem

## 6. We will compare both L1 and L2 regularization to see their effect on the
# testing phase.

# We first train on our data without any cross-validation to see its performance
# on the testing set. Thus we must manually create a train/test split.
# We'll use a random 80%-20% split, but we also ensure that each class is equally
# represented:
# We'll use a sampling function that considers this
source("ABP-ML-h2o/APB-ML-h2o_functionTemplate.R")
set.seed(1)
dim(iris)
classSize = nrow(iris) / length(unique(iris$Species))
myIdx = classWiseSample(size = classSize, count = nrow(iris)/classSize,
                        proportion = 0.8)
# We first must convert our data.frame to an h2o.frame; this makes our data
# available to the JVM that h2o runs on:
trainingSet = as.h2o(iris[myIdx,])
testingSet = as.h2o(iris[-myIdx,])
# Validate:
table(iris[myIdx,]$Species)
table(iris[-myIdx,]$Species)

# Note that we will not have regularization here
# Train
deepNoCross = h2o.deeplearning(seed = 1, standardize = T, x = predictors,
                               y = target, training_frame = trainingSet,
                               validation_frame = testingSet, nfolds = 0)

# One of the simplest ways of presenting the performance of an ML model is a
# confusion matrix. h2o automatically creates many other metrics for us as well:
h2o.confusionMatrix(deepNoCross)
# As you can see, we have ~30% error rate, where virginica was mistaken for
# versicolor

# If we refer back to our plot,
irisPlot
# We can see that versicolor and virginical are quite a lot closer to to each
# other across the 4 features compared to setosa. Let's see if we can improve
# our predictions.

## Task: Modify the testing/validation set ratios and see which ratio will give
# use minimum error. Ensure that you set seed correctly each time.

# Let's train on our data again without standardization:
deepNoCrossNoStand = h2o.deeplearning(seed = 1, standardize = F, x = predictors,
                               y = target, training_frame = trainingSet,
                               validation_frame = testingSet, nfolds = 0)

h2o.confusionMatrix(deepNoCrossNoStand)
# ~33% error rate, this is why it's best to standardize the data in such cases.
# Fortunately, it is enabled by default, so we will not need to specify it.

# Now we will use different cross validation schemes to see if it can make our
# model more generalizable
# Note that we will not need our testing set anymore
h2o.iris = as.h2o(iris)
deepCross2 = h2o.deeplearning(seed = 1, x = predictors, y = target,
                              training_frame = h2o.iris, nfolds = 2)
h2o.confusionMatrix(deepCross2)
# We can see that our with 2 folds, the error rate is at ~19%. It is common
# practice that 5 folds are chosen:
deepCross5 = h2o.deeplearning(seed = 1, x = predictors, y = target,
                              training_frame = h2o.iris, nfolds = 5)
h2o.confusionMatrix(deepCross5)
# ~15%, does it keep improving if we increase the fold count?
# Why? What are the implications of this?

## Task: Change the fold count to see how it can affect model _generalizability_

# Lastly, let's try to use a regularizer
deepCross5L1 = h2o.deeplearning(seed = 1, x = predictors, y = target,
                              training_frame = h2o.iris, nfolds = 5, l1 = 0.1)
h2o.confusionMatrix(deepCross5L1)

deepCross5L2 = h2o.deeplearning(seed = 1, x = predictors, y = target,
                                 training_frame = h2o.iris, nfolds = 5,
                                l2 = 0.1)
h2o.confusionMatrix(deepCross5L2)

# L1 and L2 regularization seem to negatively affect the model's
# performance here, may not be quite applicable due to the small number of
# features, categories and observations that we have; regularizers dissallow the
# model from becoming too complex, but our learning problem is not a particularly
# complex one.

# Return to the wiki and read through the rest of the available parameters and
# their uses

# ==== Part 3 ====
# Let's look at another problem:
data("ChickWeight")
dim(ChickWeight)
head(ChickWeight)
# Here we have the weight versus age of chicks on different diets. Here, we can
# set Diet as our learning objective
table(ChickWeight$Diet)
table(ChickWeight$Time)
length(unique(ChickWeight$Chick))


# Note that the only continuous variables here are the weights; time, chick and
# diet are only categories and thus must be converted to factors befor training
# It must also be noted that the data must be reshaped to a wider format
myWeights = melt(data = ChickWeight,
                 id.vars = c("Chick", "Diet", "Time"))[,c(1,2,3,5)]

myWeights$Chick = factor(x = myWeights$Chick, ordered = F)
myWeights$Diet = factor(myWeights$Diet, ordered = F)
myWeights$Time = factor(myWeights$Time, ordered = F)

colnames(myWeights)[4] = "Weight"

# We will again use cross validation, and see how new parameters can impact
# learning
h2o.chick = as.h2o(x = myWeights)
predictors = colnames(myWeights)[-2]
target = colnames(myWeights)[2]

# First, let's proceed wit the same setup as before
deepChicks = h2o.deeplearning(seed = 1, x = predictors, y = target,
                                     training_frame = h2o.chick, nfolds = 5)
h2o.confusionMatrix(deepChicks)
# ~62% error rate... this is not very good

# Let's see if we can improve this:
# 1. The data may be more complex and thus may need more layers, but at the same
# time since we only have 3 inputs the layer sizes can be smaller than the
# default 200.
# 2. There might be an exponential increase of signals from layer to layer; we
# can limit this by using a squishing activation funciton such as Tanh instead
# of ReLU
# 3. We may need more than the default 10 passes of the training set (epochs),
# if the data is too complex, we need much higher epochs.
# Let's modify and test the results; note that this may take a little longer
deepChicks2 = h2o.deeplearning(seed = 1, x = predictors, y = target,
                                training_frame = h2o.chick, nfolds = 5,
                                epochs = 10000, hidden = c(8,8,8,8),
                                activation = "Tanh")
h2o.confusionMatrix(deepChicks2)
# ~0.7% error rate. In conclusion, try to tweak these common parameters in case
# your initial error rates are too high.

## Task: Try to see if you can decrease training time while minimizing
# validation error.

# Return to the wiki and familiarize yourself with the last batch of important
# function parameters.

h2o.shutdown()
# ==== Part 4 ====
# Let's look at a gene annotation problem:
load("data/myGOExSet.RData")
dim(myGOExSet)
head(myGOExSet)
# Here, we have Gene Ontology annotation information appended to yeast gene
# time series expression data. We can set our learning objective as being able
# to correctly predict what GO term a gene falls under given a similar set of
# expression data.
# Our features would be the 12 data points and our target the annotated GO term:
predictors = colnames(myGOExSet)[-(1:4)]
target = colnames(myGOExSet)[3]

# As before, we must convert our categories to factors
myGOExSet$termName = factor(x = myGOExSet$termName, ordered = F)
# Note that we have 4 levels:
levels(myGOExSet$termName)
# Housekeeping genes (4 samples) are used as a control
h2o.init(port = 5001)
h2o.GO = as.h2o(myGOExSet)

# Note that since we are specifying our feature and target columns, we do not
# have to remove the ID, name and termID columns as they will be ignored.
# Let's test the usual setup
# We can set reproducible to TRUE in order to test against other benchmarks
# This slows down training as only one core is used
deepGO1 = h2o.deeplearning(seed = 1, x = predictors, y = target,
                           training_frame = h2o.GO, nfolds = 5)
h2o.confusionMatrix(deepGO1)
# 51% error rate (with reproducibility) Let's see if the same modifications from
# before can help

deepGO2 = h2o.deeplearning(reproducible = T, seed = 1, x = predictors,
                           y = target, training_frame = h2o.GO, nfolds = 5,
                           epochs = 10000, hidden = c(8,8,8,8),
                           activation = "Tanh")
h2o.confusionMatrix(deepGO2)
# ~48%, not quite there yet (apparently)

# Let's run it using multiple cores:
deepGO3 = h2o.deeplearning(reproducible = F, seed = 1, x = predictors,
                           y = target, training_frame = h2o.GO, nfolds = 5,
                           epochs = 10000, hidden = c(8,8,8,8),
                           activation = "Tanh")
h2o.confusionMatrix(deepGO3)

# Much lower (~6% in my case) error rate; whether this is a bug or a how the
# algorithm is setup for multiple cores, this is still quite strange.

# It has also been shown that hidden layer dropout improve model's performance
# on new data; this isn't the case for our data:
deepGO4 = h2o.deeplearning(reproducible = F, seed = 1, x = predictors,
                           y = target, training_frame = h2o.GO, nfolds = 5,
                           epochs = 10000, hidden = c(8,8,8,8),
                           activation = "TanhWithDropout",
                           input_dropout_ratio = 0.1)
h2o.confusionMatrix(deepGO4)


## As a final task, let's use the checkpoint parameter to train on new data.
# We can use the same dataset as above but here we are training on a larger
# portion first, then we will add new observations to re-train the model:

## Task: Randomly select 60% of the data from myGOEx, hold the other 40% for
# later use
# Make sure that each class is properly represented.

## Train a model without cross-validation
# Make sure that you use multiple cores to achieve the same efficiency

## Provide this model to the _checkpoint_ parameter and rerun the training on
# the leftover 40%. Ensure that the model achieves low error rates.
# Is it reasonable to change the function parameters on the second run?

# Refer to the wiki for a sample solution to this problem.

h2o.shutdown()


# Note that the remaining sections are for demonstration only and are not the
# focus of this unit. Feel free to read their documentation in the linked
# website if interested.
# ==== XGBoost ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/xgboost.html

# Preperation:
data("iris")
# Note that this task requires more RAM than others
h2o.init(max_mem_size = "3G",  port = 12345)
predictors = colnames(iris)[1:4]
target = colnames(iris)[5]
h2o.iris = as.h2o(iris)



XGBresults = h2o.xgboost(seed = 1, x = predictors, y = target,
                         training_frame = h2o.iris, nfolds = 5)
h2o.confusionMatrix(XGBresults)
# Apparently perfect

# Let's try it on the Chickweight data:
# Prepare the data as before
data("ChickWeight")
myWeights = melt(data = ChickWeight,
                 id.vars = c("Chick", "Diet", "Time"))[,c(1,2,3,5)]
myWeights$Chick = factor(x = myWeights$Chick, ordered = F)
myWeights$Diet = factor(myWeights$Diet, ordered = F)
myWeights$Time = factor(myWeights$Time, ordered = F)
colnames(myWeights)[4] = "Weight"
h2o.chick = as.h2o(x = myWeights)
predictors = colnames(myWeights)[-2]
target = colnames(myWeights)[2]

xgb.chicks = h2o.xgboost(seed = 1, x = predictors, y = target,
                         training_frame = h2o.chick, nfolds = 5)
h2o.confusionMatrix(xgb.chicks)
# Again, apparently perfect
# XGBoost is considered one of the best choices in many datascience
# applications

h2o.shutdown()

# ==== Auto ML ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html

# Here, a series of models are trained and a final predictive model is selected
# from an ensemble. Therefore, this procedure takes more computational resources.

# Preperation:
data("iris")
# Note that this task requires more RAM than others
h2o.init(port = 50001)

h2o.iris = as.h2o(iris)
predictors = colnames(iris)[-5]
target = colnames(iris)[5]

# Run autoML:
# By default, there will be 5-fold cross validation for each model
AutoMLresults = h2o.automl(seed = 1, x = predictors, y = target,
                           training_frame = h2o.iris)

h2o.shutdown()


# [END]

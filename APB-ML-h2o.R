# APB-ML-h2o_FINAL.R
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
#           1.0    (Final Submission)
#                     - Added missing package installations
#                     - Added synthetic data for pre-assessing models
#                     - Added grid parameter search
#                     - Added model checkpointing
#                     - Fixed typos
#                     - Replaced '=' with '<-'
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# =============== Part 1: Package Introduction and Installation ================
# H2O is an open-source platform focused on machine learning and artificial
# intelligence. It is continuously optimized for parallel and GPU computing.
# The following is an introduction to H2O for R.

# = 1.1 Installation
# Install and load the packages required for data reshaping, plotting and
# splitting:
if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(caret)) {
  install.packages("caret")
  library(caret)
}

# Ensure that a compatible JDK is installed on your computer: currently versions
# 1.6-1.8 are supported. (i.e. not JRE 8 or 9)
# http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html

# Download and install h2o from CRAN
# The installation may take a few mintues dependent on
# connection...
if (!require(h2o)) {
  install.packages("h2o")
  library(h2o)
}


# = 1.2 Setup
# After running library(h2o), the package is only loaded into the ram.
# In order to use the package, one must first initialize the engine; you can
# specify the amount of computational resources you wish to allow h2o to access.
?h2o.init

# nthreads = -1 uses all threads.
# Note that apparently in h2o, multicore processes are not quite reproducible,
# there’s one model per compute node, so multiple Mappers/threads share one
# model, which is why H2O is not reproducible unless a small dataset is used and
# force_load_balance=F or reproducible=T, which effectively rebalances to a
# single chunk and leads to only one thread to launch a map().
# thus we'll set nthreads = 1 for now.
# Found in the FAQ section:
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/deep-learning.html

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
tempIris <- as.data.table(iris)
# Turn the data into "long" form
moltenIris <- melt.data.table(data = tempIris, id.vars = "Species")
# Plot
irisPlot <- ggplot(data = moltenIris,
                   mapping = aes(x = Species, y = value, fill = Species)) +
  geom_bar(stat = "identity") +
  facet_grid(facets = ~variable) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Iris data overview")
irisPlot
# We can refer to this plot during training; its interpretation is currently not
# necessary

# =================== Part 2: Deep Learning ===================
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/deep-learning.html

# ==== Synthetic data predictions ====
# Let's start by creating a synthetic dataset where we can be certain of the
# labels assigned to the classes. Since we will be working with time-series
# data towards the end of the unit, we'll try to create a synthetic dataset that
# resembles time-series data over 8 time points:
# Create main classes:
class1 <- c(1, 1.5, 2, 3, 3, 2, 1.5, 1)
class2 <- c(3, 2.5, 2, 1, 1, 2, 2.5, 3)
class3 <- c(3, 2, 1, 2, 2, 1, 2, 3)
plot(class1, type = "l", col = "red")
lines(class2, col = "blue")
lines(class3, col = "black")

# Now add more observations for each class, each with a small deviance
set.seed(1)
class1_mat <- matrix(nrow = 100, ncol = 8)
for (i in 1:100) {
  class1_mat[i, ] <- rnorm(n = 8, mean = class1, sd = 0.1)
  lines(class1_mat[i,], type = "l", col = "red")
}
# Assign class labels, convert to data.table
class1_dt <- as.data.table(cbind(class1_mat, rep(1, 100)))

class2_mat <- matrix(nrow = 100, ncol = 8)
for (i in 1:100) {
  class2_mat[i, ] <- rnorm(n = 8, mean = class2, sd = 0.1)
  lines(class2_mat[i,], type = "l", col = "blue")
}
# Assign class labels, convert to data.table
class2_dt <- as.data.table(cbind(class2_mat, rep(2, 100)))

class3_mat <- matrix(nrow = 100, ncol = 8)
for (i in 1:100) {
  class3_mat[i, ] <- rnorm(n = 8, mean = class3, sd = 0.1)
  lines(class3_mat[i,], type = "l", col = "black")
}
# Assign class labels, convert to data.table
class3_dt <- as.data.table(cbind(class3_mat, rep(3, 100)))

# Now that we have 100 synthetic observations for each class, we will use a deep
# learning model and test how well it can predict the labels.

# Append all train data into one data.table:
train_data <- rbindlist(list(class1_dt, class2_dt, class3_dt))
# Assign column names
colnames(train_data) <- c(paste0("t", seq(1, 8)), "label")
# Convert labels to categorical variables
train_data$label <- as.factor(train_data$label)
head(train_data)

train_h2o = as.h2o(train_data)
# Train a DNN:
synthetic.DNN <-
  h2o.deeplearning(seed = 1, x = colnames(train_data)[1:8],
                   y = "label",
                   training_frame = train_h2o, nfolds = 5)
synthetic.DNN

# As shown, the cross validation accuracy for all 5 folds is 100%; this means
# that using 80% of the data at each 5 fold assignments, the label of the
# remaining 20% was correctly predicted.


# ==== Real data predictions ====
# Sources:
# Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems.
# Annals of Eugenics, 7, Part II, 179–188.
# The data were collected by Anderson, Edgar (1935). The irises of the Gaspe
# Peninsula, Bulletin of the American Iris Society, 59, 2–5.

# We have already loaded the iris data, let's first have a look at what the
# data looks like
class(iris)
typeof(iris)
head(iris)
# As you can see, our learning problem is to use the 4 independent variable
# columns Sepal.Length, Sepal.Width, Petal.Length and Petal.Width to predict the
# dependent variable Species. Since our targets are categorical and contain
# multiple classes, this is a multi-class classification problem.
predictors <- colnames(iris)[1:4]
target <- colnames(iris)[5]

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
# We'll use a sampling function from the caret package that takes care of this:
set.seed(1)
myIdx = caret::createDataPartition(y = iris$Species, p = 0.8, list = F)
# Validate that classes are equally distributed:
table(iris$Species[myIdx])
dim(iris)
# We first must convert our data.frame to an h2o.frame; this makes our data
# available to the JVM that h2o runs on:
trainingSet <- as.h2o(iris[myIdx,])
testingSet <- as.h2o(iris[-myIdx,])
# Validate:
table(iris[myIdx,]$Species)
table(iris[-myIdx,]$Species)

# Note that we will not have regularization here
# Train
deepNoCross <- h2o.deeplearning(seed = 1, standardize = T, x = predictors,
                                y = target, training_frame = trainingSet,
                                validation_frame = testingSet, nfolds = 0)

# One of the simplest ways of presenting the performance of an ML model is a
# confusion matrix. h2o automatically creates many other metrics for us as well:
h2o.confusionMatrix(deepNoCross)
# As you can see, we have ~18% error rate, where virginica was mistaken for
# versicolor

# If we refer back to our plot,
irisPlot
# We can see that versicolor and virginical are quite a lot closer to to each
# other across the 4 features compared to setosa. Let's see if we can improve
# our predictions.

## Task: Modify the testing/validation set ratios and see which ratio will give
# use minimum error. Ensure that you set seed correctly each time.

# Let's train on our data again without standardization:
deepNoCrossNoStand <- h2o.deeplearning(seed = 1, standardize = F, x = predictors,
                                       y = target, training_frame = trainingSet,
                                       validation_frame = testingSet, nfolds = 0)

h2o.confusionMatrix(deepNoCrossNoStand)
# ~33% error rate, this is why it's best to standardize the data in such cases.
# Fortunately, it is enabled by default, so we will not need to specify it.

# Now we will use different cross validation schemes to see if it can make our
# model more generalizable
# Note that we will not need our testing set anymore
h2o.iris <- as.h2o(iris)
deepCross2 <- h2o.deeplearning(seed = 1, x = predictors, y = target,
                               training_frame = h2o.iris, nfolds = 2)
h2o.confusionMatrix(deepCross2)
# We can see that our with 2 folds, the error rate is at ~19%. It is common
# practice that 5 folds are chosen:
deepCross5 <- h2o.deeplearning(seed = 1, x = predictors, y = target,
                               training_frame = h2o.iris, nfolds = 5)
h2o.confusionMatrix(deepCross5)
# ~15%, does it keep improving if we increase the fold count?
# Why might that be? We are allowing the model to use more datapoints as the
# training data as we increase the fold count:
# 5 folds -> 80/20 train-test split at each validation stage
# 2 folds -> 50/50 train-test split at each validation stage

## Task: Change the fold count to see how it can affect model _generalizability_

# Lastly, let's try to use a regularizer
deepCross5L1 <- h2o.deeplearning(seed = 1, x = predictors, y = target,
                                 training_frame = h2o.iris, nfolds = 5, l1 = 0.1)
h2o.confusionMatrix(deepCross5L1)

deepCross5L2 <- h2o.deeplearning(seed = 1, x = predictors, y = target,
                                 training_frame = h2o.iris, nfolds = 5,
                                 l2 = 0.1)
h2o.confusionMatrix(deepCross5L2)

# L1 and L2 regularization seem to negatively affect the model's
# performance here, may not be quite applicable due to the small number of
# features, categories and observations that we have; regularizers dissallow the
# model from becoming too complex, but our learning problem is not a particularly
# complex one. Think about the reasons as to why L1 and L2 regularization do not
# help here.

# Return to the wiki and read through the rest of the available parameters and
# their uses

# =================== Part 3 ===================
# Sources:
# Crowder, M. and Hand, D. (1990), Analysis of Repeated Measures, Chapman and Hall
# (example 5.3)
# Hand, D. and Crowder, M. (1996), Practical Longitudinal Data Analysis, Chapman
# and Hall (table A.2)
# Pinheiro, J. C. and Bates, D. M. (2000) Mixed-effects Models in S and S-PLUS,
# Springer.

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
myWeights <- melt(data = ChickWeight,
                  id.vars = c("Chick", "Diet", "Time"))[,c(1,2,3,5)]

myWeights$Chick <- factor(x = myWeights$Chick, ordered = F)
myWeights$Diet <- factor(myWeights$Diet, ordered = F)
myWeights$Time <- factor(myWeights$Time, ordered = F)

colnames(myWeights)[4] <- "Weight"

# We will again use cross validation, and see how new parameters can impact
# learning
h2o.chick <- as.h2o(x = myWeights)
predictors <- colnames(myWeights)[-2]
target <- colnames(myWeights)[2]

# First, let's proceed wit the same setup as before
deepChicks <- h2o.deeplearning(seed = 1, x = predictors, y = target,
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
deepChicks2 <- h2o.deeplearning(seed = 1, x = predictors, y = target,
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

# =================== Part 4 ===================
# Let's look at a gene annotation problem:
load("data/myGOExSet.RData")
dim(myGOExSet)
head(myGOExSet)
# Here, we have Gene Ontology annotation information appended to yeast gene
# time series expression data. We can set our learning objective as being able
# to correctly predict what GO term a gene falls under given a similar set of
# expression data.
# Our features would be the 12 data points and our target the annotated GO term:
predictors <- colnames(myGOExSet)[-(1:4)]
target <- colnames(myGOExSet)[3]

# As before, we must convert our categories to factors
myGOExSet$termName <- factor(x = myGOExSet$termName, ordered = F)
# Note that we have 4 levels:
levels(myGOExSet$termName)
# Housekeeping genes (4 samples) are used as a control
h2o.GO <- as.h2o(myGOExSet)

# Note that since we are specifying our feature and target columns, we do not
# have to remove the ID, name and termID columns as they will be ignored.
# Let's test the usual setup
# We can set reproducible to TRUE in order to test against other benchmarks
# This slows down training as only one core is used
deepGO1 <- h2o.deeplearning(seed = 1, x = predictors, y = target,
                            training_frame = h2o.GO, nfolds = 5)
h2o.confusionMatrix(deepGO1)
# 51% error rate (with reproducibility turned on). Let's see if the same
# modifications from before can help

deepGO2 <- h2o.deeplearning(reproducible = T, seed = 1, x = predictors,
                            y = target, training_frame = h2o.GO, nfolds = 5,
                            epochs = 10000, hidden = c(8,8,8,8),
                            activation = "Tanh")
h2o.confusionMatrix(deepGO2)
# ~46%, not quite there yet (apparently)

# Let's run it using multiple cores:
deepGO3 <- h2o.deeplearning(reproducible = F, seed = 1, x = predictors,
                            y = target, training_frame = h2o.GO, nfolds = 5,
                            epochs = 10000, hidden = c(8,8,8,8),
                            activation = "Tanh")
h2o.confusionMatrix(deepGO3)

# Much lower (~1.5% in my case) error rate; whether this is a bug or a how the
# algorithm is setup for multiple cores, this is still quite strange.

# It has also been shown that hidden layer dropout improve model's performance
# on new data
deepGO4 <- h2o.deeplearning(reproducible = F, seed = 1, x = predictors,
                            y = target, training_frame = h2o.GO, nfolds = 5,
                            epochs = 10000, hidden = c(8,8,8,8),
                            activation = "TanhWithDropout",
                            input_dropout_ratio = 0.1)
h2o.confusionMatrix(deepGO4)
# Unfortunately, this isn't the case for our data (more hidden units or layers
# may be required in our case)

# Finally, we can save an h2o model and reload at a later point to test on new
# data. We'll save the deepGO3 as it had the best performance:
h2o.saveModel(deepGO3, "h2o.model_deepGO3")

# Remove the model (from the cluster)
h2o.rm(deepGO3)

# Remove the reference in current R session:
rm(deepGO3)

# Now reload (note that models are saved to a _folder_ of the given name, so if
# there are multiple models in the specified folder, you must provide the path
# to that model
reloaded_model = h2o.loadModel("h2o.model_deepGO3")

# ==== Short introduction to checkpoints ====
## As a final task, let's use the checkpoint parameter to train on new data.
# We can use the same dataset as above but here we are training on a larger
# portion first, then we will add new observations to extend the model:

## Task: Randomly select 80% of the myGOEx data, then out of this partition
# select 60%, hold the other 40% for later use
# Make sure that each class is properly represented.

## Train a model without cross-validation on the first 60% of the train data
# Make sure that you use multiple cores to achieve the same efficiency
# Note: The training parameters must be the same to use checkpoints!

## Provide this model to the _checkpoint_ parameter and rerun the training on
# the leftover 40%. Ensure that the model achieves low error rates.


# __Refer to the wiki for a sample solution to this problem.__


# Shut down the virtual machine once you are done with h2o;
# Note that this will remove the instances of h2o objects, thus you must save
# any models you wish to keep
# ?h2o.saveModel
h2o.shutdown()



# Note that the remaining sections are for demonstration only and are not the
# focus of this unit. Feel free to read their documentation in the linked
# website if interested.
# NOTE: Grid paramter search and AutoML are computationally intensive and take
# a longer time to finish.

# ==== Grid Parameter Search ====
# As we have seen, many of the functions provided by h2o allow a variety of
# parameters to be set; most often, these parameters cannot be simply decided
# beforehand (i.e. by domain knowledge), and fine-tuning is required. h2o
# provides a grid parameter search option that allows us to choose a selection
# of values to try for a given algorithm and report the performance of each
# model based on possible combinations of those parameters.

# Load yeast gene expression data and prepare as before:
load("data/myGOExSet.RData")
dim(myGOExSet)
head(myGOExSet)
predictors <- colnames(myGOExSet)[-(1:4)]
target <- colnames(myGOExSet)[3]
myGOExSet$termName <- factor(x = myGOExSet$termName, ordered = F)
levels(myGOExSet$termName)

h2o.init(port = 5001)
h2o.GO <- as.h2o(myGOExSet)

# Let's use grid parameter search to find the best combination of parameters
# (from those that we provide) for deep learning; the parameters must be passed
# as a named list of valid arguments:
?h2o.deeplearning
?h2o.grid
# For paramters that are in vector form, we must use a list (e.g. hidden layers)
myParams <- list(nfolds = c(5, 10), activation = c("Tanh", "Rectifier", "Maxout"),
                 hidden = list(c(10, 10, 10), c(8, 8, 8, 8)),
                 epochs = c(10, 100, 1000),
                 # Check how different methods of weight initialization
                 # affect model performance
                 initial_weight_distribution = c("UniformAdaptive", "Uniform",
                                                 "Normal"),
                 loss = c("CrossEntropy", "Huber", "Absolute"))

# Note that we have 2 * 3 * 2 * 3 * 3 * 3 = 324 possible combinations!
# This is why this paramter search takes a long time.

# Setup the DNN algorithm like before, and pass the parameters list for tuning
deep.grid <- h2o.grid(algorithm = "deeplearning", grid_id = "Deep.Grid", seed = 1,
                      x = predictors, y = target, training_frame = h2o.GO,
                      hyper_params = myParams)

# Save grid results (must be saved as an RDS or RData file)
saveRDS(deep.grid, "Grid_Search_Results.rds")

h2o.shutdown()
# In the following algorithms, a series of models are trained and a final
# predictive model (or a collection in XGBoost) is selected
# Therefore, these procedures take more computational resources, and are
# provided for interest only.

# Note that these two models are still in beta phase and may produce errors;
# check h2o's website before proceeding.
# ==== XGBoost ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/xgboost.html

# Preperation:
data("iris")
# Note that this task requires more RAM than others
h2o.init(max_mem_size = "3G",  port = 12345)
predictors <- colnames(iris)[1:4]
target <- colnames(iris)[5]
h2o.iris <- as.h2o(iris)



XGBresults <- h2o.xgboost(seed = 1, x = predictors, y = target,
                          training_frame = h2o.iris, nfolds = 5)
h2o.confusionMatrix(XGBresults)
# Seems perfect (!)

# Let's try it on the Chickweight data:
# Prepare the data as before
data("ChickWeight")
myWeights <- melt(data = ChickWeight,
                  id.vars = c("Chick", "Diet", "Time"))[,c(1,2,3,5)]
myWeights$Chick <- factor(x = myWeights$Chick, ordered = F)
myWeights$Diet <- factor(myWeights$Diet, ordered = F)
myWeights$Time <- factor(myWeights$Time, ordered = F)
colnames(myWeights)[4] = "Weight"
h2o.chick <- as.h2o(x = myWeights)
predictors <- colnames(myWeights)[-2]
target <- colnames(myWeights)[2]

xgb.chicks <- h2o.xgboost(seed = 1, x = predictors, y = target,
                          training_frame = h2o.chick, nfolds = 5)
h2o.confusionMatrix(xgb.chicks)
# Again, apparently perfect

# Note that XGBoost is considered one of the best choices in many datascience
# applications, and is used by many in data science competitions.

h2o.shutdown()

# ==== Auto ML ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html


# Preperation:
data("iris")
# Note that this task requires more RAM than others
h2o.init(port = 50001)

h2o.iris <- as.h2o(iris)
predictors <- colnames(iris)[-5]
target <- colnames(iris)[5]

# Run autoML:
# By default, there will be 5-fold cross validation for each model
AutoMLresults <- h2o.automl(seed = 1, x = predictors, y = target,
                            training_frame = h2o.iris)

h2o.shutdown()

# [END]

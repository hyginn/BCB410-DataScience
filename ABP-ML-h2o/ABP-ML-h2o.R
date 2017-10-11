# APB-ML-h2o.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the APB-ML-h2o unit.
#
# Version:  0.1
#
# Date:     2017  10  05
# Author:   Farzan Taj (farzantaj@outlook.com)
#
# Versions:
#           0.1    (Initial Submission)

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

# ==== 1 Package Installation and Introduction ====
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
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# The is a direct download from the h2o repository, which is usually 1-2
# releases ahead of CRAN. The installation may take a few mintues dependent on
# connection...
repos <- c("https://h2o-release.s3.amazonaws.com/h2o/rel-turing/9/R",
           getOption("repos"))
install.packages("h2o", type="source", repos=repos)
library(h2o)

# = 1.2 Setup
# After running library(h2o), the package is only loaded into the ram.
# In order to use the package, one must first initialize the engine; you can
# specify the amount of computational resources you wish to allow h2o to access.
# In the case you do not have a compatible JDK installed, you will be prompted
# to download and install the JDK from Oracle.

?h2o.init
# Limit CPU and RAM usage for now
# Here a port is manually selected to avoid IPv6 issues
h2o.init(nthreads = -1, max_mem_size = '1G', ip = "127.0.0.1", port = 50001)


# Important note on tasks:
# For each algorithm, 165 classifiers are built for the yesat
# GSE4987 times series expression data with GOslim terms as targets.
# Since this takes long, you can simply select one column (GOslim term) and only
# train a binary classifer. Ensure that the one-hot encoded columns are either of
# type(logical) or have two factor levels (made with factor())

# ===== Dependency Check =====
if (!file.exists("GSE4987_prep.R")) {
  stop("The GSE4987 preparation script could not be found!")
}
if (!file.exists("GSE4987_GOslim.R")) {
  stop("The GSE4987 GOslim annotation script could not be found!")
}


# ===== Unsupervised learning =====
# ==== k-Means Clustering ====
source("GSE4987_prep.R")
h2o.init(max_mem_size = "1G")
a30 = fread("data/a30.txt")

# prosPath <- system.file("extdata", "prostate.csv", package="h2o")
# prostate.hex <- h2o.uploadFile(path = prosPath)
# Replace gene IDs with numerical IDs

myGenes = a30[,1]
a30[,1] = seq(1, nrow(a30))
h2o.a30 = as.h2o(a30)

# Set k to 169, based on the available GOslim annotations
# Allow the algorithm to guess the number of clusters
kMeansModel = h2o.kmeans(training_frame = h2o.a30, model_id = "a30_kMeans",
                         k = 169, estimate_k = T, standardize = F,
                         x = colnames(h2o.a30)[-1], seed = 1)

saveRDS(kMeansModel, "data/kMeansModel.RDS")
# ==== Principal Component Analysis ====
source("GSE4987_prep.R")
h2o.init(max_mem_size = '1G')

a30 = fread("data/a30.txt")
myGenes = a30[,1]
a30[,1] = seq(1, nrow(a30))
h2o.a30 = as.h2o(a30)
summary(h2o.a30)
predictors = colnames(h2o.a30)[-1]
PCAmodel = h2o.prcomp(training_frame = h2o.a30, x = predictors,
                      model_id = "a30_PCA", k = 25, max_iterations = 3000,
                      impute_missing = T, seed = 1)

saveRDS(PCAmodel, "data/PCAmodel.RDS")


# ===== Supervised Learning =====
# ==== Distributed Random Forests ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/drf.html

# Preperation:
source("GSE4987_prep.R")
h2o.init(max_mem_size = "2G")
# Read one-hot encoding of GeneIDs with GOslim labels
a30_onehot = fread("data/GSE4987_GOslim_onehot.txt")
logical = a30_onehot[,-(1:26)] == 1
logical = cbind(a30_onehot[,1:26], logical)
a30 = fread("data/a30.txt")

# Separate unannotated genes
idx = fmatch(a30_onehot$GeneID, a30$myGenes, nomatch = 0)
unannot = a30[-idx]

# Create label vector
labels = colnames(a30_onehot[,-(1:26)])
# Get the feature names (5-min expression data)
featuresNames = colnames(a30)[-1]

myGenes = a30_onehot[,1]

# Randomly subset 80% as training, the rest as validation data
set.seed(1)
samp = sample(x = nrow(a30_onehot), size = floor(.8 * (nrow(a30_onehot))),
              replace = F)

# Separate and convert to h2o frame
newData = unannot

logicalTrainData = logical[samp,]
logicalTestData = logical[-samp,]

h2o.DRF.Log.train = as.h2o(logicalTrainData, destination_frame = "h2o.DRF.Log.train")
h2o.DRF.Log.test = as.h2o(logicalTestData, destination_frame = "h2o.DRF.Log.test")

# trainData = a30_onehot[samp,]
# testData = a30_onehot[-samp,]
# h2o.DRF.train = as.h2o(trainData, destination_frame = "h2o.DRF.train")
# h2o.DRF.test = as.h2o(testData, destination_frame = "h2o.DRF.test")


# Train a classification random forest on each label (while ignoring the others)
DRFresults = vector(mode = "list", length = length(labels))
i = 1
for (lab in labels) {
  # Can pass either the model_id (character) or the h2o.frame object
  DRFresults[i] <- h2o.randomForest(x = featuresNames, y = lab,
                                    training_frame = "h2o.DRF.Log.train",
                                    validation_frame = "h2o.DRF.Log.test",
                                    nfolds = 5, min_rows = 2)
  # Why provide both cross validation fold count and a validation frame???
  i <- i + 1
}

# Save
saveRDS(object = DRFresults, file = "data/DRFresults.rds")

# Upon prediction, we would also have to predict across all models:
new_data = as.h2o(newData, destination_frame = "newData")
DRF_model = DRFresults[[1]]
# typeof(DRF_model)
# class(DRF_model)

getPredictions <- function(DRF_model, new_data) {
  curPred = predict(object = DRF_model, newdata = new_data)
}

# h2o.shutdown()
# ==== Deep Learning ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/deep-learning.html

# Preperation:
source("GSE4987_prep.R")
h2o.init(max_mem_size = "2G")
# Read one-hot encoding of GeneIDs with GOslim labels
a30_onehot = fread("data/GSE4987_GOslim_onehot.txt")
logical = a30_onehot[,-(1:26)] == 1
logical = cbind(a30_onehot[,1:26], logical)
a30 = fread("data/a30.txt")

# Separate unannotated genes
idx = fmatch(a30_onehot$GeneID, a30$myGenes, nomatch = 0)
unannot = a30[-idx]

# Create label vector
labels = colnames(a30_onehot[,-(1:26)])
# Get the feature names (5-min expression data)
featuresNames = colnames(a30)[-1]

myGenes = a30_onehot[,1]

# Randomly subset 80% as training, the rest as validation data
# set.seed(1)
# samp = sample(x = nrow(a30_onehot), size = floor(.8 * (nrow(a30_onehot))),
#               replace = F)
#
# # Separate and convert to h2o frame
# newData = unannot
#
# logicalTrainData = logical[samp,]
# logicalTestData = logical[-samp,]

# h2o.deep.Log.train = as.h2o(logicalTrainData, destination_frame = "h2o.DRF.Log.train")
# h2o.deep.Log.test = as.h2o(logicalTestData, destination_frame = "h2o.DRF.Log.test")

# OR perform cross validation
fold_count = 5
h2o.deep.Log.train = as.h2o(logical, destination_frame = "h2o.deep.Log.train")

# Train a deep learning model on each label (while ignoring the others)
deepResults = vector(mode = "list", length = length(labels))
i = 1
for (lab in labels) {
  # TODO: Should we standardize the data?
  # no standardization: 0.07 error
  # with standardization: 0.17 error
  deepResults[i] <- h2o.deeplearning(x = featuresNames, y = lab,
                                     training_frame = "h2o.deep.Log.train",
                                     nfolds = fold_count, standardize = F)
  i <- i + 1
  # TODO: Consier autoencoders for dimensionality reduction
}

# Save
saveRDS(object = deepResults, file = "data/deepResults.rds")


# ==== Naive Bayes Classifier ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/naive-bayes.html
# "Naïve Bayes is a classification algorithm that relies on strong assumptions
# of the independence of covariates in applying Bayes Theorem."
# It is understandable that the GO terms are not entirely independent, but we
# can try and see how much loss we have on our testing set.

# Preperation:
source("GSE4987_prep.R")
h2o.init(max_mem_size = "2G")
# Read one-hot encoding of GeneIDs with GOslim labels
a30_onehot = fread("data/GSE4987_GOslim_onehot.txt")
logical = a30_onehot[,-(1:26)] == 1
logical = cbind(a30_onehot[,1:26], logical)
a30 = fread("data/a30.txt")

# Separate unannotated genes
idx = fmatch(a30_onehot$GeneID, a30$myGenes, nomatch = 0)
unannot = a30[-idx]

# Create label vector
labels = colnames(a30_onehot[,-(1:26)])
# Get the feature names (5-min expression data)
featuresNames = colnames(a30)[-1]

myGenes = a30_onehot[,1]

# Perform cross validation
fold_count = 5
h2o.bayes.Log.train = as.h2o(logical, destination_frame = "h2o.bayes.Log.train")

bayesResults = h2o.naiveBayes(x = featuresNames, y = lab,
                              training_frame = "h2o.bayes.Log.train",
                              nfolds = fold_count)

bayesResults@model$cross_validation_metrics_summary
# Not very accurate, confirming that GO term interdependence may affect the
# results.

# ==== Gradient Boosting Machine ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/gbm.html

# Preperation:
source("GSE4987_prep.R")
# Note that this task requires more RAM than others
h2o.init(max_mem_size = "3G")
# Read one-hot encoding of GeneIDs with GOslim labels
a30_onehot = fread("data/GSE4987_GOslim_onehot.txt")
logical = a30_onehot[,-(1:26)] == 1
logical = cbind(a30_onehot[,1:26], logical)
a30 = fread("data/a30.txt")

# Separate unannotated genes
idx = fmatch(a30_onehot$GeneID, a30$myGenes, nomatch = 0)
unannot = a30[-idx]

# Create label vector
labels = colnames(a30_onehot[,-(1:26)])
# Get the feature names (5-min expression data)
featuresNames = colnames(a30)[-1]

myGenes = a30_onehot[,1]

# Perform cross validation
fold_count = 5
h2o.gbm.Log.train = as.h2o(logical, destination_frame = "h2o.gmb.Log.train")


# Train a GBM learning model on each label (while ignoring the others)
gbmResults = vector(mode = "list", length = length(labels))
i = 145
for (lab in labels[145:length(labels)]) {
  gbmResults[i] = h2o.gbm(x = featuresNames, y = lab,
                          training_frame = "h2o.gmb.Log.train",
                          nfolds = fold_count, min_rows = 2)
  i <- i + 1
}

# Save
saveRDS(object = gbmResults, file = "data/gbmResults.rds")

h2o.shutdown()


# ==== XGBoost ====
http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/xgboost.html

# Preperation:
source("GSE4987_prep.R")
# Note that this task requires more RAM than others
h2o.init(max_mem_size = "3G",  port = 12345)
# Read one-hot encoding of GeneIDs with GOslim labels
a30_onehot = fread("data/GSE4987_GOslim_onehot.txt")
logical = a30_onehot[,-(1:26)] == 1
logical = cbind(a30_onehot[,1:26], logical)
a30 = fread("data/a30.txt")

# Separate unannotated genes
idx = fmatch(a30_onehot$GeneID, a30$myGenes, nomatch = 0)
unannot = a30[-idx]

# Create label vector
labels = colnames(a30_onehot[,-(1:26)])
# Get the feature names (5-min expression data)
featuresNames = colnames(a30)[-1]

myGenes = a30_onehot[,1]

# Perform cross validation
fold_count = 5
h2o.XGB.Log.train = as.h2o(logical, destination_frame = "h2o.XGB.Log.train")


# Train a GBM learning model on each label (while ignoring the others)
XGBresults = vector(mode = "list", length = length(labels))
i = 1
for (lab in labels) {
  # Min observations in leaf set to 4 seems to provide optimal results
  XGBresults[i] = h2o.xgboost(x = featuresNames, y = lab,
                              training_frame = "h2o.XGB.Log.train",
                              nfolds = fold_count, min_rows = 4)
  i <- i + 1
}

# Save
saveRDS(object = XGBresults, file = "data/XGBresults.rds")
h2o.shutdown()

# ==== Auto ML ====
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html

# Here, a series of models are trained and a final predictive model is selected
# from an ensemble. Therefore, this procedure takes more computational resources.

# Load file into a variable of choice
temp = load(file = "./data/myGOExSet.RData")
smallSet = get(temp)
rm(temp)

# Preperation:
source("PackagePrep.R")
# Note that this task requires more RAM than others
h2o.init(max_mem_size = "6G", port = 12345)

nrow(smallSet) # 281 rows

# Find control genes for normalization
trainAndTestSet = smallSet[smallSet$termID != "000000", ]
unique(smallSet$termName)

trainAndTestSet$termID = factor(trainAndTestSet$termID)

# Assign predictor variable column names
predictorNames = colnames(trainAndTestSet)[-(1:4)]

h2o.smallSet = as.h2o(x = trainAndTestSet, destination_frame = "SmallSetAutoML")

# Run autoML:
# Use "termID" as target ID
# By default, there will be 5-fold cross validation for each model
AutoMLresults = h2o.automl(x = predictorNames, y = "termID",
                           training_frame = "SmallSetAutoML", seed = 1)

# Save
saveRDS(object = autoMLresults, file = "data/autoMLresults.rds")
h2o.shutdown()

# ==== Run all algorithms on the small GOExSet ====

# Preparation:
source("PackagePrep.R")

# Load file into a variable of choice
temp = load(file = "./data/myGOExSet.RData")
smallSet = get(temp)
rm(temp)

# Find control genes for normalization
trainAndTestSet = smallSet[smallSet$termID != "000000", ]
unique(trainAndTestSet$termName)

# Assign predictor variable column names
predictorNames = colnames(trainAndTestSet)[-(1:4)]

# Initialize h2o
h2o.init(max_mem_size = "2G", port = 50001)

# Create h2o.frame
h2o.smallSet = as.h2o(x = trainAndTestSet, destination_frame = "SmallSetAll")


# Train all models on the dataset and save their results:
## K-Means
smallKmeans = h2o.kmeans(x = predictorNames, training_frame = "SmallSetAll",
                         nfolds = 5, estimate_k = T, seed = 1, k = 3,
                         standardize = T)
saveRDS(smallKmeans, "data/smallKmeans.RDS")

## Distributed Random Forest
smallDRF = h2o.randomForest(x = predictorNames, y = "termID",
                            training_frame = "SmallSetAll", nfolds = 5,
                            min_rows = 2, seed = 1)
saveRDS(smallDRF, "data/smallDRF.RDS")

## Naive Bayes Classifier
smallNBS = h2o.naiveBayes(x = predictorNames, y = "termID",
                          training_frame = h2o.smallSet, nfolds = 5,
                          score_each_iteration = T, seed = 1)

## Gradient Boosting Machine
smallGBM = h2o.gbm(x = predictorNames, y = "termID",
                   training_frame = h2o.smallSet, nfolds = 5, seed = 1)
saveRDS(smallGBM, "data/smallGBM.RDS")

## XGBoost
smallXGB = h2o.xgboost(x = predictorNames, y = "termID",
                       training_frame = h2o.smallSet, nfolds = 5,
                       min_rows = 2, seed = 1)
saveRDS(smallXGB, "data/smallXGB.RDS")
# Very interesting, both the GBM and XGB have zero error rate
# (TODO investigate why)

## Deep Neural Network
smallDeep = h2o.deeplearning(x = predictorNames, y = "termID",
                             training_frame = h2o.smallSet,
                             nfolds = 5, seed = 1)
saveRDS(smallDeep, "data.smallDeep.RDS")

# Current data is not predictive at all except for in XGBoost and GBM,
# at least not on its own... (unless classifier was incorrectly set up)

h2o.shutdown()




h2o.shutdown()
# = 99  Task solutions


# = 99.1  Task 1: ...

# = 99.2  Task 2: ...


# [END]

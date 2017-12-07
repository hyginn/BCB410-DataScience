# APB-ML-RWeka.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the APB-ML-RWeka unit.
#
# Version:  0.4
#
# Date:     2017  12  05
# Author:   Truman Wang (truman.wang@mail.utoronto.ca)
#
# Versions:
#           0.1 (Initial design)
#           0.2 (Incorporated feedback from Code Review)
#           0.3 (Added sources for code examples)
#           0.4 (Added more interpretation of results)

# License: GPL-3 https://www.gnu.org/licenses/gpl-3.0.en.html

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

# = 1 Installing RWeka

# IF YOU ARE USING A MAC AND ARE GETTING ERRORS RUNNING
# "library(RWeka)" THEN RUNNING THIS COMMAND MAY FIX IT:
# sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib

# Installing some packages

# The main RWeka package, which contains all the RWeka libraries.
if (!require(RWeka, quietly = TRUE)) {
  install.packages("RWeka")
  library(RWeka)
}

# Needed to load some packages from github for high performance linear algebra.
# This package is not necessary for the code to run, however you will encounter an error
# if it is not loaded.
if(!require(partykit, quietly = TRUE)) {
  install.packages("partykit")
  library(partykit)
}

# Needed to load some packages from github for high performance linear algebra.
# This package is not necessary for the code to run, however you will encounter an error
# if it is not loaded.
if(!require(party, quietly = TRUE)) {
  install.packages("party")
  library(party)
}

# Contains the Pima Indians dataset that we will be analyzing
if (!require(MASS, quietly = TRUE)) {
  install.packages("MASS")
  library(MASS)
}

# = 2 Package manipulation/Environment setup

# Refresh the Weka package cache
WPM("refresh-cache")
# List already installed packages
WPM("list-packages", "installed")
# list packages available for installation
WPM("list-packages", "available")

# To install a package run "WPM("install-package", <package>)
# To remove a package run "WPM("remove-package", <package>)

# = 4 examples using RWeka

# Runnning some tests on the iris dataset. This dataset contains data on three different flowers,
# setosa, versicolor, and virginica. Each flower has data on its sepal length, sepal width, petal
# length, and petal width. We will be building a simple classifier using the built in J48 algorithm
# in RWeka.

# Code example from: Hornik, K (2017) RWeka Odds and Ends
# [Source code]https://cran.r-project.org/web/packages/RWeka/vignettes/RWeka.pdf

# Code example from: fibosworld (2013) R talks to Weka about Data Mining
# https://www.r-bloggers.com/r-talks-to-weka-about-data-mining/

# R.A. Fischer 1936, (Iris dataset)[Dataset] retrieved from:
# https://archive.ics.uci.edu/ml/datasets/iris

# Sneak peek of the dataset
head(iris)
summary(iris)

# C4.5 classifier - this classifier generates a decision tree
iris_j48 <- J48(Species~., data = iris)
iris_j48
summary(iris_j48)

# 10 folds cross validation using a C4.5 classifier

eval_j48 <- evaluate_Weka_classifier(iris_j48, numFolds = 10, complexity = FALSE,
                                     seed = 1, class = TRUE)
eval_j48

# Load example dataset (GOSlim dataset)
load(file = "./data/myGOExSet.RData")

# sample classifier for GOSlim terms using C4.5 classifier.

data <- myGOExSet

# Since multiple genes can have multiple GOSlim terms I decided to use a binary classifier to classify
# one specific gene. This idea can be expanded to classify all 165 different GOSlim terms".

# Converts Strings to one-hot encoding representations
data$termName <- as.factor(data$termName)

# Selects the GOSlim term names, and the columns containing its associated gene expression data
dataset <- data[, c(3,5:15)]

# Removes rows that contain NA from the dataset
dataset <- na.omit(dataset)

# Uses the C4.5 algorithm to classify the GOSlim termNames
GOSlim_j48 <- J48(termName ~., data = dataset)
summary(GOSlim_j48)

# Lets evaluate our J48 classifier using k-folds cross validation
eval_GOSlim_j48 <- evaluate_Weka_classifier(GOSlim_j48, numFolds = 10, complexity = FALSE,
                                            seed = 1, class = TRUE)
eval_GOSlim_j48
# Unfortunately, we weren't able to classify the termNames very well. This is most likely due to
# the GOterms having similar gene expression data, making it very difficult for the C4.5 algorithm
# to properly classify the terms. Also the C4.5 algorithm often works better on larger training sets.

# Octavian (2011, Mar 25) Decision Trees - C4.5 retrieved from:
# https://octaviansima.wordpress.com/2011/03/25/decision-trees-c4-5/

# ======================= Linear Regression =========================================
# RWeka does not support multinomial linear regression so we will be training a model
# on the mtcars dataset using Linear Regression. This data set contains information
# for a collection of cars

# Motor Trend US magazine 1974, (mtcars dataset)[Dataset]
# retrieved from: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html
# Sneak peek of the data
head(mtcars)

linear_reg_dataset <- mtcars

# Runs linear regression on the linear_reg_dataset, where mpg is the dependent variable
mtcars_linear_regression <- LinearRegression(mpg ~., data = linear_reg_dataset)

# Shows the associated weights and base value that linear regression has assigned to model
# the mtcars dataset.
mtcars_linear_regression

# Shows different error values - gives a measurement of how "good" our model is
summary(mtcars_linear_regression)

# ======================= K means clustering =========================================

# Runs K means clustering on the iris dataset and myGOExSet. myGOExSet contains gene IDs,
# their associated GOSlim term names GOSlim IDs, and gene expression data from different
# experiments.

# WOW(SimpleKMeans)

# N is the number of clusters we want to have,in this case N = 3 since we have
# 3 different flower types.
cl1 <- SimpleKMeans(iris[, -5], Weka_control(N = 3))
cl1

# Predicts Species using the k means cluster we fit and represents the predictions in a table
table(predict(cl1), iris$Species)

# The results show that there is a clear distinction between clusters. Where cluster 0 represents
# veriscolor, cluster 1 represents setosa, and cluster 2 represents virginica. It turns out that there
# is some overlap between cluster 0 and cluster 2. Let's try and find out why this is the case!

# First separate the dataset into setosa, veriscolor, and virginica
setosa <- iris[iris$Species == 'setosa',]
versicolor <- iris[iris$Species == 'versicolor',]
virginica <- iris[iris$Species == 'virginica',]

# Get an overview of each separated dataset
summary(setosa)
summary(versicolor)
summary(virginica)

# Calculate the absolute value of the difference between each feature of
# versicolor flowers and virginica flowers.
diff_ver_vir_c1 <- c(abs(versicolor[1] - virginica[1]))
diff_ver_vir_c2 <- c(abs(versicolor[2] - virginica[2]))
diff_ver_vir_c3 <- c(abs(versicolor[3] - virginica[3]))
diff_ver_vir_c4 <- c(abs(versicolor[4] - virginica[4]))

# Combine the columns into a dataframe
diff_ver_vir <- data.frame(diff_ver_vir_c1, diff_ver_vir_c2, diff_ver_vir_c3, diff_ver_vir_c4)
# Get a summary of the data
summary(diff_ver_vir)

# By looking at the median and mean of the difference between each of the features for versicolor
# and virginica we can see that overall these flowers seem similar in appearance. However, lets compare
# these results to the difference between setosa and versicolor flowers

diff_ver_set_c1 <- c(abs(versicolor[1] - setosa[1]))
diff_ver_set_c2 <- c(abs(versicolor[2] - setosa[2]))
diff_ver_set_c3 <- c(abs(versicolor[3] - setosa[3]))
diff_ver_set_c4 <- c(abs(versicolor[4] - setosa[4]))

diff_ver_set <- data.frame(diff_ver_set_c1, diff_ver_set_c2, diff_ver_set_c3, diff_ver_set_c4)

summary(diff_ver_set)

# These results show that the median and mean of the absolute value of the different between
# versicolor flowers and setosa flowers is indeed larger than the median and mean of the
# absolute value of the different between versicolor and virginica flowers, which supports the results
# from our clustering. (versicolor and virginica have a slight overlap, while all setosa flowers have their
# own cluster).

# Now lets run clustering to gropu GOSlim terms
# Choose columns 2 to 7 inclusive to create clusters. These columns contain expression levels of the
# terms at different times.
clusters <- SimpleKMeans(dataset[,2:7], Weka_control(N = 4))
clusters

# Predicts termName using the k means cluster
table(predict(clusters), dataset$termName)

# From the results we can see that our clusters are not able to precict the GOSlim terms
# accurately. All GOSlim terms seem to favor cluters 0, and 2. This could be caused by many different
# reasons. For example there could some sort of property that all of these clusters are representing.
# We can also notice that there are only 5 Housekeeping terms in total in the dataset.
# Lets remove the Housekeeping term and see if this will improve our results.

# Lets try and figure out why!

# Remove the all rows that correspond to the Housekeeping termName
dataset_no_housekeeping <- dataset[dataset$termName != "Housekeeping",]

clusters_no_housekeeping <- SimpleKMeans(dataset_no_housekeeping[,2:7], Weka_control(N = 3))
clusters_no_housekeeping

table(predict(clusters_no_housekeeping), dataset_no_housekeeping$termName)

# Unfortunately this has not imporved our results, in general all three terms seem to favor
# cluster 0 and cluster 2. Let's try and find out why this is the case.


# Separate dataset in to their individual termNames so we can analyze results corresponding to
# each termName independently.
dna_replication <- dataset[dataset$termName == 'DNA replication',]
cell_budding <- dataset[dataset$termName == "cell budding",]
housekeeping <- dataset[dataset$termName == "Housekeeping",]
response_osmotic <- dataset[dataset$termName == "response to osmotic stress",]

summary(dna_replication)
summary(cell_budding)
summary(housekeeping)
summary(response_osmotic)

# Lets compare the gene expression data for DNA Replication and cell budding. Since unlike the iris
# dataset, the GOSlim data set does not contain the same number of results for each termName we will
# randomly select 40 rows of data from each termName to compare with each other.

dna_replication_40 <- dna_replication[sample(nrow(dna_replication), 40), ]
cell_budding_40 <- cell_budding[sample(nrow(cell_budding), 40), ]

# Calculates the absolute value of the difference between columns 2:7 from dna_replication_40
# and columsn 2:7 from cell_budding_40.
diff_dna_cell <- data.frame(c(abs(dna_replication_40[2:7] - cell_budding_40[2:7])))

summary(diff_dna_cell)

# From the summary of the dataframe we can see that there isn't a significant difference between the
# gene expression data for DNA Replication and cell budding. The lack of a significant difference
# between the gene expression data may be causing clustering to fail.

# Try comparing the gene expression data for DNA replication and response to osmotic stress using the
# same format.

# ====================== Bayesian Networks ==============================

# Load the Pima Indians Diabetes dataset

# US National Institute of Diabetes and Digestive and Kidney Diseases,  (Pima.tr/Pima.te)[Dataset]
# retried from https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Pima.tr.html
# Sneak peek of the data
head(Pima.tr)

# Sepearting dataset into training set and test set
train_set <- Pima.tr
test_set <- Pima.te

# Creating a RWeka Bayes net classifier
BNet <- make_Weka_classifier("weka/classifiers/bayes/BayesNet")

# Generating a Bayes Net
model <- BNet(type~., data=train_set)

# Viewing our results
summary(model)
# Evaluating our model
eval_model <- evaluate_Weka_classifier(model, newdata=Pima.te)

# At a quick glance our results look decent. Our model is able to correctly predict 73.1928%
# of our instances. However upon closer inspection we can see that in general our Bayes net
# is much better at identifying "no" a patient does not have diabetes compared to "yes" a patient does
# have diabetes.

# From eval_model we can see that it correctly classified 190/233 out all "no" patients,
# which is roughly an accuracy of 81.5%. However it only correctly classifier 53/109 out of
# all "yes" patients, which is only an accuracy of 48.6%. The reason the results still look quite
# decet at 73.2% is because there are so many more "no" instances compared to "instances"

#[END]

# ___ID___.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the ___ID___ unit.
#
# Version:  0.1
#
# Date:     2017  MM  DD
# Author:   NN (name@host.tld)
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

# = 1 Installing RWeka

# IF YOU ARE USING A MAC AND ARE GETTING ERRORS RUNNING 
# "library(RWeka)" THEN RUNNING THIS COMMAND MAY FIX IT: 
# sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib
 
if (!require(RWeka, quietly = TRUE)) {
  install.packages("RWeka")
  library(RWeka)
}

if(!require(partykit, quietly = TRUE)) {
  install.packages("partykit")
  library(partykit)
}
# = 2 Simple example using RWeka
# Runnning some tests on the iris dataset
summary(iris)

# C4.5 classifier
iris_j48 <- J48(Species ~ ., data = iris)
iris_j48
summary(iris_j48)
# 10 folds cross validation on a C4.5 classifier
eval_j48 <- evaluate_Weka_classifier(iris_j48, numFolds = 10, complexity = FALSE, 
                                     seed = 1, class = TRUE)
eval_j48

plot(iris_j48)

# Load example dataset 
load(file = "./data/myGOExSet.RData")

# sample binary classifier for YAL013W using C4.5

YAL013W_data <- myGOExSet

YAL013W_data$ID[YAL013W_data$ID != "YAL013W"] <- "notYAL013W"

YAL013W_data$ID <- as.factor(YAL013W_data$ID)

dataset <- YAL013W_data[, c(1,5:15)]
YAL013W_j48 <- J48(ID ~., data = dataset)
summary(YAL013W_j48)

# sample binary classifier for YAL013W using Linear Regression
# YAL013W_linear_regression <- LinearRegression(ID ~., data = dataset)
# summary(YAL013W_linear_regression)


# = 1.1 Subsection






# = 99  Task solutions


# = 99.1  Task 1: ...

# = 99.2  Task 2: ...


# [END]

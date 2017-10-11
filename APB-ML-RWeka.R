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

if(!require(party, quietly = TRUE)) {
  install.packages("party")
  library(party)
}
# = 2 Simple example using RWeka
# Runnning some tests on the iris dataset. This dataset contains data on three different flowers,
# setosa, versicolor, and virginica. Each flower has data on its sepal length, sepal width, petal
# length, and petal width. We will be building a simple classifier using the built in J48 algorithm
# in RWeka.

summary(iris)

# C4.5 classifier - this classifier generates a decision tree
iris_j48 <- J48(Species~., data = iris)
iris_j48
summary(iris_j48)
# 10 folds cross validation on a C4.5 classifier
eval_j48 <- evaluate_Weka_classifier(iris_j48, numFolds = 10, complexity = FALSE, 
                                     seed = 1, class = TRUE)
eval_j48

plot(iris_j48)

# Load example dataset 
load(file = "./data/myGOExSet.RData")

# sample binary classifier for YAL013W using C4.5 classifier.

YAL013W_data <- myGOExSet

# Since multiple genes can have multiple GOSlim terms I decided to use a binary classifier to classify
# one specific gene. This idea can be expanded to classify all 165 differen GOSlim terms. However, one problem
# is that in the dataset there is only one row for one gene, and every other line is for genes that are not
# "that gene".

YAL013W_data$ID[YAL013W_data$ID != "YAL013W"] <- "notYAL013W"

YAL013W_data$ID <- as.factor(YAL013W_data$ID)

dataset <- YAL013W_data[, c(1,5:15)]
YAL013W_j48 <- J48(ID ~., data = dataset)
summary(YAL013W_j48)

# sample binary classifier for YAL013W using Linear Regression
# YAL013W_linear_regression <- LinearRegression(ID ~., data = dataset)
# summary(YAL013W_linear_regression)
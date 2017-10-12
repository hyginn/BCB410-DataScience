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
# = 2 Simple examples using RWeka
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

# sample binary classifier for GOSlim terms using C4.5 classifier.

data <- myGOExSet

# Since multiple genes can have multiple GOSlim terms I decided to use a binary classifier to classify
# one specific gene. This idea can be expanded to classify all 165 differen GOSlim terms. However, one problem
# is that in the dataset there is only one row for one gene, and every other line is for genes that are not
# "that gene".

# Converts Strings to one-hot encoding representations
data$termName <- as.factor(data$termName)

# Selects the GOSlim term names, and the columns containing its associated gene expression data
dataset <- data[, c(3,5:15)]
GOSlim_j48 <- J48(termName ~., data = dataset)
summary(GOSlim_j48)

plot(GOSlim_j48)

# sample binary classifier for YAL013W using Linear Regression
# YAL013W_linear_regression <- LinearRegression(ID ~., data = dataset)
# summary(YAL013W_linear_regression)
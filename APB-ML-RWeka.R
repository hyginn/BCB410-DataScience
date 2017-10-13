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
# = Simple examples using RWeka

# Runnning some tests on the iris dataset. This dataset contains data on three different flowers,
# setosa, versicolor, and virginica. Each flower has data on its sepal length, sepal width, petal
# length, and petal width. We will be building a simple classifier using the built in J48 algorithm
# in RWeka.

summary(iris)

# C4.5 classifier - this classifier generates a decision tree
iris_j48 <- J48(Species~., data = iris)
iris_j48
summary(iris_j48)

# 10 folds cross validation using a C4.5 classifier
eval_j48 <- evaluate_Weka_classifier(iris_j48, numFolds = 10, complexity = FALSE, 
                                     seed = 1, class = TRUE)
eval_j48

# Load example dataset 
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

# ======================= Linear Regression =========================================
# RWeka does not support multinomial linear regression so we will be training a model 
# on the mtcars dataset using Linear Regression. 

linear_reg_dataset <- mtcars

# Runs linear regression on the linear_reg_dataset, where mpg is the dependent variable
mtcars_linear_regression <- LinearRegression(mpg ~., data = linear_reg_dataset)

# Shows the associated weights and base value that linear regression has assigned to model
# the mtcars dataset.
mtcars_linear_regression

# Shows different error values - gives a measurement of how "good" our model is
summary(mtcars_linear_regression)

# ======================= K means clustering =========================================

# Runs K means clustering on the gene expression dataset, where N is the number of clusters,
# in this case N=4 since we have 4 different GOSlim term names. For more information uncomment
# the line below.

# WOW(SimpleKMeans)

cl1 <- SimpleKMeans(iris[, -5], Weka_control(N = 3))
cl1

# Predicts Species using the k means cluster we fit
table(predict(cl1), iris$Species)

# Choose columns 2 to 5 inclusive to create clusters
clusters <- SimpleKMeans(dataset[,2:7], Weka_control(N = 4))
clusters

# Precits termName using the k means cluster we fit
table(predict(clusters), dataset$termName)

# From the results we can see that our clusters are not able to precict the GOSlim terms
# accurately. All GOSlim terms seem to favor cluters 0, and 2. This could imply that they all
# share some sort of property that these clusters are representing.



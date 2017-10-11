# ___ID___.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the MOD_EDA_Logistic_Regression unit.
#
# Version:  0.1
#
# Date:     2017  10  10
# Author:   Ian Shi (ian.shi@mail.utoronto.ca)
#
# Versions:
#           0.1    First draft, performing logsitic regression on myGOExSet to find potential 
#                   relationship between expression and function.

#
# TODO: Find a dataset that shows better accuracy
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# = Load Required Helper Packages

# = 1 Data Pre-Processing

# To preform logistic regression, we will need to first decide what we're trying to categorize.
# In the myGOExSet data, we can see if there is any relation between expression and function.
load("./data/myGOExSet.RData")

# = 1.1 Selecting Features

# Because we are using binomial discrete outputs, lets trim the data to only those of function 
# DNA replication and osmotic stress response.
trimmed <- subset(myGOExSet, termName == "DNA replication" | termName == "response to osmotic stress")

# = 1.2 Cleaning Data

# We will remove any data points without a complete expression profile, and also remove irrelevant columns
cleaned <- trimmed[complete.cases(trimmed), ]
cleaned <- subset(cleaned, select=c(3, 5:17))

# Finally, we need to reduce our response variable to a 0/1 output.
# We will let 1 equate to DNA replication, and 0 to osmotic stress
for (i in 1:length(cleaned$termName)) {
  if (cleaned$termName[i] == "DNA replication") {
    cleaned$termName[i] <- 1
  } else {
    cleaned$termName[i] <- 0
  }
}
cleaned$termName <- as.numeric(as.character(cleaned$termName))

# = 1.3 Splitting Data

# Next, we will sample data to create an training and testing set with a 70/30 split. 

# We set a seed for reproduction
set.seed(0)
split_size <- ceiling(0.7 * nrow(cleaned))
training_indices <- sample(seq_len(nrow(final)), size = split_size)

train <- cleaned[training_indices, ]
test <- cleaned[-training_indices, ]

# = 2 Logistic Regression

# Now for the fun part! We will perform logistic regression using the glm package.
logistic <- glm( formula = termName ~ ., family=binomial(link='logit'), data=train)

# We can also see the weights of the logistic regression curve
summary(logistic)

# = 3 Prediction

# Using our trained classifier, we can try to predict the test data.
# Response gives us the condition probability.
results <- predict(logistic, test, type='response')

# We choose 0.5 to be the decision boundary
results <- ifelse(results > 0.5, 1, 0)
error <- mean(results != test$termName)
print(error)

# This unfortunately yields a rather poor result of only 43% predictive accuracy.
# It is a result nonetheless, and could indicate several issues:
# Its possible there is no relationship between our response and predictors, or it is also possible that 
# our dataset isn't sufficient to observe the underlying relationship.

# = 4 Visualization

# Borrowing from other modules, you could also use visualization techniques to see the ROC curve here, etc.


# = 4 Excercises

# Task: Perform a logistic regression on the same dataset, but try to classify between other functions.

# Suggested Task Solution: Replace data filtering from DNA replication / osmotic stress to those desired.
# [END]
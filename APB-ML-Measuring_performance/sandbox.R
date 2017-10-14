library(mlbench)
library(caret)

# Dataset
data("PimaIndiansDiabetes")
dataset = PimaIndiansDiabetes

# Test options
control = trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE, summaryFunction=twoClassSummary)  # classProbs=TRUE is needed for ROC
seed = 7

# Test metric, try Accuracy, ROC, RMSE, logLoss
metric = "ROC"

# Model building
preProcess = c("center", "scale")

# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(diabetes~., data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(diabetes~., data=dataset, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed)
fit.glmnet <- train(diabetes~., data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(diabetes~., data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(diabetes~., data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(diabetes~., data=dataset, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(diabetes~., data=dataset, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed)
fit.c50 <- train(diabetes~., data=dataset, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(diabetes~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(diabetes~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(diabetes~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)


# Model selection
results = resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)

# boxplot
bwplot(results)
# Dot-plot
dotplot(results)








library(ggplot2)
lrows <- 3000
lcols <- 11

syn_data <- array(data=0, dim=c(lrows, lcols))

colnames(syn_data) <- c("ONE","NUMBER","R1","R2","R3",
                        "R4", "R5", "R6","NINE","TEN", "ELEVEN")

syn_data[,2] <- c(seq(lrows, 1))
syn_data[,1] <- c(runif(lrows, 0.0, 7.5))


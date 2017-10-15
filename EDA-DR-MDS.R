#EDA-DR-MDS.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-DR-MDS unit.
#
# Version:  0.1
#
# Date:     2017  10  07
# Author:   Ricardo Ramnarine (ricardo.ramnarine@mail.utoronto.ca)
#
# Versions:
#           0.1    (Initial - skeleton template)
#           0.2    (Added section for dependencies of unit)
#
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

# = 1 Dependencies

if (!require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(devtools, quietly=TRUE)) {
  install.packages("devtools")
  library(devtools)
}

if (!require(MASS, quietly=TRUE)) {
  devtools::install_github("MASS")
  library(MASS)
}

if (!require(here, quietly=TRUE)) {
  devtools::install_github("krlmlr/here")
  library(here)
}



#Load Functions
source("EDA-DR-MDS/performMDS.R")

#Load Data
load("./data/GSE3635.RData")


# = 1.1 MDS Dimension Reduction techniques
# There are 3 flavours of MDS
# 1. Classic
  #If the
# 2. Metric
  #If there is a metric distance between coordinates in the matrix, the MDS is considered metric.
  #Any pair of objects are considered metric if dist(x,y) >= 0, d(x,y) = 0 <=> x = y, d(x,y) = d(y,x), d(x,z) <= d(x,y) + d(y,x)

# 3. Nonmetric
  #If there isn't a metric distance between coordinates in the matric, the MDS is considered nonmetric.
## Multidimensional Scaling (MDS)

#####################################################


# = 1.2 Why is this technique useful? Ex. Overfitting

# = 1.3 One dimensional projections in user space

# = 2 Multidimensional Scaling

#Let's use some data as Boris used in DR-PCA example
set.seed(100)
library(MASS)
data(crabs)
mydata <- crabs

#have a look at our data
mydata

#Let's calculate euclidean distances and put it in a matrix
d <- dist(crabs[c(-1,-2)])
d <- as.matrix(d)

#isoMDS is a function that comes with R
result <- isoMDS(d, k=2)
result
#Notice that our stress score is within an acceptable range. i.e < 5%
d <- as.data.frame(result)
d <- d[-3]
descriptions <- paste(crabs[,1], crabs[,2])
#let's create some factors so we can categorize variables in the graph
fact <- factor(descriptions)
fact

#plot it using ggplot
ggplot(d, aes(x=points.1, y=points.2, group=fact)) +
  geom_point(aes(shape=fact, color=fact))


# = Multidimensional Scaling with Biological relevance


#Step 1: split data into times,
getwd()

#setwd()
#source("performMDS.R")
performMDS(GSE3635)


exprs(GSE3635)
featureNames(GSE3635)
sampleNames(GSE3635)
new <- na.omit(exprs(GSE3635))

#Lets use mean of the rows
m <- svd(new-rowMeans(new))
#factors are samples
group <- factor(sampleNames(GSE3635))
D1 <- m$d[1]*m$v[,1]
D2 <- m$d[2]*m$v[,2]
plot(D1,D2,pch=21,bg=as.numeric(group))

#Let's use mean of the columns this time
n <- svd(new-colMeans(new))
#factors are features
groups <- factor(featureNames(GSE3635))
#plot 1
E2 <- n$d[1]*n$v[,1]
E1 <- n$d[2]*n$v[,2]
plot(E2,E1,pch=21,bg=as.numeric(groups))
#notice that the relative distance between each points in plot 1 and 2 are close.
#When using MDS, dimension 1 and 2 have no meaning.

# = 10 Tasks to do

# = 99  Task solutions

# = 99.1  Task 1: ...

# = 99.2  Task 2: ...


# [END]

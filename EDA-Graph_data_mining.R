# EDA-Graph_data_mining.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA Graph data mining unit.
#
# Version:  0.1
#
# Date:     2017  10  14
#
# Author:   Kevin Lin ( jianbin.lin@mail.utoronto.ca)
#
# Versions History:
#           0.1    (This is first verison edited on Oct 14 2017)

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

# = 1 Section install required packages "Biobase", "GEOquery","igraph","network","sna","ndtv"

source("https://bioconductor.org/biocLite.R")


if (!require(Biobase, quietly=TRUE)) {
  biocLite("Biobase")
  library(Biobase)
}
if (!require(GEOquery, quietly=TRUE)) {
  biocLite("GEOquery")
  library(GEOquery)
}

# 1.1 build up gene co-expression matrix from GSE3635 data packages which contain 13 samples and 6228 features

gset <- getGEO("GSE3635", GSEMatrix =TRUE, getGPL=FALSE)

if (length(gset) > 1) {
  idx <- grep("GPL1914", attr(gset, "names"))
} else {
  idx <- 1
}

gset <- gset[[idx]]

# rows represent feature of matrix and columns represent sample
featureNames(gset)[1:20]   # rows
sampleNames(gset)[1:10]    # columns
( tmp <- gset[1:20, 1:10] )
exprs(tmp)
(tmp <- gset[1:20,1:10])
exprs(tmp)

genename <- featureNames(gset)[1:20]
genename


#build up adjacentmatrix1 for gene expression
#since total it's 6228 genes, it's too large to build up 6228 x 6228 matrix, we will build only 20*20
adjacentmatrix1 <- matrix(exprs(tmp),nrow = 20, ncol=10)

i <- 1
i
j <- 1
j

adjacentmatrix2 <- matrix(exprs(tmp), nrow=20, ncol=10)
adjacentmatrix2

# since not all of raw data has a value, we assign non-value cell into 0

for (i in 1:20 )
{
   for(j in 1:10)
{

    if(is.na(adjacentmatrix1[i,j] == TRUE))
    adjacentmatrix1[i,j] <- 0
   }
}

adjacentmatrix1

# build up adjacent matrix
adjacentmatrix2 <- t(adjacentmatrix1)

adjacentmatrix2

ff <- mean(adjacentmatrix2)
ff

# build up correlation
bb <-cor(adjacentmatrix2)

bb

gg <- mean(bb)
gg

# using mean value as threshhold value
cc<-as.numeric(abs(bb)>= mean(bb))
cc

dd<-matrix(cc,nrow=20)

dd

install.packages("igraph")
library("igraph")

install.packages("network")
library("network")

install.packages("sna")
library("sna")

install.packages("ndtv")
library("ndtv")

net <- network(dd, directed=FALSE)
net

plot(net,label=genename)




# [END]

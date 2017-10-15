# RPR-Data-Merge.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the merge unit.
#
# Version:  0.1
#
# Date:     2017  Oct  14th
# Author:   Yuhan Zhang(yuhan.zhang@mail.utoronto.ca)
#
# Versions:
#           0.2    (After feedback)

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

# = 1 Section - Abstract
#merging and joining multiple dataset for preparation of analysis
#last step in split-apply-merge analysis


# = 2 Section - Objective
#learning funtions in R to merge two data sets for proper usage.
#Understanding why we merge datasets.


# = 3 Section - Outcomes
#Ability to use merge in proper situation.
#Ability to merge two data frames properly based on requirement
#Complete tasks correctly.



# = 4 Section Pre-requisite
#RPR-Data-Import (Importing data in R)
#R and Rstudio basic concepts and functions, e.g:vector, factor and data.frame() etc.
# Understanding basic Table,matrix, and data frame structures.



# = 5 Section :Useful functions

# = 5.1 Subsection : add columns or rows prespectively
#adding a single column from 1 to 7
#cbind for add column, rbind for add rows
m <- cbind(1, 1:7)
m

#insert a column
m <- cbind(m, 8:14)[, c(1, 3, 2)]
m
# combination of rbind and cbind
cbind(0, rbind(1, 1:3))

# add some names
cbind(I = 0, X = rbind(a = 1, b = 1:3))

# Be careful that if you want to use rbind(data frame A, data Frame B), you need
# to make sure that they have the same variables.
# cbind and rbind bind cols and rows without matching common columns, which means you may insert wrong values
# in some row/cols.
# Therefore, we introduce other function below to merge two frames when we want to match some columns.

# = 5.2 Subsection: Merge() function in R
#What if two frames does not have the same varibales? we use merge() to add them together.
help(merge)

# merge(x,y,..)
# x, y are frames to be merged
# by, specification of column you want to match on;by.x and by.y used when
# two data frames have different names for the columns you want to match on,
# all. all.x all.y  If True then extra row/column will be extra rows will be added to the output,
# one for each row in x that has no matching row in y.
# These rows will have NAs in those columns that are usually filled with values from y.
# The default is FALSE, so that only rows with data from both x and y are included in the output.

# merge(frameA,frameB, by="ID") merged by common column ID
# merge(frameA,frameB,by=c("ID","Country")) merged by ID and Country
# merge(frameA,frameB,by.x = "ID",by.y = "Id")merged by ID of frameA and Id of frameB.

# When merging directly, be careful that x,y are the same data structure. If not, you'd better
# convert one of them to become useful.

# = 5.2.1 Convert ExpressionSet into matrix or frame.
#simply provide a simple way:
#prepare downloading data
install.packages("readr")
library(readr)
# Load the bioconductor package installer
source("https://bioconductor.org/biocLite.R")

biocLite("Biobase")
library(Biobase)

biocLite("GEOquery")
library(GEOquery)

gset <- getGEO("GSE3635", GSEMatrix =TRUE, getGPL=FALSE)

if (length(gset) > 1) {
  idx <- grep("GPL1914", attr(gset, "names"))
} else {
  idx <- 1
}
# Fallback data - in case the GEO server is not working:
gset <- gset[[idx]]
GSE3635 <- gset
save(GSE3635, file="./data/GSE3635.RData")
load(file="./data/GSE3635.RData")
gset <- GSE3635
#now gset is a eset.
m <- exprs(gset) #use exprs to return matrix of expression of values stored in m.
# Assumed we want to pick pheoData as new rows/cols in new matrix.
pdata <- pData(gset)
d <- cbind(pdata, t(m))
#use cbind of combine them. t() is tranpose function used to convert rows and columns
# to make sure the number of rows and cols can match.
d[1:5,1:5] #see first couple of lines
d <- data.frame(d) # making it as frame
# then we can merge with other frames.




# = 5.2.2 Task 1: (test basic use of merge in bio data, be careful of multiple common column names)
# We want to investigate difference of genes with amino acid transportation and nucleolus annotations.
# To do that, you need:
# merge aaTrans and nucle from Yeast GOSlim by their common columns to create a new data frame to work.

# prepare aaTrans and nucle from GOSlim of yeast

# Download Yeast GOSlim
url <- "https://downloads.yeastgenome.org/curation/literature/go_slim_mapping.tab"
download.file(url, destfile = "./data/go_slim_mapping.tab", method = "libcurl")
scGsl <- read_tsv("./data/go_slim_mapping.tab",
colNames <- c("ID",
              "name",
              "SGDId",
              "Ontology",
              "termName",
              "termID",
              "status"))

na.omit(scGsl) ## remove NA values
#split scGsl into multiple groups by its termName
newset = split(scGsl, scGsl$termName)
#derive aaTrans and nucle using split
aaTrans <- newset$`amino acid transport`
nucle <- newset$nucleolus

# merge aaTrans and nucle below :


# = 5.3 Subsection : combine() for ExpressionSet(eset)
#combine(eset1, eset2)

# = 5.3.1 Task 2:
#since gset and gset2 has common featureNames, assumed that we want to merge this two ExpressionSets
# for 6 common featureNames(YAL001C to YAL007C) with 6 samples in gset and 6 samples in gset2
#what whould you code for?


#fallback data in case of not working
GSE4987 <- getGEO("GSE4987", GSEMatrix =TRUE, getGPL=FALSE)
length(GSE4987) # 1
GSE4987 <- GSE4987[[1]]
save(GSE4987, file="./data/GSE4987.RData")
load(file="./data/GSE4987.RData")
gset2 <- GSE4987
#please implement codes below :




# = 6 Section :Application in large-scale analysis
#An increasing amount of microarray gene expression data sets is available through public repositories such as NCBI GEO.
#The public repositories might contain the necessary clues for the discovery of new findings,
#leading to the development of new treatments or therapies
#Their huge potential in making new findings is yet to be unlocked by making them available for large-scale analysis.
#Integration of this vast amount of data originating from different but independent studies
#could be beneficial for the discovery of new biological insights
#by increasing the statistical power of gene expression analysis.

# = 7 Section : Practice for application:
# merging and integrating data can be used after spliting and analyzing each group data,in order to combine
# results,new features and give conclusions, saving as new data created.
# Also,you will need merging ability when you want to create model(could be used in machine learning or regression etc.)
# that needs information from more than one data frame/profile.
# For example
# Task 3: (test whether understand and can use all functions above)
#here some student wants to create a training model for classification to investigate GO and corresponding
#data from microarrays, combining termID (as one column) in Yeast GOSlim and expression
#data from GSE3635 based on their common features/IDs like YAL002C.
#write your codes here:




# = 8  Task solutions

# = 8.1  Task 1:
newFrame <- merge(aaTrans,nucle,all=TRUE, by = c("ID","name","SGDId","Ontology","termName","termID"))
unique(newFrame) # remove duplicate rows.
na.omit(newFrame) # remove NA values


# = 8.2  Task 2:
tmp = gset[12:17,1:6]
tmp2 = gset2[12:17, 1:6]
combine(tmp, tmp2)


# = 8.3 Task 3
#Since gset(data of GSE3635 derived before) has different structure as scGsl(), we need convert the format first.
frame1 <- exprs(gset)  ## get matrix expression data from gset
names <- featureNames(gset)  ## get feature names from gset used as row/col names
train.data <- cbind(names, t(frame1))  ## combine feature names and expression data together
train.data <- t(train.data)  ## convert row and cols in train.data. for now feature names are rownames
train.data <- cbind(ID = rownames(train.data), train.data)  ## insert one column of ID as common col for further merge
rescGsl <- scGsl[,-c(3:5)] ## reduced data that removing SGDId, Ontology and termName which are useless in this case
train.data <- data.frame(train.data) ## convert matrix to frame.
newdata <- merge(rescGsl,train.data, by="ID") ## merge the two data frame.




# = 9 Section Resources
#http://www.statmethods.net/management/merging.html
#https://www.hindawi.com/journals/isrn/2014/345106/
#ExpressionSet convertion https://support.bioconductor.org/p/77432/
#merge usage http://www.cookbook-r.com/Manipulating_data/Merging_data_frames/

# [END]

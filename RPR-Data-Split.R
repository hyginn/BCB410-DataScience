# RPR-Data-Split.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the split unit.
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

# R split and modification are usually used in processing raw data frames downloaded from original database.
# The original data frames we've got usually are too big and complicated for our purpose and further study.
# Therefore we simplify or rearrange data set based on specific requirements.
# In order to make results more reliable and unbiased, we subset the data frame and do analysis
# separately or apply different algorithms in different groups(split-apply-merge analysis).


# = 2 Section - Objective
#Understanding why and when we should split and modify dataset;
#Achieveing the ability of using split function to derive proper dataset we want.


# = 3 Section - Outcome
#ablility to use split in proper situation
#ablility to use split correctly
#data set derived from split is proper for further usage
#completion of tasks correctly to test the abilities above.


# = 4 Section Pre-requisite
#RPR-Data-Import (Importing data in R)
#R and Rstudio basic concepts and functions, including :vector, factor and data.frame(), row and col etc.
# Understanding basic Table,matrix, and data frame structures.



# = 5 Section : useful functions

# = 5.1 Subsection: subset
# The most simplist way to split data is just get subset by known indices or names.
data = data.frame(x=c(1,2,3),variable = c("a","b","c"))
subset = data[1:2, 1:2]


# = Section 5.2: split() in R
# What if the subset we want is not continuously distributed in the data or the
# data is too large to look for specific term? we use split function.
help(split)
# states usage of split.split(x,f,...)
# x is the data frame that we want to split, and f is factor or list of factors that we split based on.
# split(data,data$variable)

# = Section 5.2.1 Task 1:
df <- data.frame(
  x=rnorm(25),
  y=rnorm(25),
  g=rep(factor(LETTERS[1:5]), 5)
)

#Task 1: how to split df by g? (testing basic use of split)
#code below:


# = Section 6 : Application

# = Subsection 6.1 : data frame for Modeling
# Usually we need to build models for investigating relationships(linear or nonlinear) between two or multiple subjects
# In this way, we need to create a data frame specific for the model from raw data set.
# Here is a simple example:
# We want to investigate the relationship of expression data of global transcription profile for YAL002W intron
# along yeast cell cycle numbers for all samples from GSE3635.

#To reach the goal, we need a data frame for modeling which contains all expression data from
#GSE3635 for YAL002W.

# Load series and platform data from GEO
gset <- getGEO("GSE3635", GSEMatrix =TRUE, getGPL=FALSE)

if (length(gset) > 1) {
  idx <- grep("GPL1914", attr(gset, "names"))
} else {
  idx <- 1
}

gset <- gset[[idx]]
# Fallback data - in case the GEO server is not working:
GSE3635 <- gset
save(GSE3635, file="./data/GSE3635.RData")
load(file="./data/GSE3635.RData")
gset <- GSE3635  #save to
gset  #print gset

#see features
featureNames(gset)[1:20]
#see samples
sampleNames(gset)
#from above, we saw YAL002W at row 13
#we can subset gset:
gset[13,]

#if we do not know the exact row number,we can use the name for access:
exp <- featureNames(gset) == "YAL002W"
YAL002W <- gset[exp]
exprs(YAL002W)
#frame YAL002W would be used in modeling later.

# = Subsection 6.1.1:Task 2: (test seeking specific annoatation and split it from data.)
#Constructing a data frame for model of investigation of GSE4987 expression data for YAL023C along samples.
GSE4987 <- getGEO("GSE4987", GSEMatrix =TRUE, AnnotGPL=TRUE)
length(GSE4987) # 1
GSE4987 <- GSE4987[[1]]
save(GSE4987, file="./data/GSE4987.RData")
load(file="./data/GSE4987.RData")
gset2 <- GSE4987
#codes implemented here:




# = Section 6.2: split-apply-merge analysis
# Many data analysis problem in bioinformatics involve split-apply-merge strategy,which is
# breaking up a big problem into multiple pieces and perform operation/functions/algorithms
# in each piece and then combine them together. for this unit, we only concern split part.

# here is a simple example:
# If researchers want to investigate genes with different functions such as amino acid transport or ATPase activity
# derived from yeast GOSlim dataset, and apply assays/functions/algorithms on each group combined with expression data(merge part not included here),
# Because of too many functions for genes under termName in non-order, the fastest way to get the data we need is using split().
install.packages("readr")
library(readr)
# Load the bioconductor package installer
source("https://bioconductor.org/biocLite.R")

biocLite("Biobase")
library(Biobase)

biocLite("GEOquery")
library(GEOquery)

# Download Yeast GOSlim for preparation
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

##First, split scGsl into multiple groups by its termName annotations
newset = split(scGsl, scGsl$termName)
##choose one group that annotation is amino acid transport as one of the interest of the analysis
aaTrans <- newset$`amino acid transport`
unique(aaTrans) ## remove duplicate rows
##Then apply assays or some function or modeling in aaTrans
##also for ATPase:
ATPase <- newset$`ATPase activity`
#repeat same step as other annotations.

##if we use subset without split like:
sel <- scGsl$termName == "amino acid transport"
aaTrans_gene = unique(scGsl$ID[sel])
#we will produce the same result. But personally I prefer the split() beacuse "$" symbol would help you
#choose from lists of termNames instead of typing by hand,reducing lots of mistakes and save more time.

# = Section 6.2.1 Task 3: (test usage of split())
#We want to investigate all rRNA genes in yeast, please build a new data frame contains
# all rRNA genes in yeast from GOSlim for the analysis:




# = Section 6.3: Train-Test strategy
# When our objective turns to prediction instead of description and summaries of primary data, such as Machine
# Learning that will be discussed in other unit, we typically use test data to simulate the predictive model
# to see whether the model works fine or not,called as classification model.
# One of methods to get train and test data is using split.

#e.g: using GSE3635 profile to build train and test data frames for classification model.
## Train subset from first 10 samples,sample "GSM81064" to "GSM81073"
(train = gset[,1:10])
## Test subset from rest samples
(test = gset[,11:13])



# = 7  Task solutions


# = 7.1  Task 1: new <- split(df,g)

# = 7.2  Task 2:
exp2 <- featureNames(gset2) == "YAL023C"
YAL023C <- gset2[exp]
exprs(YAL023C)

# = 7.3 Task 3:
newset2 = split(scGsl, scGsl$status) ##split by status
rRNA <- newset2$rRNA_gene  ## choose rRNA_gene
unique(rRNA)   ## remove duplicate rows.



# = 9 Section Resources
#https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/split
#https://medium.com/fuzz/machine-learning-classification-models-3040f71e2529


# [END]

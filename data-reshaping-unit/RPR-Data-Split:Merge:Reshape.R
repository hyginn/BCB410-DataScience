# RPR-Data-Split/Merge/Reshape.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the split/merge/reshape unit.
#
# Version:  2.0
#
# Date:     2017  Dec  6th
# Author:   Yuhan Zhang(yuhan.zhang@mail.utoronto.ca)
#
# Versions:
#           2.0    (final revision)

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

# preparing the data, loading necessary packages
install.packages("readr")
library(readr)
# Load the bioconductor package installer
source("https://bioconductor.org/biocLite.R")

biocLite("Biobase")
library(Biobase)

biocLite("GEOquery")
library(GEOquery)

# Prepare gset and gset2 data frame loaded from GSE3635 and GSE4987.
# Load GSE3635 series and platform data from GEO
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


# Load GSE4987 series and platform data from GEO
GSE4987 <- getGEO("GSE4987", GSEMatrix =TRUE, AnnotGPL=TRUE)
length(GSE4987) # 1
GSE4987 <- GSE4987[[1]]
save(GSE4987, file="./data/GSE4987.RData")
load(file="./data/GSE4987.RData")
gset2 <- GSE4987

## ================Split==================
#Whenever you need to divide the data frame into subparts, you need split

# ========= Subsection 1.1: subset
# The most simplist way to split data is just get subset by known indices or names.
data = data.frame(x=c(1,2,3),variable = c("a","b","c"))
subset = data[1:2, 1:2]


# ========== Subsection 1.2: split() and unsplit() in R
help(split)
# states usage of split.split(x,f,...)
##[1]see also from R dosumentation of split()
# x is the data frame that we want to split, and f is factor or list of factors that we split based on.
# split(data,data$gender)
# we can also split frame based on a list of factors such as gender+marriage status, then we get group single male,
# single female,...etc.
# split(data, data$gender+data$marriage)

## In case that the f is not a factor variable, you'd better use as.factor(f) before splitting.


# unsplit() is simply reverse of split
# unsplit(x,f,...), f is also a factor or list of factors


# =========== Section 1.2.1 Task 1 :
# Given a new data frame df, try to split it based on g which has 5 levels (A,B,C,D,E) and reverse it using unsplit.
df <- data.frame(
  x=rnorm(25),
  y=rnorm(25),
  g=rep(factor(LETTERS[1:5]), 5)
)
##[2]Reference:see also https://stackoverflow.com/questions/9713294/split-data-frame-based-on-levels-of-a-factor-into-new-data-frames/9713456
## author:Jaap on Mar 15th, 2012.
#please implement code below:


# ============== Section 2 : Application
#
# ============== Subsection 2.1 : data frame for Modeling
# consider this scenranio:
# We want to investigate the relationship of expression data of global transcription profile for YAL002W intron
# along yeast cell cycle numbers for all samples from GSE3635.
#To reach the goal, we need a data frame for modeling which contains all expression data of YAL002W from GSE3635 .

gset  #contains expression data loading from GSE3635.

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

# ============ Subsection 2.1.1:Task 2:
# (test seeking specific annoatation and split it from data.)
#Constructing a data frame for model of investigation of GSE4987 expression data for YAL023C along samples.
#codes implemented here:
gset2  ## data loading from GSE4987



# ============ Section 2.2: split-apply-merge analysis

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





# =============== Section 2.3: Train-Test strategy
# One of methods to get train and test data is using split.

# Here is a simple case using subset:
# Get 75% of sample size of mtcars as train group and the rest as test groups
data(mtcars)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mtcars))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)

## using subset to create train and test.
train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

##[3] Reference: see also Jaap on May 5th,2014,
#  Retrieved from https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function



# =========== Subsection 3:  Task solutions
# = 3.1  Task 1:
new <- split(df,df$g)
reverse <- unsplit(new, df$g)

# = 3.2  Task 2:
exp2 <- featureNames(gset2) == "YAL023C"
YAL023C <- gset2[exp]
exprs(YAL023C)



##================= Merge===================

# =========== 4.1 Subsection : add columns or rows respectively
##[4] The following codes are referenced from R documentation of cbind.
#see from https://stat.ethz.ch/R-manual/R-devel/library/base/html/cbind.html.
help(cbind)
#adding a single column from 1 to 7
#cbind for add column, rbind for add rows
m <- cbind(1, 1:7)
# here argument 1 in cbind meaning a single column with all values are 1s.
m

#insert a column
m <- cbind(m, 8:14)[, c(1, 3, 2)]
# "[,c(1,3,2)]" rearranged the columns of m so that we insert a column in the middle.
m
# combination of rbind and cbind
cbind(0, rbind(1, 1:3))
# here rbind first created a matrix with two columns then cbind added this matrix to 0s.

# add some names
cbind(I = 0, X = rbind(a = 1, b = 1:3))

# ============= 4.2 Subsection: Merge() function in R
#What if two frames does not have the same varibales? we use merge() to add them together.
help(merge) ##[5] see from merge documentation

# merge(x,y,by,all,...)
# x, y: frames to be merged
# by,by.x,by.y: columns that you want to match on, in other words, the common columns of x and y.
# The default is to use the columns with common names between the two data frames.

# merge(frameA,frameB, by="ID") merged by common column ID
# merge(frameA,frameB,by=c("ID","Country")) merged by ID and Country
# merge(frameA,frameB,by.x = "ID",by.y = "Id")merged by ID of frameA and Id of frameB.
##[6] See also from https://www.statmethods.net/management/merging.html.


# When merging directly, be careful that x,y are the same data structure. If not, you'd better
# convert one of them to become useful.
# ============ 4.2.1 Convert ExpressionSet into matrix or frame.
#simply provide a simple way:
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
##[7] The above codes are inspired by Diego Diez from https://support.bioconductor.org/p/77432/.

# ======== 4.2.2 Task 1:
# We want to investigate difference of yeast genes with amino acid transportation and nucleolus annotations.
#For convenience, we need a data frame with only genes whose termName is amino acid transport and nucleolus.
#To do that , you can simply split scGsl by termName and pick up amino acid transport and nucleolus as we did in split unit and merge them together.
#please implement codes below:



# ================= 4.3 Subsection : combine() for ExpressionSet(eset)
#combine(eset1, eset2)

# ========== 4.3.1 Task 2:
#since gset and gset2 has common featureNames, assumed that we want to merge this two ExpressionSets
# for 6 common featureNames(YAL001C to YAL007C) with 6 samples in gset and 6 samples in gset2
#what would you code for?
#please implement codes below :

# ========== 4.3.2 Task 3:
#(test whether understand and can use all functions above)
#here some student wants to create a training model for classification to investigate GO and corresponding
#data from microarrays, combining termID (as one column) in Yeast GOSlim and expression
#data from GSE3635 based on their common features/IDs like YAL002C.
#write your codes here:

# ===========Subsecton 5:  Task solutions

# = 5.1  Task 1:
# prepare aaTrans and nucle from GOSlim of yeast
#split scGsl into multiple groups by its termName
newset = split(scGsl, scGsl$termName)
#derive aaTrans and nucle using split
aaTrans <- newset$`amino acid transport`
nucle <- newset$nucleolus

# merge aaTrans and nucle below :
newFrame <- merge(aaTrans,nucle,all=TRUE, by = c("ID","name","SGDId","Ontology","termName","termID"))
unique(newFrame) # remove duplicate rows.
na.omit(newFrame) # remove NA values


# = 5.2  Task 2:
tmp = gset[12:17,1:6]
tmp2 = gset2[12:17, 1:6]
combine(tmp, tmp2)


# = 5.3 Task 3
#Since gset(data of GSE3635 derived before) has different structure as scGsl(), we need convert the format first.
frame1 <- exprs(gset)  ## get matrix expression data from gset
names <- featureNames(gset)  ## get feature names from gset used as row/col names
train.data <- cbind(names, t(frame1))  ## combine feature names and expression data together
train.data <- t(train.data)  ## convert row and cols in train.data. for now feature names are rownames
train.data <- cbind(ID = rownames(train.data), train.data)  ## insert one column of ID as common col for further merge
rescGsl <- scGsl[,-c(3:5)] ## reduced data that removing SGDId, Ontology and termName which are useless in this case
train.data <- data.frame(train.data) ## convert matrix to frame.
newdata <- merge(rescGsl,train.data, by="ID") ## merge the two data frame.


## ================= Reshape ==================

# = Subsection 6.1: Transpose
#t(data)

# = Section 6.2 Introduction and useful functions of reshaping data
# = Subsection 6.2.1 : melt().
##[8] codes are inspired by "Robert I. Kabacoff, R in Action 2nd Edition,Chapter 5.6 Aggregation and reshaping, Published by May 2015, ISBN 9781617291388".
install.packages("reshape")
library(reshape)

mydata = data.frame(id = c("A","B","C","D"),
                    time = c(1,2,1,2),
                    x1=c(5,3,6,2),
                    x2= c(6,5,1,4))
melt(mydata)
# melt has two important arguments, id.vars and measure.vars, which is "ID variable" and "measure variable".
# By default, melt has assumed that all columns with numeric values are "measure variable".

# But maybe we want to know values of x1 and x2 for each id and time, we can do following:
mdata <- melt(mydata, id.vars=c("id","time"))
# We can do that with melt by telling it that we want id and time to be “ID variables”.
# x1 and x2 are measure variables,then melt returns two columns, one is called 'variable' which contains levels of x1 and x2
#, the other is called 'value' which contain the numeric value of each id-time-x1/x2 combinations.
mdata

# What if we wanted to control the column names in our long-format data?
# melt lets us set those too all in one step:
mdata <- melt(mydata, id.vars=c("id","time"),variable.name = "x1orx2")

# as for GEO file
gset <- GSE3635

tmp = gset[12:17,1:6]
test <- exprs(tmp) ## get expression matrix of tmp
# test is wide format, sometimes we want to make it as long format
# so that we can insert more columns later to make a new data frame.
mt <- melt(test) ## melt the test,
head(mt)
# melt gives two columns x1 and x2 with values of previous rownames and colnames.And for each combination, we have a value.
# sampleNames now become one column with corresponding features.
# Now we can insert or merge with other frames by matching the feature names and samples.


# = Subsection 6.2.2 cast():
install.packages("reshape2")
library(reshape2)
# cast(data, formula, fun.aggregate,..)
# the fun.aggregate is usuallly the function that we may want to apply on the data when casting. default is NULL.


# Typical practice is to recover the format we started with
# For the GEO file:
ct <- dcast(mt,X1~X2)
# we tell dcast that X1 is ID variable and X2 describes measured variable.
# Since there is only one remaining column, dcast will figure out that it contains the values themselves.
# We could explicitly declare this with value.var. (And in some cases it will be necessary to do so.)


# For mdata
mdata <- melt(mydata, id.vars=c("id","time"), variable.name = "x1orx2")
cdata <- cast(mdata,id + time ~ x1orx2)
## ID variables are id and time, and x1orx2 describes measured variable.

##[9] codes below are also inspired by "Robert I. Kabacoff, R in Action 2nd Edition,Chapter 5.6 Aggregation and reshaping, Published by May 2015, ISBN 9781617291388".
# When you run
ct <- dcast(mt,X2 ~value)
# you probably will see "Aggregation function missing: defaulting to length"
# It means When you cast your data and there are multiple values per cell,
# you also need to tell dcast how to aggregate the data.It could be mean, median or sum, etc.
subjmeans <- cast(mdata, id~variable, mean)
# use cast to apply average on x1 and x2 for each id
timemeans <- cast(mdata, time~variable, mean)
# apply average on x1 and x2 for each time


# = Subsection 6.3 : Task : test usage of melt and cast
gset2 <- GSE4987
featureNames(gset2)[1:20]
sampleNames(gset2)[1:6]
task.data <- gset2[12:17,1:6]
# Identify which format the task.data is :_________________
# Suppose now we want to merge it with some other frames from Yeast GOSlim
# So we need those sampleNames become one column to reduce column numbers,simplfying the final data frame.
# Implement these codes before merging part (remember to name the new column):


# Recover the frame to the beginning:



# = Subsection 6.4: Important application of reshape is pivot-table-like analysis.
##[10] The preparation of data was inspired by Christopher Bare, Pivot tables in R, published by 9th Jan ,2010. Retrieved from https://digitheadslabnotebook.blogspot.ca/2010/01/pivot-tables-in-r.html.
#Here is an example:
genes = paste('MMP', sprintf("%04d",1:10), sep="")
data = expand.grid(gene=genes, condition=c('copper', 'cheetos', 'beer', 'pizza'))
data$value = rnorm(40)
# if you look at the data, you realize it is a long-format data.
# If we want to pivot the conditions into columns so that we end up with one column for each condition and one row for each gene.
#The easiest way is using cast function in reshape package,
data.c <- dcast(data, gene ~ condition)
# what if we want to calculate the average of all four conditions for each gene like pivot table?
data.m <- melt(data, id.vars=c(1:2),measure.vars = c(3))
# gene and conditions are ID variables and numeric value is measure variables.
data.mean <- dcast(data.m, gene~variable, mean)
# And then if you want a complete look on this data with all genes, conditions and means of values,
# You can use merge() that we've learned before
data.merge <- merge(data.c,data.mean, by="gene")


# = Subsection 6.5 : Task solutions
# wide format
task.data <- exprs(gset2[12:17,1:6])
new.mt <- melt(task.data) # melt the task.data
colnames(new.mt) <- c("ID", "sampleNames","value") #name the cols
new.ct <- dcast(new.mt, ID ~ sampleNames) # recover the data.

# =======Subsection 7: Resources
#[1]https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/split
#[2]https://stackoverflow.com/questions/9713294/split-data-frame-based-on-levels-of-a-factor-into-new-data-frames/9713456
#[3]https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
#[4]https://stat.ethz.ch/R-manual/R-devel/library/base/html/cbind.html.
#[5]https://www.rdocumentation.org/packages/data.table/versions/1.10.4-2/topics/merge
#[6]https://www.statmethods.net/management/merging.html.
#[7]https://support.bioconductor.org/p/77432/.
#[8]Robert I. Kabacoff, R in Action 2nd Edition,Chapter 5.6 Aggregation and reshaping, Published by May 2015, ISBN 9781617291388
#[9]Robert I. Kabacoff, R in Action 2nd Edition,Chapter 5.6 Aggregation and reshaping, Published by May 2015, ISBN 9781617291388".
#[10]Christopher Bare, Pivot tables in R, published by 9th Jan ,2010. Retrieved from https://digitheadslabnotebook.blogspot.ca/2010/01/pivot-tables-in-r.html.
# [END]

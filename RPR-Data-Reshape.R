# RPR-Data-Reshape.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the reshape unit.
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

# = Section 1: Abstract
#Reshaping data in R for pivot-table-like analysis of data.
#usually when we access data, the raw form is not able for us to use directly so that
#we need to change its form for further usage. One of the methods is reshaping data.
#R provides various functions (such as melt and cast) to reshape the data as table or matrix
#before analysis, making it into useful form.




# = Section 2 - Objectives
#introduce the concept of data reshaping and applications.
# Understanding why we need to reshape data.
# learn different functions or methods in R to reshape data.
# getting to know Reshape R package.

# = Section 3 - Outcomes
#ability to transform raw dataset into specific form that we like to use.
#ability to use reshape R package
#complete tasks correctly to see whether achieve above outcomes.

# = Section 4 - Prerequisites
#RPR-Data-Import (Importing data in R)
#R and Rstudio basic concepts and functions, e.g:vector, factor,data.frame etc.
# Understanding basic Table,matrix, and data frame structures.


# = Section 5 Introduction and useful functions of reshaping data
# Data format can be long or wide. Wide data has a column for each variable.
# Long data is different, it has much more rows than columns.
# Long-format data has a column for possible variable types and a column for the values of those variables.
# Long-format data isn’t necessarily only two columns.It might have different levels for one of variable type,
# in that case, we could have another column.
# The ultimate shape you want to get your data into will depend on what you are doing with it.

# It turns out that you need wide-format data for some types of data analysis and long-format data for others.
# In reality, you need long-format data much more commonly than wide-format data.
# For example, ggplot2 requires long-format data (technically tidy data), plyr requires long-format data,
# and most modelling functions (such as lm(), glm(), and gam()) require long-format data.
# But people often find it easier to record their data in wide format.
# That's why we need reshape data.

# = Subsection 5.1: Transpose
#switch the row variables and column variables.
#R code : t() only works for  a matrix or data frame.
#sometimes converting row and column directly can simply convert the format.
# But t() sometimes is not a good idea.
#For matrix and frame, you'd better only use t() when data is all numeric not include any factor or string.
#
#t(data)



# = Subsection 5.2.1 : melt().
# melt takes wide-format data and melts it into long-format data.
# cast takes long-format data and casts it into wide-format data.
# reshape can choose either way by direction argument.
# Think of working with metal:
# if you melt metal, it drips and becomes long. If you cast it into a mould, it becomes wide.
install.packages("reshape")
library(reshape)
id = c(1,1,2,2)
time = c(1,2,1,2)
x1=c(5,3,6,2)
x2 = c(6,5,1,4)
mydata = data.frame(id,time,x1,x2)
melt(mydata)
# By default, melt has assumed that all columns with numeric values are variables with values.
# Often this is what you want.
# But maybe we want to know values of x1 and x2 for each id and time, we can do following:
mdata <- melt(mydata, id.vars=c("id","time"))
# We can do that with melt by telling it that we want id and time to be “ID variables”.
# ID variables are the variables that identify individual rows of data.
mdata


# What if we wanted to control the column names in our long-format data?
# melt lets us set those too all in one step:
mdata <- melt(mydata, id.vars=c("id","time"), variable_name = "x1orx2")


# as for GEO file
# prepare downloading data
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

tmp = gset[12:17,1:6]
test <- exprs(tmp) ## get expression matrix of tmp
# test is wide format, sometimes we want to make it as long format
# so that we can insert more columns later to make a new data frame.
mt <- melt(test) ## melt the test,
head(mt)
# melt gives two columns x1 and x2 with values of previous rownames and colnames.And for each combination, we have a value.
# sampleNames now become one column with corresponding features.
# Now we can insert or merge with other frames by matching the feature names and samples.


# = Subsection 5.2.2 cast():
install.packages("reshape2")
library(reshape2)
# In reshape2 there are multiple cast functions.
# Since we will most commonly work with data.frame objects, we’ll explore the dcast function.
# (There is also acast to return a vector, matrix, or array.)
# cast uses a formula to describe the shape of the data.
# The arguments on the left refer to the ID variables and the arguments on the right refer to the measured variables.
# Coming up with the right formula can take some trial and error at first
# cast(data, formula, function)


# Typical practice is to recover the format we started with
# For the GEO file:
ct <- dcast(mt,X1~X2)
# we tell dcast that X1 is ID variable and X2 describes measured variable.
# Since there is only one remaining column, dcast will figure out that it contains the values themselves.
# We could explicitly declare this with value.var. (And in some cases it will be necessary to do so.)


# For mdata
mdata <- melt(mydata, id.vars=c("id","time"), variable_name = "x1orx2")
cdata <- cast(mdata,id + time ~ x1orx2)
## ID variables are is and time, and x1orx2 describes measured variable.


# When you run
ct <- dcast(mt,X2 ~value)
# you probably will see "Aggregation function missing: defaulting to length"
# It means When you cast your data and there are multiple values per cell,
# you also need to tell dcast how to aggregate the data.It could be mean, median or sum, etc.
subjmeans <- cast(mdata, id~variable, mean)
timemeans <- cast(mdata, time~variable, mean)


#reshape() can do either wide or long by setting direction.
#reshape(mdata, idvar = "id", timevar = "variable", direction = "wide")

# = Subsection 5.3 : Task : test usage of melt and cast
#fallback data in case of not working
GSE4987 <- getGEO("GSE4987", GSEMatrix =TRUE, getGPL=FALSE)
length(GSE4987) # 1
GSE4987 <- GSE4987[[1]]
save(GSE4987, file="./data/GSE4987.RData")
load(file="./data/GSE4987.RData")
gset2 <- GSE4987
featureNames(gset2)[1:20]
sampleNames(gset2)[1:6]
task.data <- gset2[12:17,1:6]
# Identify which format the task.data is :_________________
# Suppose now we want to merge it with some other frames from Yeast GOSlim
# So we need those sampleNames become one column to reduce column numbers,simplfying the final data frame.
# Implement these codes before merging part (remember to name the new column):




# Recover the frame to the beginning:


# = Section 6 : Task solutions
# wide format
task.data <- exprs(gset2[12:17,1:6])
new.mt <- melt(task.data) # melt the task.data
colnames(new.mt) <- c("ID", "sampleNames","value") #name the cols
new.ct <- dcast(new.mt, ID ~ sampleNames) # recover the data.



# = Section 7 Resources
#*R guidebook:[http://r4ds.had.co.nz/ R for Data Science]
#*simple example of R codes using reshape package: [http://www.statmethods.net/management/reshape.html/ Quick-R reshaping Data]
#*reshape R package:[https://cran.r-project.org/web/packages/reshape/reshape.pdf/ rehshape package]
#*comparison with gather() and melt():[https://www.r-bloggers.com/how-to-reshape-data-in-r-tidyr-vs-reshape2/]
#*[http://seananderson.ca/2013/10/19/reshape.html/  Answers to Exercises]
# melt() and cast() http://seananderson.ca/2013/10/19/reshape.html
# [END]

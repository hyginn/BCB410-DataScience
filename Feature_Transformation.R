# Feature_Transformation.R
#
# Purpose: A Bioinformatics Course:
#           R code accompanying the feature transformation unit.
#
# Version:  1.0
# Date:     2017 12 02
# Author:   Xiao Wang (xiaow.wang@mail.utoronto.ca)
#
# License:  GPL-3  https://www.gnu.org/licenses/gpl-3.0.en.html
#
#
# ==============================================================================
# ======= DO NOT SIMPLY  source()  THIS FILE! ==================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                                  Line
#TOC> ------------------------------------------------------------------- ---
#TOC>   1    Log-transformation                                         41
#TOC>   2    Scaling                                                    67
#TOC>   3    Convert categorical data into numeric                      103
#TOC>   4    Rank transformation                                        130
#TOC>   5    Standardization                                            144
#TOC>   6    Working with biological data
#TOC>   7    Task solutions
#TOC>
#TOC> ==========================================================================

# In this script, I will first walk you through different transformation techniques
# with synthetic data; your tasks will be followed by each demo. Solution is at the
# end of script.


#=================  1  Log-transformation  =====================================
set.seed(5195) #set seed for this demo
x<-rlnorm(10000) # generate 10000 sample data points with log-normal-transformation

hist(x, breaks = 30, main = "Before transformation") # use histogram to visualize raw data
qqnorm(x) # use qqnorm() to check normality,
# straight-line means the data does not violate normal distribution,
# otherwise, data is not normal.

y <- log(x) # Log-transformation with raw data
hist(y, breaks = 30, main = "After Log-transformation") # use histogram to visualize log-transformed data.
qqnorm(y)# use qqnorm() to check normality,
# right now, the data is normalized.

#Task 1:
# Frist Generate 10000 datapoints that follow log-normal distribution with mean of 5, SD=3.
# Use log-transformation and check its normality.


# Log1p deal with zero value.
x[1:10] <- 0
# Now, when you take the log(x), and qqnorm(), error will apear as it cannot take infinite value.
qqnorm(log1p(x)) # solve the problem



#====================  2 Scaling  ==============================================
#==== Defined function ======
scale_by_max <- function(data_col){
  # input: a column from dataset
  # Divided by the maximum value in the column/feature.
  # output: scaled column
  MAX_VALUE <- max(data_col)
  new_col <- c()
  for(i in 1:length(data_col)){
    new_v <- data_col[i]/MAX_VALUE
    new_col <- rbind(new_col, new_v)
  }
  return(new_col)
}

#=== prepare a sythetic data ========
a <- rbind(1,2,3,4,1,2,3,4,1,2,3,4)
b <- rbind(2000, 1333, 1234, 2012, 4003, 2341, 4321, 5324, 1242, 2134, 6464, 7122)
c <- cbind(a,b)
colnames(c) <- c("f1", "f2")
head(c)     # You can see that the value in f1 is ranging from 1 to 4
max(c[,2])  # value in f2 ranging from 1234 to 7122.
min(c[,2])

#====== scale raw data with defined function ====
new_col_c1 <- scale_by_max(c[,1])
new_col_c2 <- scale_by_max(c[,2])
new_c <- cbind(new_col_c1, new_col_c2)
colnames(new_c) <- c("scaled_f1", "scaled_f2")

print(c)
print(new_c) # compare before scaling and after.

#Task2:
# Write a function that scale the data by dividing (Max+Min)
# Compare the result.



#=================  3  Convert categorical data into numeric  ===================
# == dummies package for one-hot encoding =======
if (! require(dummies, quietly=TRUE)) {
  install.packages("dummies")
  library(dummies)
}
# documentation for the package
# https://cran.r-project.org/web/packages/dummies/dummies.pdf

#=== prepare a sythetic data ========
f1 <- rbind("A", "B", "C", "D", "E")
f2 <- rbind(200, 101, 849, 846, 283)
df <- cbind(f1,f2)
df <- as.data.frame(df)
colnames(df) <- c("categoric", "f2")
#=====
# use dummy.data.frame() to convert categorical data into numeric.
new_df <- dummy.data.frame(df, names = c("categoric"), sep="")
print(df)
print(new_df) # as you can see the result.
# "categoric"column is split into number of levels, each categorical value has a unique value.



#====================  4 Rank transformation  ===================================
x <- rbind(33,22,55,333,11223,53,2134)

rank_of_data <- rank(x) # returns the index of rank with smallest value as rank 1.

#combine them into a df.
df2 <- cbind(x, rank_of_data)
df2 <- as.data.frame(df2)
colnames(df2) <- c("raw_data", "rank")
print(df2) # as you can see each data have its correspond rank.
# Notice that even extremely large value such as "11223" have rank 7.
# You can use rank-transformed column for future task.


#====================  5 Standardization  =======================================
x<-rnorm(1000,4,4) #generate 1000 random numbers follows N(4,4)

y<-(x-mean(x))/sd(x) #standize x by minus the mean of x and divided by the sd of x

# Now, use ggplot to visualize the difference.
names<-c(rep("before_transformation",1000),rep("after_transformation",1000))
data<-cbind.data.frame(names,c(x,y))
colnames(data)<-c("names","value")
ggplot(data,aes(value,fill=names))+geom_density(alpha=0.2)
# As you can see, after standardization, data becomes N(0,1).



#==================  6 Working with Gene Expression dataset  ===================
load(file="./data/GSE3635.RData")
# manipute one column/feature at first.
y <- GSE3635@assayData$exprs[12:nrow(GSE3635@assayData$exprs),1]

# check for NA values.
sum(is.na(y)) # 20 missing values

# Impute NA values by replacing it with a random sample from the row
for (item in 1:length(y)){
  if(is.na(y[item])){
    y[item] <- sample(y, 1)
  }
}

sum(is.na(y)) # check again, now is zero.

# there's negative values in the data, can not do log transformation
summary(y)
hist(y, breaks=30)
# for this dataset, not much transformation is needed.


#====================  7 Task solution  ========================================
# == Solution of Task 1:
x2 <- rlnorm(10000, 5, 3)
hist(x2, breaks = 30, main = "Before transformation")
qqnorm(x2)
y2 <- log(x2)
hist(y2, breaks = 30, main = "After Log-transformation")
qqnorm(y2)


# === Solution of Task 2: ================================
scale_by_sum_max_min <- function(data_col){
  MAX_VALUE <- max(data_col)
  MIN_VALUE <- min(data_col)
  new_col <- c()
  for(i in 1:length(data_col)){
    new_v <- data_col[i]/(MAX_VALUE+MIN_VALUE)
    new_col <- rbind(new_col, new_v)
  }
  return(new_col)
}
new_col2_c1 <- scale_by_sum_max_min(c[,1])
new_col2_c2 <- scale_by_sum_max_min(c[,2])
new_c2 <- cbind(new_col2_c1, new_col2_c2)
colnames(new_c2) <- c("scaled_by_sum_minmax_f1", "scaled_by_sum_minmax_f2")

print(c)
print(new_c2) # compare before scaling and after.

#==============
#END

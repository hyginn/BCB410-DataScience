# RPR_Data_Imputation.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the RPR-Data Imputation unit.
#
# Version:  0.4
#
# Date:     2017  10  06
# Author:   Greg Huang
#
# Versions:
#           0.1    First draft of RPR-Data Imputation learning unit.
#           0.2    Final version for review
#           0.3    First edited version after code review
#           0.4    Final version after code review
#
# TODO:
# 1. Build a synthetic data set to demonstrate properties of missingness,
#    and list-wise deletion/Complete Case Analysis(CCA)
# 2. Use MICE to impute data on the created synthetic dataset
# 3. Use MICE to impute data on a small GSE4987 dataset with 20 observations.
# 4. Use Hmisc to impute data on a large GSE4987 datset with 6228 observations.
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================
# Sections:
# 0. Load libraries                                          Line 51
# 1. Intro/Exploration of missingness                        Line 107
# 2. Running MICE on a small synthetic data set              Line 163
# 3. Running MICE on a small subset of the GSE4987 Dataset   Line 234
# 4. Running Hmisc's aregImpute() on a large GSE4987 subset  Line 298
# 5. Exercise solutions                                      Line 365
#
# ==============================================================================
# Important Note:
# The code references two main sources. I'll label them here as Source A and B
# to avoid clogging up the comments with the code.
# Source A: Analytics Vidhya's Tutorial on 5 R packages used for imputation
# URL: https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-
#      imputing-missing-values/
#
# Source B: R Blogger's Imputing Missing data with Mice
# URL: https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
#
# ==============================================================================
# 0.  Load required packages
source("https://bioconductor.org/biocLite.R")

if (!require(readr, quietly = TRUE)) {
  install.packages("readr")
  library(readr)
}

if (!require(Biobase, quietly=TRUE)) {
  biocLite("Biobase")
  library(Biobase)
}

if (!require(GEOquery, quietly=TRUE)) {
  biocLite("GEOquery")
  library(GEOquery)
}

# This is the MICE package, which we will be using to impute our data.
if (!require(mice, quietly=TRUE)) {
  install.packages("mice")
  library(mice)
}

# The VIM package, acronym for "Visualization and Imputation of Missing Values",
# help us visualize and analyze our data that contains missing elements before
# we impute them
if (!require(VIM, quietly=TRUE)) {
  install.packages("VIM")
  library(VIM)
}

# missForest package is used to generate random missing data in our examples.
if (!require(missForest, quietly=TRUE)) {
  install.packages("missForest")
  library(missForest)
}

# The Hmisc package will be used for imputation of large data sets.
if (!require(Hmisc, quietly = TRUE)){
  install.packages("Hmisc")
  library(Hmisc)
}

# Provide datasets for our exercises.
if (!require(datasets, quietly = TRUE)){
  install.packages("datasets")
  library(datasets)
}

# Lattice is primarily used to show multivariate relationships in our data.
if (!require(lattice, quietly = TRUE)){
  install.packages("lattice")
  library(lattice)
}

# 1.  Missingness Demo
# Let's create a synthetic data frame. Here, I created 4 different cell
# types, labeled from A-D as ct_A, ct_B, ct_C, and ct_D, and each
# with a different growth rate in a certain medium. The synthetic data
# shows the cell colony sizes over the first 10 time measurements.
ct_A <- c(5,10,15,20,25,30,35,40,45,50)
ct_B <- c(5,10,20,40,80,160,320,640,1280,2560)
ct_C <- c(5,4,3,2,1,0,0,0,0,0)
ct_D <- c(5,4.9,4.8,4.7,4.6,4.5,4.3,4.2,4.1,4)
synth_df <- data.frame(ct_A,ct_B,ct_C,ct_D)

# Check for correlation in between variables in this data frame
# We see some pretty high correlation r values here.
cor(synth_df)

# I will not use this for MCAR demo because MCAR very rarely happens and
# requires essentially a completely random set of data.

# 1.1 MAR: Missingness is random, but the missing data can be inferred
#          from the observed data
# Here we use random deletion and remove 5% of the data, using the prodNA()
# function found in the missForest package.
# Learned about prodNA() from Source A; code inspired by it.
set.seed(100)
df_MAR <- prodNA(synth_df, noNA = 0.05)
View(df_MAR)

# Now, with missing values in the data frame, cor() wouldn't work anymore.
# There is actually a complete-case analysis tool in cor(), which is
# complete.obs (listwise deletion)
# Learned about CCA in cor() from Quick-R's 'Correlations' page, URL:
# https://www.statmethods.net/stats/correlations.html
cor(df_MAR, use = "complete.obs")

# Compare this to the one we had with the full data. Now it looks like all
# variables are strongly correlated to one another. Probably wouldn't want
# that! We'll revisit them later in the unit.

# Let's do a massive loss of data this time: 30% loss.
set.seed(100)
df_MAR_thirty <- prodNA(synth_df, noNA = 0.3)
# Obviously, the results with CCA is pretty bad, too.
cor(df_MAR_thirty, use = "complete.obs")


# 1.2 MNAR: Suppose for whatever reason, the person collecting data did not
#           record values when less than 2. Then, the missing data is MNAR,
#           as there is a specific reason why the NAs are there.
df_MNAR <- synth_df
df_MNAR[df_MNAR < 2] <- NA
View(df_MNAR)

# Again, correlation analysis shows r values of close to 1 across the board
# Which is not desireable compared to the original data.
cor(df_MNAR, use = "complete.obs")

# 2.  Running MICE on a small synthetic data set
# Now that we've seen the different types of missing data and seen how
# CCA can be a bad way of dealing with our data, we can finally move
# on and look at imputation methods to avoid CCA.
# We'll use the same synthetic data created in section 1 of the script.

# At this point it is important to realize that we need to remove categorical
# values, as mice does not take it. In this example, there are none, so we
# don't need to delete any column of data.

# Just to review, obtain a summary of the dataset to see 2 NAs.
summary(df_MAR)

# Now we are ready to impute our missing data.
# First, we'd like to see if there's a pattern with the missing data.
# Wiki demomnstrates how to read the resulting table.
# Using md.pattern() is inspired by Source A.
md.pattern(df_MAR)

# Now, we use mice to impute the missing spots.
# Using the mice function, we look into the df_MAR file.
# m = 5 indicates the default number of imputed datasets, so in this case we'll
# get 5 different sets of imputed data for us to select from, or to pool
# together.
# maxit = 50 is the maximum number of iterations
# method = method for imputing. 'pmm' stands for "predictive mean matching.
# there are other methods available.
# we can set a seed for mice functions as well.
# Formatting of imputed_data and complete() is based on both Source A and
# Source B.
imputed_data <- mice(df_MAR, m=5, maxit = 50,
                     method = "pmm", seed = 100)

# We can get a comprehensive look at our imputated data after mice runs.
# You can also see what values each of the 5 iterations yielded by examining
# imputed_data in the Environment window.
summary(imputed_data)
# Now that we have 5 sets of data we can plug back to our dataset with missing
# values, simply run the complete()) method and select any 1 of the 5 lists
# we imputed. Here, I selected set #1.
df_MAR_completed <- complete(imputed_data, 1)

# See that the missing points have indeed been imputed:
summary(df_MAR_completed)

# Now let's compare our correlation matrices:
cor(synth_df)                         #orignal
cor(df_MAR_completed)                 #imputed after MAR
# Pretty similar, and much better than our CCA results!

# Additionally, we can use some plots to visualize our imputed data.
# The pink icons are the imputed numbers. Here we can see that the imputed
# numbers are rougly in line with the existing data.
# Idea of incorporating an xyplot is inspired by Source B.
xyplot(imputed_data, ct_B ~ ct_A + ct_C + ct_D, pch=2, cex = 0.5)

# It is also possible to take the 5 sets generated and fit a linear model to
# the data. Using mice, we use the with() function to fit the data, and pool()
# method the pool results together.
# Learned how to fit my data with both Source A and B.
fit <- with(data = imputed_data, lm(ct_B ~ ct_A + ct_C + ct_D))
summary(pool(fit))

# On another note, how does mice perform as more and more data is lost? Let's
# go with our 30% loss case. You can see that the imputation isn't as great
# anymore.
imputed_data_thirty <- mice(df_MAR_thirty, m=5, maxit = 50,
                     method = "pmm", seed = 100)
df_MAR_thirty_completed <- complete(imputed_data_thirty,2)
cor(df_MAR_thirty_completed)

# 3.  MICE example on the small GSE4987 data subset.
# Now we will apply the same process to the GSE4987 yeast cell cycle dataset.
# Before we get to the GSE examples, we'll load GSE4987 from GEO.
# Adapted from Dr. Steipe's dataPrep.R
GSE4987 <- getGEO("GSE4987", GSEMatrix =TRUE, AnnotGPL=TRUE)
if (length(GSE4987) > 1) {
  idx <- grep("GPL1914", attr(GSE4987, "names"))
} else {
  idx <- 1
}
GSE4987 <- GSE4987[[idx]]

# Access contents via methods:
featureNames(GSE4987)   # rows
sampleNames(GSE4987)    # columns

experimentData(GSE4987)
# Access contents by subsetting:
(tmp <- GSE4987[12:17, 1:6] )

# Access data
exprs(tmp)

# load the accessed data into snippet_GSE_dataset
snippet_GSE_dataset <- exprs(tmp)

# Let's have a look at the correlation matrix of our snippet
cor(snippet_GSE_dataset)
# Hmm, we are actually seeing good correlation coefficients across the board
# Given the structure of our data set, it seems like the separate transcription
# profiles have similar behaviour across the 30 minutes in this snippet.

# Get a statistical summary of the snippet GSE dataset
summary(snippet_GSE_dataset)
# Running summary() shows that there are no missing values (NA). As a result,
# Let's randomly put in gaps in our data. This will be a case of MAR, as we are
# removing items at random, but we can likely impute some of those missing
# values reasonably through imputation
set.seed(150)
snippet_GSE_dataset_missing <- prodNA(snippet_GSE_dataset, noNA = 0.1)

# Again, we can check out the pattern with md.pattern
md.pattern(snippet_GSE_dataset_missing)

# Impute data and get a summary of the imputations
imputed_snippet_GSE_data <- mice(snippet_GSE_dataset_missing, m=5,
                                 maxit = 50, method = "pmm", seed = 100)

summary(imputed_snippet_GSE_data)

# Add the imputed data into the missing parts
complete_snippet_GSE_data <- complete(imputed_snippet_GSE_data, 1)

# Check correlation:
cor(complete_snippet_GSE_data)

# Compare: See the differences between the original and the one we experimented
# with (after random removal and imputation with MICE.
cor(snippet_GSE_dataset)           #original
cor(complete_snippet_GSE_data)     #experimented
# Well, it's not too bad. Generally speaking, the matrix does not look too
# far off. The correlation matrix did not deviate too far from the original
# matrix before we randomly deleted elements.

# 4.  Hmisc example on large GSE4987 dataset.
# The Hmisc package is better at handling large datasets. In our case where the
# data frame has more than 6000 entries, imputations done on MICE and
# missForest tend to crash.
# Begin by reading in a much larger set. In this case, we'll get the first 10
# samples in GSE4987. [1:10] will include 10 of the samples and all 6000+
# features/observations.
(bigset <- GSE4987[12:6228, 1:10] )

# access data
exprs(bigset)
large_GSE_dataset <- exprs(bigset)

# get a summary of our dataset
summary(large_GSE_dataset)

# Upon examining the summary of the large dataset, we find that there are
# actually already NA's littered throughout each column / sample. Here we run
# the Hmisc imputation method to fill in those blanks. The next three lines
# set up the "formula" as well as preparing the data for the Hmisc function.
# Formatting of this is helped by Source A and this Stack Overflow question,
# URL: https://stats.stackexchange.com/questions/117383/impute-missing-values
#      -using-aregimpute
# Question asked by user 'Gurkenhals'.
colNames <- (sampleNames(GSE4987)[1:10])
impute_colNames <- as.formula(c("~", paste(colNames, collapse = '+')))
large_GSE_dataset_df <- as.data.frame.matrix(large_GSE_dataset)

# Before we impute, have a look at correlation results with CCA.
cor(large_GSE_dataset, use = "complete.obs")

# Run Hmisc. This will take some time (30s - 1 min). We will do 5 different
# iterations, as per our previous examples.
# Code based on R documentation for Hmisc and Source A.
imputed_large_GSE_data <- aregImpute(formula = impute_colNames,
                                     data = large_GSE_dataset_df,
                                     n.impute = 5)

# Now we have a list of imputed values available (set of 5).
# We will use impute.transcan() function to select a specific set of imputations
# and Re-insert them into the original data frame with missing values.
# Here, I selected imputation iteration number 2, out of the 5 available.
# impute.transcan() and code to complete imputation is based on Source A.
imputed_transcan <- impute.transcan(imputed_large_GSE_data,
                                    data = large_GSE_dataset_df,
                                    imputation = 2, list.out = TRUE,
                                    pr = FALSE, check = FALSE)
completed_large_GSE_dataset <- as.data.frame(do.call(cbind, imputed_transcan))
completed_large_GSE_dataset <-
  completed_large_GSE_dataset[,colnames(large_GSE_dataset_df), drop = FALSE]

# Summary of complete_large_GSM_data and note that the spaces have been filled
# in. (No more NAs)
summary(completed_large_GSE_dataset)

# compare this with the original dataset
summary(large_GSE_dataset)

# Correlation for completed large GSE dataset:
cor(completed_large_GSE_dataset)

# This learning unit demonstrated 2 types of data imputation packages available
# in R, and hopefully provided enough information for everyone to be able to
# impute data in future encounters with data sets with missing values.
# End of Learning Unit.

# ======================
# 5.  Exercise solutions
# Congratulations on completing this learning unit for RPR-Data-Imputation.
# Hope you enjoyed this unit.
# Answers:
# Exercise 1:
md.pattern(airquality)
# Answer:This should show 44 total missing values, and 35 of which have the
# pattern of just Ozone missing.
#
# Exercise 2:
exercise2_impute_data <- mice(airquality, m=5, maxit = 50,
                              method = "pmm", seed = 100)
exercise2_completed_data <- complete(exercise2_impute_data, 2)
View(exercise2_completed_data)
exercise2_fit_data <- with(data = exercise2_impute_data,
                           lm(Wind ~ Temp+Month+Day+Solar.R+Ozone))
summary(pool(exercise2_fit_data))
# Answer: 0.3180647

# Exercise 3
# Simply change the subsetting from 1:10 to 11:20 for the code in section 3.
# Compare the values between summary(large_GSE_dataset) and
# summary(completed_large_GSE_dataset). The median does not change for
# GSM112148, which remains as 0.005550.
# [END]

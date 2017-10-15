# RPR_Data_Imputation.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the RPR-Data Imputation unit.
#
# Version:  0.2
#
# Date:     2017  10  06
# Author:   Greg Huang
#
# Versions:
#           0.1    First draft learning unit for the RPR-Data Imputation section.
#           0.2    Final version
#
# TODO:
# 2. Use MICE to impute data on a R 'datasets' package dataset, 'trees'.
# 2. Use MICE to impute data on a small GSE4987 dataset with 20 observations.
# 3. Use Hmisc to impute data on a large GSE4987 datset with 6228 observations.
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================
# Sections:
# 0. Load libraries                                          Line 36
# 1. Running MICE on a small synthetic data set              Line 91
# 2. Running MICE on a small subset of the GSE4987 Dataset   Line 147
# 3. Running Hmisc's aregImpute() on a large GSE4987 subset  Line 196
# 4. Exercise solutions                                      Line 243

# Load required packages

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
# help us visualize and analyze our data that contains missing elements before we
# impute them
if (!require(VIM, quietly=TRUE)) {
  install.packages("VIM")
  library(VIM)
}

# The missForest package is used to generate random missing data in our examples.
if (!require(missForest, quietly=TRUE)) {
  install.packages("missForest")
  library(missForest)
}

# The Hmisc package will be used for imputation of large data sets.
if (!require(Hmisc, quietly = TRUE)){
  install.packages("Hmisc")
  library(Hmisc)
}

# Provide one of the datasets for our first example.
if (!require(datasets, quietly = TRUE)){
  install.packages("datasets")
  library(datasets)
}

# Lattice is primarily a tool used to show multivariate relationships in our data.
if (!require(lattice, quietly = TRUE)){
  install.packages("lattice")
  library(lattice)
}

# 1.  Imputation with MICE on an example R dataset
# load in the dataset "trees", a dataset available in R datasets package.
# This dataset shows girth, height, and volume for black cherry trees.
r_db_data <- trees

# Display the structure of the dataseet
str(trees)

# Obtain a summary of the dataset
summary(trees)

# Since this data is complete (no missing values), we will perform a random deletion
# and remove 5% of the data, using the prodNA() function. It is found in the
# missForest package.
r_db_data_missing <- prodNA(trees, noNA = 0.05)

# at this point it is important to realize that we need to remove categorical values.
# in this example, there are none, so we don't need to delete any column of data.

# Obtain a summary of the dataset, now with 5% values removed.
summary(r_db_data_missing)

# Now we are ready to impute our missing data.
# First, we'd like to see if there's a pattern with the missing data.
md.pattern(r_db_data_missing)

# Now, we use mice to impute the missing spots.
# Using the mice function, we look into the r_db_data_missing file.
# m = 5 indicates the default number of imputed datasets, so in this case we'll get 5
# different sets of imputed data for us to select from, or to pool together.
# maxit = 50 is the maximum number of iterations
# method = method for imputing. 'pmm' stands for "predictive mean matching. there are
# other methods available.
imputed_data <- mice(r_db_data_missing, m=5, maxit = 50, method = "pmm")

# We can get a comprehensive look at our imputated data after mice runs.
summary(imputed_data)
# Now that we have 5 sets of data we can plug back to our dataset with missing values,
# Simply run the complete(x,...) method and select any 1 of the 5 lists we imputed.
# I arbitrarily selected set #2.
r_db_data_complete <- complete(imputed_data, 2)

# You can open all three datasets: r_db_data, r_db_data_missing, and r_db_data_complete
# for comparison.

# We can use some plots to have visualizations of our imputed data. The pink icons are
# the imputed numbers. Here we can see that the imputed numbers are rougly in line with
# the existing data.
xyplot(imputed_data, Girth~ Height + Volume, pch=2, cex = 0.5)

# It is also possible to take the 5 sets generated and fit a linear model to the data.
# Using mice, we use the with() function to fit the data, and pool() method the pool
# results together.
fit <- with(data = imputed_data, lm(Girth ~ Height + Volume))
summary(pool(fit))

# 2.  MICE example on the small GSE4987 data subset.
# Now we will apply the same process to the GSE4987 yeast cell cycle dataset.
# Before we get to the GSE examples, we'll load GSE4987 from GEO.
# Adapted from dataPrep.R
GSE4987 <- getGEO("GSE4987", GSEMatrix =TRUE, AnnotGPL=TRUE)
if (length(GSE4987) > 1) {
  idx <- grep("GPL1914", attr(GSE4987, "names"))
} else {
  idx <- 1
}
GSE4987 <- GSE4987[[idx]]
# again: ... Fallback data
# save(GSE4987, file="./data/GSE4987.RData")
# load(file="./data/GSE4987.RData")

# Access contents via methods:
featureNames(GSE4987)   # rows
sampleNames(GSE4987)    # columns

experimentData(GSE4987)
# Access contents by subsetting:
(tmp <- GSE4987[12:17, 1:6] )

# Access data
exprs(tmp)

# load the accessed data into small_GSE_dataset
small_GSE_dataset <- exprs(tmp)

# Get a statistical summary of the small GSE dataset
summary(small_GSE_dataset)
# Running summary() shows that indeed there are no "missing values". As a result,
# Let's randomly put in gaps in our data.
small_GSE_dataset_missing <- prodNA(small_GSE_dataset, noNA = 0.1)

# Again, we can check out the pattern with md.pattern
md.pattern(small_GSE_dataset_missing)

# Impute data and get a summary of the imputations
imputed_small_GSE_data <- mice(small_GSE_dataset_missing, m=5, maxit = 50, method = "pmm")
summary(imputed_small_GSE_data)

# Add the imputed data into the missing parts of
complete_small_GSE_data <- complete(imputed_small_GSE_data, 3)

# Compare: See the differences between the original and the one we experimented with.
summary(small_GSE_dataset)           #original
summary(complete_small_GSE_data)     #after random removal and imputation with MICE

# 3.  Hmisc example on large GSE4987 dataset.
# The Hmisc package is better at handling large datasets. In our case where the data
# frame has more than 6000 entries, imputations done on MICE and missForest tend to
# crash.
# Begin by reading in a much larger set. In this case, we'll get the first 10 samples in GSE4987.
(bigset <- GSE4987[12:6228, 1:10] ) #include 10 of the samples and all 6000+ features/observations.

# access data
exprs(bigset)
large_GSE_dataset <- exprs(bigset)

summary(large_GSE_dataset)

# Upon examining the summary of the large dataset, we find that there are actually already NA's littered throughout each column / sample.
# Here we run the Hmisc imputation method to fill in those blanks. The next three lines set up the "formula" as well as preparing the data
# for the Hmisc function.
colNames <- (sampleNames(GSE4987)[1:10])
impute_colNames <- as.formula(c("~", paste(colNames, collapse = '+')))
large_GSE_dataset_df <- as.data.frame.matrix(large_GSE_dataset)

col_for_xyplot <- sampleNames(GSE4987)[2:10]
xy_formula <- as.formula(c("~", paste(col_for_xyplot, collapse = '+')))


# Run Hmisc. This will take about 30 seconds. We will do 5 different iterations, as per our previous examples.
imputed_large_GSE_data <- aregImpute(formula = impute_colNames, data = large_GSE_dataset_df, n.impute = 5)

# Now we have a list of imputed values available (set of 5).
# We will use impute.transcan() function to select a specific set of imputations and
# Re-insert them into the original data frame with missing values.
# Here, I arbitrarily selected imputation iteration number 2, out of the 5 available.
imputed_transcan <- impute.transcan(imputed_large_GSE_data, data = large_GSE_dataset_df, imputation = 2, list.out = TRUE, pr = FALSE, check = FALSE)
completed_large_GSE_dataset <- as.data.frame(do.call(cbind, imputed_transcan))
completed_large_GSE_dataset <- completed_large_GSE_dataset[,colnames(large_GSE_dataset_df), drop = FALSE]

# Summary of complete_large_GSM_data and note that the spaces have been filled in. (No more NAs)
summary(completed_large_GSE_dataset)

# compare this with the original dataset
summary(large_GSE_dataset)

# This learning unit demonstrated 2 types of data imputation packages available
# in R, and hopefully provided enough information for everyone to be able to
# impute data in future encounters with data sets with missing values.
# End of Learning Unit.

# ======================
# 4.  Exercise solutions
# Congratulations on completing this learning unit for RPR-Data-Imputation. Hope you enjoyed this unit.
# Answers:
# Exercise 1:
md.pattern(airquality)
# Answer:This should show 44 total missing values, and 35 of which have the pattern of just Ozone missing.
#
# Exercise 2:
exercise2_impute_data <- mice(airquality, m=5, maxit = 50, method = "pmm")
exercise2_completed_data <- complete(exercise2_impute_data, 2)
View(exercise2_completed_data)
exercise2_fit_data <- with(data = exercise2_impute_data, lm(Wind ~ Temp+Month+Day+Solar.R+Ozone))
summary(pool(exercise2_fit_data))
# Answer: 0.2404003

# Exercise 3
# Simply change the subsetting from 1:10 to 11:20 for the code in section 3. Compare the values between summary(large_GSE_dataset) and
# summary(completed_large_GSE_dataset). The median does not change for GSM112148, which remains as 0.005550.
# [END]
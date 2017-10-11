# RPR_Data_Imputation.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the RPR-Data Imputation unit.
#
# Version:  0.1
#
# Date:     2017  10  06
# Author:   Greg Huang
#
# Versions:
#           0.1    Learning unit for the RPR-Data Imputation section.

#
# TODO:
# 1. Use MICE to impute data on a R 'datasets' package dataset, 'trees'.  Line
# 2. Use MICE to impute data on a small GSE4987 dataset with 20 observations. Line
# 3. Use Hmisc to impute data on a large GSE4987 datset with 6228 observations. Line
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================
# Set working directory
setwd("C:/Users/Greg/Documents/4th/BCB410/BCB410-DataScience")

# Load required packages
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

# 1.  Imputation with MICE on an example R database
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
# seed 5000 times, an arbitrary number.
imputed_data <- mice(r_db_data_missing, m=5, maxit = 50, method = "pmm", seed = 5000)
# We can get a summary after mice runs.
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

# 2.  MICE example on the small GSE4987 dataset, GSM112133
# Now we will apply the same process to the GSE4987 yeast cell cycle dataset.
GSM_data <- read.csv("~/4th/BCB410/BCB410-DataScience/data/GSM112133-2.csv", header = TRUE)
str(GSM_data)
summary(GSM_data)
# Running summary() shows that in the "Value" section, there are some logical expressions
# mixed into what should be a column of numbers. We will replace those with NA.
# We will also delete  non-numeric columns, which are "Flagged." and "ID_REF".
GSM_data_clean <- GSM_data[-c(1,4)]
GSM_data_clean$VALUE[GSM_data_clean$VALUE == "TRUE"] <- NA

# Again, we can check out the pattern with md.pattern
md.pattern(GSM_data_clean)
# Looks like we don't have any missing data. Let's create 10% NAs.
GSM_data_clean_missing <- prodNA(GSM_data_clean, noNA = 0.1)

# Impute data
imputed_GSM_data <- mice(GSM_data_clean_missing, m=5, maxit = 50, method = "pmm", seed = 5000)
summary(imputed_GSM_data)

# Add the imputed data into the missing parts of GSM_data_clean_missing
complete_GSM_data <- complete(imputed_GSM_data, 3)

# See how well our imputed data plots with existing data
xyplot(imputed_GSM_data, VALUE ~ Ratio + INV_VALUE, pch = 2, cex = 0.5)

# Again, we can pool the imputations together using the with() and pool() functions
GSM_fit <- with(data = imputed_GSM_data, lm(VALUE ~ Ratio + INV_VALUE))
summary(pool(GSM_fit))

# 3.  Hmisc example on large GSE4987 dataset.
# The Hmisc package is better at handling large datasets. In our case where the data
# frame has more than 6000 entries, imputations done on MICE and missForest tend to
# crash.
# Begin by reading in the large CSV file. This is the GSE4987, GSM112133 sample.
large_GSM_data <- read.csv("~/4th/BCB410/BCB410-DataScience/data/GSM112133.csv", header = TRUE)
# Clean out the categorical data. Remaining data are Ratio and INV_VALUE.
large_GSM_data_clean <- large_GSM_data[-c(1,2,4)]

# Get a summary for this cleaned data set, making sure they are numerical.
summary(large_GSM_data_clean)

# Here is the big chunk for Hmisc imputation.
imputed_large_GSM_data_clean <- aregImpute(formula = ~Ratio + INV_VALUE, data = large_GSM_data_clean, n.impute = 5)

# Now we have a list of imputed values available (set of 5).
# We will use impute.transcan() function to select a specific set of imputations and
# Re-insert them into the original data frame with missing values.
# Here, I arbitrarily selected imputation iteration number 2, out of 5 available.
imputed_transcan <- impute.transcan(imputed_large_GSM_data_clean, data = large_GSM_data_clean, imputation = 2, list.out = TRUE, pr = FALSE, check = FALSE)
completed_large_GSM_data <- as.data.frame(do.call(cbind, imputed_transcan))
completed_large_GSM_data <- completed_large_GSM_data[,colnames(large_GSM_data_clean), drop = FALSE]

# Summary of complete_large_GSM_data and note that the spaces have been filled in.
summary(completed_large_GSM_data)

# This learnign unit demonstrated 2 types of data imputation packages available
# in R, and hopefully provided enough information for everyone to be able to
# impute data in future encounters with data sets with missing values.


# [END]

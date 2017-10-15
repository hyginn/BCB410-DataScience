# EDA-CLU-Density_based.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-CLU-Density_based unit.
#
# Version:  0.1
#
# Date:     2017  MM  DD
# Author:   Alana Man
#
# Versions:
#           0.1    Initial commit
#           1.0    This code contains a toy data set example, a more detailed
#                  example explaining how to clustering on 2 features,
#                  a (hopefully) convenient loop to view many pairs of
#                  clustering, and an example of clustering over the entire
#                  data set.
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
set.seed(123) # let's set seed in case we want to reproduce random examples

# Please run this file using the Run Selected Line(s) command (CMD + [enter] on MacOS)

# Sections:
# 1 Setup packages       Line 40
# 2 Warm up example      Line 62
# 3 Yeast data example   Line 101


# = 1  Setup packages

# We get DBSCAN and a variety of other related functions in this
# package. Note that the fcp package also has a DBSCAN function
# but it is much slower than the implementation in the dbscan library
if (!require("dbscan", quietly = TRUE)) {
  install.packages("dbscan")
  library(dbscan)
}

# This package offers a nice visualization tool based on ggplot2
if (!require("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
  library(factoextra)
}

# This is the MICE package, which we will be using to impute our data.
if (!require(mice, quietly=TRUE)) {
  install.packages("mice")
  library(mice)
}

# = 2 Warm up example
# Adapted from the example in the STHDA article: goo.gl/2VbeCf

# = 2.1 Data setup

# Get multishapes data from factoextra
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

# = 2.2 DBSCAN
# Find the optimal value for eps (epsilon)
dbscan::kNNdistplot(df, k =  5)

# Our goal for running the kNNdistplot is to roughly find
# the "knee" or the last point before the sharp increase

# We could probably use derivatives to find the optimal
# eps value but we will just "eyeball" it for this example
abline(h = 0.1, lty = 2) # too late
abline(h = 0.2, lty = 2) # too early
abline(h = 0.15, lty = 2) # looks about right, use eps = 0.15

# Run DBSCAN... explicit namespacing used just in case
# the fcp library is also loaded in the session, which
# provides a much slower version of DBSCAN
db <- dbscan::dbscan(df, eps = 0.15, minPts = 5)

# Plot DBSCAN results using the default plot functions
plot(df, col=db$cluster)

# Add the outliers/noise as + in the plot
points(df[db$cluster==0,], pch = 20, col = "black")

# Plot DBSCAN results using factoextra
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())


# = 3 Yeast data example

# = 3.1 load and sort out the data
load("./data/myGeneFeatures.RData")

# Indices of columns are conveniently recorded so that
# pairs of features can be clustered together in
# section 3.3.

# get the BP features column indices
bp_col_idxs <- grep("^BP[0-9]$", colnames(myGeneFeatures))

# get the time points column indices
t_col_idxs <- grep("^t[0-9]{1,3}$", colnames(myGeneFeatures))

# get the go term label column index
go_terms_idx <- grep("^termName$", colnames(myGeneFeatures))

# = 3.2 Simple cluster for 2 features

# In this section we cluster on time point 0 (t0) and BP1

# Subset for the 2 cols (we need to go term column index for the table)
test_set1 <- c(bp_col_idxs[1], t_col_idxs[1], go_terms_idx)
test1 <- myGeneFeatures[, test_set1]
summary(test1)

# Let's do some data cleanup
# We can either omit all rows with any NAs in the columns
#test1_no_na <- na.omit(test1)

# Or we can impute the data, here I use the workflow suggested
# in the data_imputation unit, except with m=1 since I'll
# just be taking the first pool anyways
test1_no_na <- mice(test1, m=1, maxit = 50, method = "pmm")

# Let's take a look at the new imputed data
summary(test1_no_na)

# Choose the first set of the 5
test1_clean <- complete(test1_no_na, 1)

# Run the kNNdistplot in the dbscan library
dbscan::kNNdistplot(test1_clean[,1:2], k =  5)
abline(h = 0.1, lty = 5) # too early
abline(h = 0.2, lty = 5) # too early
abline(h = 0.3, lty = 5) # looks about right, use eps = 0.3

# Run DBSCAN and store results
db_test1 <- dbscan::dbscan(test1_clean[,1:2], eps = 0.3, minPts = 5)

# The follwing table tells us how many data points from each cluster (rows)
# belong to which GO term. Note that row 0 is always the outlier/noise
#  category, which means that these points do not belong to any cluster
table(db_test1$cluster, test1_clean$termName)

# For example, row 1 corresponds to cluster 1 and there are 24 points with
# the 'DNA replication' GO term attributed to them, and 1 point with the
# 'response to osmotic stress' GO term attributed.

# Note that black circles are outlier/noise points
fviz_cluster(db_test1, data = test1_clean,
             choose.vars = c("BP1", "t0"),
             stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = c("point"),
             palette = "jco", ggtheme = theme_classic())

# We can look at the regular plot to compare.]
plot(test1_clean[,-3], col=db_test1$cluster)

# Add the outliers as the filled in black circles in the plot to match
# the fviz_cluster plot
points(test1_clean[db_test1$cluster==0,-3], pch = 20, col = "black")


# For the next section, please enter numbers when typing in values.
# Error checking is not performed on the inputs so proper inputs are assumed.

# = 3.3 Looped plotting for every time point against every BP feature

# \!!!/ Any time a readline command is executed, please head
# \!!!/ to the console to type in the input

# This sets the time point you would like to plot against all the BP features.
# Please re-run this line before running the for loop again if you would like
# to plot using a different time point. Alternatively you can manually set
# the time_stop variable if that is more convenient.
cat("Pay attention to console starting now...")
time_stop <- readline(prompt="Enter an integer from 0 to 12 to choose a time stop: ")

# Loop through each BP feature column... just place cursor on the for loop line and
# use the Run Selected Line command to execute (use CMD + [enter] on MacOS)
for (bp in bp_col_idxs){

  cat("Clustering ", colnames(myGeneFeatures)[bp], "with ",
      colnames(myGeneFeatures)[as.numeric(time_stop)])

  # Do some subsetting and data clean-up per pair
  # First we get all 3 columns we need for the cluster
  clus_set_idxs <- c(bp, t_col_idxs[as.numeric(time_stop)], go_terms_idx)

  # Subset the data from myGeneFeatures
  clus_set <- myGeneFeatures[, clus_set_idxs]

  # Remove NA values from clus_set
  #clus_set_clean <- na.omit(clus_set)

  # Impute the data to remove NA instead of omit
  clus_set_imputed <- mice(clus_set, m=1, maxit = 50, method = "pmm")
  summary(clus_set_imputed)
  clus_set_clean <- complete(clus_set_imputed, 1)

  # Run the kNNdistplot function, with k = 5. Since each plot is different, we must
  # allow for a customization for epsilon (the eps variable)
  dbscan::kNNdistplot(clus_set_clean[,1:2], k =  5)

  # The following block handles the logic for dynamic setting of the eps value
  s_abline_val <-"Enter your desired abline value to view, \nor press [enter] to continue and use default eps as 0.3: "
  abline_val <- readline(prompt=s_abline_val)

  s_abline_new_val <- "If this is your chosen eps, press [enter] to continue, \nor enter another number to view on the graph: "
  abline_new_val <- "default"

  exit_while <- 0L

  while (abline_val != "" & exit_while == 0L) {
    abline(h = as.numeric(abline_val), lty = 5)
    abline_new_val <- readline(prompt=s_abline_new_val)

    if (abline_new_val == "") {
      exit_while <- 1L
    } else {
      abline_val <- abline_new_val
    }
  }

  if (abline_new_val == "default") {
    abline_val <- 0.3
  }

  # Now that we have the eps value, we can finally cluster
  db_clus <- dbscan::dbscan(clus_set_clean[,1:2], eps = as.numeric(abline_val), minPts = 5)

  # Printing db_clus gives us a summary of how many points are in each cluster,
  # and as always, 0 denotes the outliers/noise.
  print(db_clus)

  # The follwing table tells us how many data points from each cluster (rows)
  # belong to which GO term. Note that row 0 is always the outlier/noise
  #  category, which means that these points do not belong to any cluster
  cat("
      Chart of GO term label distribution for each cluster")
  print(table(db_clus$cluster, clus_set_clean$termName))

  s_plot <- "Press [enter] again to see the plot..."
  readline(prompt=s_plot)

  # Plot using default plotter because fviz_cluster does not work
  # in the for loop for whatever reason
  plot(clus_set_clean[,1:2], col=db_clus$cluster)
  points(clus_set_clean[db_clus$cluster==0L,1:2], pch = 20, col = "black")

  readline(prompt="Press [enter] to stop viewing this plot...")

}

# 3.4 Try to cluster the entire data set (18 dimensional plotting)

# We use the same workflow as in 3.2
# Subset and clean data
test_all_idx <- c(bp_col_idxs, t_col_idxs, go_terms_idx)
test_all <- myGeneFeatures[, test_all_idx]
test_all_clean <- na.omit(test_all)

# Figure out best epsilon value
dbscan::kNNdistplot(test_all_clean[,-19], k =  5)
abline(h = 1.3, lty = 5)

# Run DBSCAN
db_test_all <- dbscan::dbscan(test_all_clean[,-19], eps = 1.3, minPts = 5)

# Take a look at the GO term distrubution for each cluster table
table(db_test_all$cluster, test_all_clean$termName)

# As you will see when you cluster the entire data set and attempt to plot the clustering,
# the axes will read Dim 1 (some %) and Dim 2 (some %). These axes are a further PCA
# reduction of the features. The percent you see displayed next to the dimensions
# correspond to how much variation is "explained". This is typically pretty
# difficult for a human to make sense of since we are reducing so many dimensions.
# You can read more about what happens when you plot using fviz_cluster on results from
# a DBSCAN of high dimensional data in the "Update" in the comment by user jsb:
# https://stats.stackexchange.com/a/263497
fviz_cluster(db_test_all, data = test_all_clean[,-19], stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = c("point"),
             palette = "jco", ggtheme = theme_classic())

# That is the end of the unit! Hopefully it was simple enough to follow along.

# EDA-CLU-Mutual_information.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-CLU-Mutual_information unit.
#
# Version:  1.1
#
# Date:     2017  12  06
# Author:   Dominick Han (dominick.han@mail.utoronto.ca)
#
# Versions:
#           1.2    (Revised unit)
#           1.1    (Fixing up the comments while writing Wiki)
#           1.0    (Finishing, added comments)
#           0.6    (Added GO BP data (does not perform well))
#           0.5    (Added GO time data)
#           0.4    (Added Synthetic data)
#           0.3    (Added MI distance matrixcalculation)
#           0.2    (Added MI calculation script)

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

# Set seed so the result is reproducible
set.seed(0)
library('cluster')

# = 1 Mutual Information
# = 1.1 Calculating Mutual Information
MI_dist <- function(v1, v2) {
  # Purpose:
  #     Calculate Mutual Information between two vectors
  #
  # Parameters:
  #     v1:   vector of int
  #     v2:   vector of int
  #
  # Value:
  #     Return the Mutual Information distance between two vectors

  # Prep data
  min1 <- min(v1, na.rm=TRUE)
  min2 <- min(v2, na.rm=TRUE)
  max1 <- max(v1, na.rm=TRUE)
  max2 <- max(v2, na.rm=TRUE)
  matrix <- matrix(0, max1 - min1 + 1, max2 - min2 + 1)

  # Create the joint probability distribution table
  for (i in seq_along(v1)) {
    matrix[v1[i]-min1+1, v2[i]-min2+1] <- matrix[v1[i]-min1+1, v2[i]-min2+1] + 1
  }

  # Normalize, and calculate the probability distribution table for v1 and v2 separately
  matrix <- matrix / length(v1)
  colS <- colSums(matrix)
  rowS <- rowSums(matrix)

  # Use log to calculate the entropy value, thus the MI between two distributions
  for (i in seq_along(colS)) {
    colS[i] <- colS[i] * log2(colS[i] + (colS[i]==0))
  }
  for (i in seq_along(rowS)) {
    rowS[i] <- rowS[i] * log2(rowS[i] + (rowS[i]==0))
  }
  for (i in seq_along(rowS)) {
    for (j in seq_along(colS)) {
      matrix[i,j] <- matrix[i,j] * log2(matrix[i,j] + (matrix[i,j]==0))
    }
  }

  # Calculation
  MI <- -(sum(colS) + sum(rowS)) / sum(matrix)

  # Added 2 so that the MI_dist() on the same vector would be 0
  return(MI + 2)
}

# = 1.2 Example with random variables
# Random vectors
v1 <- sample(1:5, 10, replace=T)
v2 <- sample(1:5, 10, replace=T)
# These two will output 0 since its the same vector
MI_dist(v1, v1)
MI_dist(v2, v2)
# This distance should be around 0.8
MI_dist(v1, v2)

# = 2 Clustering by Mutual Information
# = 2.1 Clustering by Mutual Information function
cal_dMI <- function(data_frame) {
  # Purpose:
  #     Calculate Mutual Information matrix
  #
  # Parameters:
  #     data_frame: data.frame of our data
  #
  # Value:
  #     Return the Mutual Information distance between two vectors

  # Ignore data frame observations that has all N/A
  ind <- apply(data_frame, 1, function(x) all(is.na(x)))
  data_frame <- data_frame[ !ind, ]

  # Create distance matrix
  dist <- matrix(0, nrow(data_frame), nrow(data_frame))

  # Unpack the data frame
  data <- data.matrix(data_frame)

  # Use MI_dist() to calculate pair-wise MI distance
  for (i in 1:nrow(data_frame)) {
    for (j in 1:i) {
      dist[i, j] <- MI_dist(data[i,], data[j,])
    }
  }

  # Convert it to distance matrix
  dMI <- as.dist(dist)
  return(dMI)
}

# = 2.2 Syntetic data to cluster

synth_data_list <- list()
labels <- list()
# first 20 data points are uniform distribution
for (i in 1:20) {
  synth_data_list[[i]] <- sample(1:20, 200, replace=T)
  labels[[i]] <- "Uniform"
}
# last 20 data points are normal distribution
for (i in 21:40) {
  synth_data_list[[i]] <- as.integer(rnorm(200, mean=10, sd=2))
  labels[[i]] <- "Normal"
}
synth_data <- data.frame(t(data.frame(synth_data_list)))

# Calculate the Mutual Information distance between each data point
cat("Clustering on synthetic data...")
dMI <- cal_dMI(synth_data)
attr(dMI, "Labels") <- labels

# Does hclust, agnes, diana 3 types of hierarchy based clustering
# Note: Here readline() is to prompt user for input
#     So we display plots one by one
readline(prompt="Press <Enter> to show synthetic data hclust...")
plot(hclust(dMI))
readline(prompt="Press <Enter> to show synthetic data agnes...")
plot(agnes(dMI), which.plots=2)
readline(prompt="Press <Enter> to show synthetic data diana...")
plot(diana(dMI), which.plots=2)


# = 3 GO-based feature cluster (actual example)

# Get data from our GO dataset
load("data/myGeneFeatures.RData")

# GO time series data
# Since GO data is only floating points, we need to multiply it to enlarge the data then round it to integer for our MI calculation
GOTimeData <- myGeneFeatures[5:17] * 25

# Calculate the Mutual Information distance between each data point
# WARN: THIS CAN TAKE SOME TIME
cat("Clustering on GO time data...(this will take some time)")
dMI <- cal_dMI(GOTimeData)

# Does hclust, agnes, diana 3 types of hierarchy based clustering
readline(prompt="Press <Enter> to show GO time data hclust...(zoom to see details)")
plot(hclust(dMI))
readline(prompt="Press <Enter> to show GO time data agnes...(zoom to see details)")
attr(dMI, "Labels") <- row.names(GOTimeData)
plot(agnes(dMI), which.plots=2)
readline(prompt="Press <Enter> to show GO time data diana...(zoom to see details)")
plot(diana(dMI), which.plots=2)


# GO BP Terms data (After dimensional reduction)
# Since GO data is only floating points, we need to multiply it to enlarge the data then round it to integer for our MI calculation
GOBPData <- myGeneFeatures[18:22] * 5

# Calculate the Mutual Information distance between each data point
# WARN: THIS CAN TAKE SOME TIME
cat("Clustering on GO Biological Process data...(this will take some time)")
dMI <- cal_dMI(GOBPData)

# Does hclust, agnes, diana 3 types of hierarchy based clustering
readline(prompt="Press <Enter> to show GO Biological Process data hclust...(zoom to see details)")
plot(hclust(dMI))
readline(prompt="Press <Enter> to show GO Biological Process data agnes...(zoom to see details)")
attr(dMI, "Labels") <- row.names(GOTimeData)
plot(agnes(dMI), which.plots=2)
readline(prompt="Press <Enter> to show GO Biological Process data diana...(zoom to see details)")
plot(diana(dMI), which.plots=2)

# = 4 TASK: Try creating synthetic data with several distributions,
#           and see if MI clustering can classify them correctly
#           For example:
#             * Uniform
#             * Normal
#             * Geometry
#             * Poisson



















# Solution:
synth_data_list <- list()
labels <- list()
# 10 data points of uniform distribution
for (i in 1:10) {
  synth_data_list[[i]] <- sample(1:20, 200, replace=T)
  labels[[i]] <- "Uniform"
}
# 10 data points of normal distribution
for (i in 11:20) {
  synth_data_list[[i]] <- as.integer(rnorm(200, mean=10, sd=3))
  labels[[i]] <- "Normal"
}
# 10 data points of poisson distribution
for (i in 21:30) {
  synth_data_list[[i]] <- as.integer(rpois(200, lambda=1))
  labels[[i]] <- "Poisson"
}
# 10 data points of geometry distribution
for (i in 31:40) {
  synth_data_list[[i]] <- as.integer(rgeom(200, prob=0.4))
  labels[[i]] <- "Geometry"
}
synth_data <- data.frame(t(data.frame(synth_data_list)))

# Calculate the Mutual Information distance between each data point
dMI <- cal_dMI(synth_data)
attr(dMI, "Labels") <- labels
plot(agnes(dMI), which.plots=2)

# [END]

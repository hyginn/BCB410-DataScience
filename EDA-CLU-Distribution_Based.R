# EDA-CLU-Distribution_Based.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-CLU-Distribution Based unit.
#
# Version:  0.1
#
# Date:     2017  10  14
# Author:   Marcus Chiam
#
# Versions:
#           0.1    Learning unit for the EDA-CLU-Distribution_Based section.
#
# TODO:
# 1. Install packages
# 2. Generate synthetic data
# 3. Perform clustering on synthetic data
# 4. Compare clustering results with synthetic data clusters
# 5. Get data from myGOExSet.RData
# 6. Clean the data
# 7. Perform clustering on the data
# 8. Observe results of clustering
# 9. Plot the data for visualization

# =    1. Install packages  =========================================================================

# we will be using mlbench to generate synthetic data
if (!require(mlbench, quietly = TRUE)) {
  install.packages("mlbench")
  library(mlbench)
}
# we will be using the Mclust function in the mclust package for clustering
if (!require(mclust, quietly = TRUE)) {
  install.packages("mclust")
  library(mclust)
}
# we will be using a plotting function from factoextra to plot our data
if (!require(factoextra, quietly = TRUE)) {
  install.packages("factoextra")
  library(factoextra)
}

# =    2. Generate synthetic data  ==================================================================

# we will first start by confirming our clustering algorithm functions properly
# by using it on synthetic data and comparing its results with the synthetic data clusters

# generate 100 2D data points that are grouped into one of 2 classes (clusters)
synthetic_data <- mlbench.2dnormals(100,2)
# synthetic_data$x is a matrix that shows the data points that were generated
head(synthetic_data$x)
# synthetic_data$classes is a vector that shows what cluster number each data point is assigned to
head(synthetic_data$classes)

# plot the points
plot(synthetic_data$x)
# plot the points with the synthetic cluster groupings
plot(synthetic_data)

# =    3. Perform clustering on synthetic data ======================================================

# model-based-clustering using the Mclust function
# type ?Mclust for more details about the function outputs
clustered_synthetic_data <- Mclust(synthetic_data$x)

# =    4. Compare clustering results with synthetic data clusters ===================================

# print a summary
summary(clustered_synthetic_data)

# mc$G shows the optimal number of clusters
clustered_synthetic_data$G

# plot the points with the cluster groupings
plot(clustered_synthetic_data,"classification")

# compare the the synthetic data clusters, with the clusters generated from the Mclust function
# cycle between the two plots below:
# synthetic clusters
plot(synthetic_data)
# Mclust clusters
plot(clustered_synthetic_data,"classification")
# the clustering groupings should look similar!

# to look into more detail, you can also check the cluster assignments for each individual data point
# synthetic cluster assignments
synthetic_data$classes
# Mclust cluster assignments
clustered_synthetic_data$classification
# there should be some similarity between the two vectors!

# keep in mind, the cluster number is arbitrary, so there may be cases where the synthetic clustering
# assigned a cluster as "1", but the Mclust cluster assignment assigned that same cluster as "2"
# in which case, you would expect to see the majority of data points from the synthetic cluster "1",
# being assigned to cluster "2" in the Mclust classification

# EXAMPLE: synthetic cluster assignment vector --> 1 1 1 2 1 2
# Mclust cluster assignment vector --> 2 2 2 1 2 1
# in the above example, the cluster number assignments are different between the two vectors
# but the important thing is that the data points are grouped the same way in both vectors,
# just that the clusters have different number labels

# before we move to real data, I would encourage you to play around more with mlbench
# and try generating different data sets, perhaps with a different number of data points
# and a different number of classification/clusters

# =    5. Get data from myGOExSet.RData  ============================================================

# hopefully the exercise above will convince you that the Mclust function works properly
# now let's move onto real data

# import the myGOExSet data set
data(myGOExSet)
head(myGOExSet,3)

# =    6. Clean the data  ===========================================================================

# exclude character columns; only numeric columns denoting time remains
df <- myGOExSet[,-1:-4]
head(df)
# we need to resolve any NA values, since Mclust will raise an error if we have any NA values
# impute data for NA values
df_no_na <- imputeData(df)
# alternatively we can omit NA values (this will remove the entire gene row) by typing:
# df_no_na <- na.omit(df)

# =    7. Perform clustering on the data ============================================================

# model-based-clustering using the Mclust function
# type ?Mclust for more details about the function outputs
mc <- Mclust(df_no_na)

# =    8. Observe results of clustering =============================================================

# print a summary
summary(mc)

# mc$G shows the optimal number of clusters
mc$G

# mc$z is a matrix showing the probability of each gene belonging to a particular cluster
head(mc$z,30)
# mc$classification shows the cluster assignement of each gene
head(mc$classification,30)
# mc$uncertainty shows the uncertainty associated with each classification
head(mc$uncertainty,30)

# =    9. Plot the data for visualization ===========================================================

# plot the data; each data point is color-coded to show which cluster it belongs to
fviz_mclust(mc,"classification",geom="point",pointsize=1.5,palette="jco")
# plot the data; the larger the data point on the plot, the higher the uncertainty
fviz_mclust(mc,"uncertainty",palette="jco")

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
# 10. Revisiting our methods and thinking of alternatives

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
# we will be using a data imputation function from the mix package
# to deal with NA values in our gene expression data set
if (!require(mix, quietly = TRUE)) {
  install.packages("mix")
  library(mix)
}
# we will use 3dplot as an alternative plotting tool
if (!require(rgl, quietly = TRUE)) {
  install.packages("rgl")
  library(rgl)
}
# =    2. Generate synthetic data  ==================================================================

set.seed(100)

# we will first start by confirming our clustering algorithm functions properly
# by using it on synthetic data and comparing its results with the synthetic data clusters

# the function mlbench.2dnormals(n, cl=2, r=sqrt(cl), sd=1) generates n normally distributed
# 2D data points. The parameter cl refers to the number of classes (or clusters)
# in the data set. More information on the parameters can be found here:
# https://www.rdocumentation.org/packages/mlbench/versions/2.1-1/topics/mlbench.2dnormals

# we will use mlbench.2dnormals to generate 100 2D data points that are grouped into one of 2 classes (clusters)
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

# Now that we have the generated data points and their associated cluster assignments,
# let's use the Mclust function to apply distribution-based clustering on the data points.
# Then we can compare how effective our Mclust function is at clustering by comparing its
# clustering assignment results with the clustering assignments from mlbench.2dnormals

# model-based-clustering using the Mclust function
# type ?Mclust for more details about the function outputs
clustered_synthetic_data <- Mclust(synthetic_data$x)

# =    4. Compare clustering results with synthetic data clusters ===================================

# After we pass our model through the Mclust function, let's take a look at the results

# the $G attribute shows the number of clusters that Mclust decided was optimal for the data set
clustered_synthetic_data$G
# result is 2, which is the same as ml2bench.2dnormals; so far so good

# plot the points with the cluster groupings
plot(clustered_synthetic_data,"classification")

# compare the the synthetic data clusters, with the clusters generated from the Mclust function
# cycle between the two plots below:
# synthetic clusters
plot(synthetic_data)
# Mclust clusters
plot(clustered_synthetic_data,"classification")
# the cluster groupings look pretty similar!

# to look into more detail, we can also check the cluster assignments for each individual data point
# synthetic cluster assignments
synthetic_data$classes
# Mclust cluster assignments
clustered_synthetic_data$classification
# The two vectors are quite similar!

# To quantify this similarity, we can see what percentage of data points have the same assignments
# between the two vectors.
# We make a logical vector that contains a TRUE value for each data point that has
# the same clustering assignment in both vectors, and FALSE otherwise.
# We then sum up the TRUE values and divide by 100 (since we have a total of 100 data points)
sum(synthetic_data$classes == clustered_synthetic_data$classification)/100
# 92% similarity...not bad!

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

# exclude character columns; only keep the numeric columns denoting time points
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

# mc$G shows the optimal number of clusters
mc$G

# mc$z is a matrix showing the probability of each gene belonging to a particular cluster
head(mc$z,30)
# mc$classification shows the cluster assignement of each gene
head(mc$classification,30)

# =    9. Plot the data for visualization ===========================================================

# Since we are clustering all points in a time-series data set,
# each time point is treated as its own dimension.
# Therefore with 13 different time points, we are working with 13-dimensional data.
# We can confirm this by typing mc$d which tells us how many dimensions we are working with
mc$d

# Unfortunately, there isn't one good and effective way to visualize high dimensional data
# On a 2d plot, we can try to plot on only two dimensions and see how they cluster
fviz_cluster(mc,axes=c(1,2),pointsize=1.0,labelsize=0)
# Each data point is color-coded to show which cluster it belongs to

# we can manipulate which dimensions to show with axes parameter; it takes a vector of length 2
# c(1,2) shows dimensions 1 and 2 on the plot
# lets take a look dimensions 3 and 4
fviz_cluster(mc,axes=c(3,4),pointsize=1.0,labelsize=0)

# still, even with this, it is quite hard to visualize our data

# Another alternative is to plot in 3 dimensions, with each dimension being one cluster group.
# We are fortunate (this time) to have our data be clustered into only 3 groups.
# By plotting in this way, data points that are more likely to belong to a certain cluster,
# will be represented by how far along they are on the axis of that cluster group

mc$z
# a reminder that the $z attribute is a matrix showing the probability of
# each gene belonging to a particular cluster

# now plot it in 3D
plot3d(mc$z)
# seems that very few genes belong to cluster 3
# this can be confirmed by trying:
length(which(mc$classification == 3))
# only 8 out of the 281 genes are clustered into group 3

# it seems our Mclust function may not be performing so well...
# or perhaps it's due to the data itself?

# =    10. Revisiting our methods and thinking of alternatives ===========================================================

# Let's look at our cluster assignments again
mc$classification

# data points belonging to the same cluster may have some kind of meaning in context
# to the data set we are working with.
# Perhaps having similar gene expression profiles (with regards to the time-series) may help
# elucidate their function.
# Although it is very difficult to draw conclusions, seeing as we're working with so many dimensions.
# It may be better to try clustering only between two time points, rather than 12

# remove all other time points except the first two columns
two_dimensional_data <- df_no_na[,-3:-13]

# cluster
mc_tdd <- Mclust(two_dimensional_data)

# sanity check, to make sure we only have two dimensions
mc_tdd$d

mc_tdd$G
# 3 clusters generated again...hm...

# let's look at the classifications
mc_tdd$classification

# and plot
plot(mc_tdd, "classification")

# Whether this plot has more "meaning" than the previous one, is hard to say,
# However, the context is easier to interpret:
# genes that are clustered in the same group, must have similar gene expression profiles
# between the two dimensions we had in our data, i.e. the time points t0 and t10.

# Arguably, this result is easier to interpret as well as being easier to make inferences
# to potential applications of this result.
# Although, if we wanted to be thorough, we would have to cluster each pair of dimensions
# (t0 with everything, t1 with everything, ..., t120 with everything) which would take a
# lot of time.

# But this result, I believe, is more substantial and the time invested will be worthwhile

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

#
# TODO: get the OPTICS clustering to plot
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# = 1  Setup packages

if (!require("dbscan", quietly = TRUE)) {
  install.packages("dbscan")
  library(dbscan)
}
if (!require("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
  library(factoextra)
}

# = 2 Warm up example
# adapted from the example in the STHDA article: goo.gl/2VbeCf

# = 2.1 data setup
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

# = 2.2 DBSCAN
# find the optimal value for eps (epsilon)
dbscan::kNNdistplot(df, k =  5)

# our goal is to roughly find the "knee" or the
# last point before the sharp increase

# we could probably use derivatives to find the optimal
# eps value but we will just "eyeball" it for this example
abline(h = 0.1, lty = 2) # too late
abline(h = 0.2, lty = 2) # too early
abline(h = 0.15, lty = 2) # looks about right, use eps = 0.15

set.seed(123)

# run DBSCAN
db <- dbscan::dbscan(df, eps = 0.15, minPts = 5)

# plot DBSCAN results using the default plot functions
plot(df, col=db$cluster)

# add the outliers as + in the plot
points(df[db$cluster==0,], pch = 3, col = "grey")

# plot DBSCAN results using factoextra
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

# = 3 Yeast data example

# = 3.1 load yeast data
# !!!!!! this data comes from the repo https://github.com/hyginn/BCH2024
# if you would like to get this data please first download the file
# to the same directory as where this file is saved. This is a temporary
# file that I am using to test for syntax errors and proof of concept
# before the "real" data is commited to the repo. Also note that this
# data isn't very good for DBSCAN, as most of the points are densely
# populated in one area.
load("ygProfiles.RData")

ygProfiles <- ygProfiles[12:nrow(ygProfiles), ]

# = 3.2 DBSCAN
dbscan::kNNdistplot(ygProfiles, k = 5)

# our goal is to roughly find the "knee" or the
# last point before the sharp increase

# we could probably use derivatives to find the optimal
# eps value but we will just "eyeball" it for this example
abline(h = 0.5, lty = 2) # looks about right

cl <- dbscan::dbscan(ygProfiles, eps = 0.5, minPts = 5)

# plot DBSCAN results using the default plot functions
plot(ygProfiles, col=cl$cluster)

# add the outliers as + in the plot
points(ygProfiles[cl$cluster==0], pch = 3, col = "grey")

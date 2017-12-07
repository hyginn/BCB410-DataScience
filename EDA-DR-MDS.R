#EDA-DR-MDS.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-DR-MDS unit.
#
# Version:  1
#
# Date:     2017  12  06
# Author:   Ricardo Ramnarine (ricardo.ramnarine@mail.utoronto.ca)
#
# Versions:
#           0.1    (Initial - skeleton template)
#           0.2    (Added section for dependencies of unit)
#           0.3    (Added section 2: Multidimensional Scaling)
#           0.4    (Added section 9: Multidimensional Scaling with Biological relevance)
#           0.5    (Added section 2.2: Nonmetric Multidimensional Scaling)
#           0.6    (Added section 9: Multidimensional Scaling with Biological relevance)
#           1      (Updated sections according to feedback received)
#
#
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# = 1 Dependencies

if (!require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(devtools, quietly=TRUE)) {
  install.packages("devtools")
  library(devtools)
}

if (!require(MASS, quietly=TRUE)) {
  devtools::install_github("MASS")
  library(MASS)
}

if(!require(scatterplot3d, quietly=TRUE)) {
  install.packages("scatterplot3d")
}

if (!require(Biobase, quietly=TRUE)) {
  biocLite("Biobase")
  library(Biobase)
}

if (!require(parallel, quietly=TRUE)) {
  biocLite("parallel")
  library(parallel)
}

if (!require(ggplot2, quietly=TRUE)) {
  biocLite("ggplot2")
  library(ggplot2l)
}

#Load Libraries
library("scatterplot3d")
library(MASS)

source("https://bioconductor.org/biocLite.R")

#Load Data
load("./data/GSE3635.RData")


# = 1.1 Multidimensional Scaling (MDS) Dimension Reduction techniques
# Read this section on the wiki page and then come back and work through the example.

#####################################################

# = 2 Multidimensional Scaling

# = 2.1 Classical/Metric Multidimensional Scaling

#Let's create a matrix that satisfies the euclidean distance matrix so we can apply classical MDS.
set.seed(50)
euclidean_distance_matrix <- matrix(1, nrow = 4, ncol = 4)
euclidean_distance_matrix[row(euclidean_distance_matrix) == col(euclidean_distance_matrix)] <- 0
euclidean_distance_matrix

#Let's calculate the distance matrix
euclidean_distance <- dist(euclidean_distance_matrix)
euclidean_distance

#The Following example is adapted from Lecture 8: Multidimensional scaling by Sungkyu Jung, a Professor at the University of Pittsburgh
#which is cited here http://steipe.biochemistry.utoronto.ca/abc/students/index.php/EDA-DR-MDS#Sources

#Great, now let's apply metric multidimensional scaling to this
#R has a built in function called cmdscale that applies Classical MDS.
euclidean <- cmdscale(euclidean_distance,eig=TRUE, k=3)
euclidean
x <- euclidean$points[,1]
y <- euclidean$points[,2]
z <- euclidean$points[,3]

#scatterplot3d is from library scatterplot3D
euclidean_plot <- scatterplot3d(x=x, y=y, z=z, color="blue", main = "Euclidean Distance Matrix MDS")
p1 <- euclidean_plot$xyz.convert(x[1],y[1],z[1])
p2 <- euclidean_plot$xyz.convert(x[2],y[2],z[2])
p3 <- euclidean_plot$xyz.convert(x[3],y[3],z[3])
p4 <- euclidean_plot$xyz.convert(x[4],y[4],z[4])
#Can you guess what shape it's making?
segments(p1$x,p1$y,p2$x,p2$y,lwd=2,col=2)
segments(p1$x,p1$y,p3$x,p3$y,lwd=2,col=2)
segments(p1$x,p1$y,p4$x,p4$y,lwd=2,col=2)
segments(p2$x,p2$y,p3$x,p3$y,lwd=2,col=2)
segments(p2$x,p2$y,p4$x,p4$y,lwd=2,col=2)
segments(p3$x,p3$y,p4$x,p4$y,lwd=2,col=2)
#We made a tetrahedron!


# 2.2 = Nonmetric Multidimensional Scaling

#Let's use the same data Boris used in his DR-PCA example. The data "crabs" contains morphological
#measurements of the species "Leptograpsus variegatus" from Fremantle, W. Australia.
#The columns contain [species, sex, index, FL, RW, CL, CW, and BD] in that order which represent:
#species := species type. "B" for Blue, "O" for Orange
#sex := gender. "M" for Male, "F" for Female
#FL := frontal lobe size
#RW := rear width
#CL := carapace length
#CW := carapace width
#BD := body depth
#for more information type ?crabs in the console

#index :=  1 <= index <= 50 represents the index of the particular crab within each of the
#         four groups (B + F,B + M,O + F,O + M)

set.seed(100)
#crabs is from library MASS
data(crabs)
measurements <- crabs

#have a look at our data called measurements
measurements

#Let's create a distance matrix using the built in function dist()
#Notice i'm removing the species and sex columns
dist_mat<- dist(measurements[c(-1,-2)])

#sanity check that d is of type "dist", a distance structure
class(dist_mat)
#Look at the data in d.
dist_mat
#It is clearly not a euclidean distance matrix (it isn't a symmetrical matrix
#with diagionals = 0), so we will apply nonmetric MDS.
#isoMDS is a function that comes with package MASS.
#It performs Kruskal's non-metric MDS on a euclidean distance matrix.
#parameter k is the desired dimension for the solution.
result <- isoMDS(dist_mat, k=2)
result
#isoMDS returns a stress percentage. Therefore, anything below 5 is good or better. i.e less than 5%
#Notice the value stress of ~0.78. Don't remember how to interpret the result?
#Refer to the wiki section "How MDS Works"


#Let's create useful labels, i.e factors that characterize which columns each crab belongs
#to (ex. B&F, B&M, O&F, O&M)
result_frame <- as.data.frame(result)
#remove third column
result_frame <- result_frame[-3]
descriptions <- paste(measurements[,1], measurements[,2], sep="&")
#let's create some factors so we can categorize variables in the graph
Groups <- factor(descriptions)
#sanity check
Groups

#plot it using ggplot
ggplot(result_frame, aes(x=points.1, y=points.2, group=Groups)) +
  geom_point(aes(shape=Groups, color=Groups))

#Notice how crabs from the same group tend to cluster closely together.
#This suggests they share more characteristics between each other than other crabs




#9 = Multidimensional Scaling with Biological relevance

#Let's now try to apply MDS techniques to our Yest cycle data.
set.seed(30)

#Store our data called measurements and check it's dimensions and class

#Step 1: split data into times based on Yeast cycle time point.
#isolating time 0
GSE <- exprs(GSE3635)
V2 <- substr(featureNames(GSE3635), 0, 3) == 'YAR'
V2
YAR <- GSE[V2,]
YAR
#sanity check
dim(YAR)
class(YAR)
#calculate distance matrix
YAR_DIST <- dist(YAR)

#sanity check
YAR_DIST
#It is clearly not a euclidean distance matrix (it isn't a symmetrical matrix
#with diagionals = 0), so we will apply Kruskal's non-metric MDS on a euclidean distance matrix.
#parameter k is the desired dimension for the solution.
result_2D <- isoMDS(YAR_DIST, k=2)
result_2D
#our stress value is ~11.9% which is too high. Thus, to get more accurate results,
#we must increase the number of dimensions.
#Of course, visualizing and understanding higher than a 3 dimensional plot isn't realistic.
#since visualizing 7 dimensions isn't very helpful, let's continue with the 3D results, out of curiousty

YAR_2D <- as.data.frame(result_2D)
YAR_2D

#create factor with only features that start with YAR from GSE3635
fact <- featureNames(GSE3635)[substr(featureNames(GSE3635), 0, 3) == 'YAR']
#sanity check
fact
#plot it
ggplot(YAR_2D, aes(x=points.1, y=points.2, group=fact)) +
  geom_point()
#From the plot above, we can't really say much because it's cargo cult. The data isn't reliable with a high stress value

#Let's also plot it in 3D
result_3D <- isoMDS(YAR_DIST, k=3)
result_3D
x_3 <- result_3D$points[,1]
y_3 <- result_3D$points[,2]
z_3 <- result_3D$points[,3]
mystery <- scatterplot3d(x=x_3, y=y_3, z=z_3, color="blue", main = "YAR MDS")
descriptions <- paste(measurements[,1], measurements[,2], sep="&")
#again, can't say much about the data based on these results since a high stress value adds more distortion.


# = 10 Tasks to do

# = 10.1 First task: Mystery Matrix
#Given the Matrix mystery_matrix below, run an appropriate MDS algorithm.
mystery_matrix <- matrix(1, nrow = 4, ncol = 4)
mystery_matrix[row(mystery_matrix) == col(mystery_matrix)] <- 0
mystery_matrix[row(mystery_matrix) == col(mystery_matrix)+1] <- 3
mystery_matrix[row(mystery_matrix)+1 == col(mystery_matrix)] <- 3
mystery_matrix[row(mystery_matrix)+2 == col(mystery_matrix)] <- 5
mystery_matrix[row(mystery_matrix) == col(mystery_matrix)+2] <- 5
mystery_matrix

# 10.2 Task 2: The Plot Thickens...
#Plot the results of your mystery matrix

# 10.3 Task 3:  Interpret
# Interpret the results of your mystery matrix

# = 10.4 Task 4: Stressing out? Read more to find out one great way to reduce stress.
#For the biological data, figure out what dimension would give us a "good" stress value.



# = 99  Task solutions

# = 99.1  First task: Mystery Matrix

# Hopefully you noticed this matrix was a Euclidean Distance matrix, so we will use cmdscale.
mystery_matrix <- dist(mystery_matrix)
mystery_matrix
mystery <- cmdscale(mystery_matrix,eig=TRUE, k=3)
mystery


# = 99.2 Task 2: The Plot Thickens...
#In fact, this matrix is suspiciously similar the example I used...
x_3 <- mystery$points[,1]
y_3 <- mystery$points[,2]
z_3 <- mystery$points[,3]
mystery <- scatterplot3d(x=x_3, y=y_3, z=z_3, color="blue", main = "Mystery Matrix MDS")
p1 <- mystery$xyz.convert(x_3[1],y_3[1],z_3[1])
p2 <- mystery$xyz.convert(x_3[2],y_3[2],z_3[2])
p3 <- mystery$xyz.convert(x_3[3],y_3[3],z_3[3])
p4 <- mystery$xyz.convert(x_3[4],y_3[4],z_3[4])
segments(p1$x,p1$y,p2$x,p2$y,lwd=2,col=2)
segments(p1$x,p1$y,p3$x,p3$y,lwd=2,col=2)
segments(p1$x,p1$y,p4$x,p4$y,lwd=2,col=2)
segments(p2$x,p2$y,p3$x,p3$y,lwd=2,col=2)
segments(p2$x,p2$y,p4$x,p4$y,lwd=2,col=2)
segments(p3$x,p3$y,p4$x,p4$y,lwd=2,col=2)


#10.3 Task 3:  Interpret
#We made a tetrahedron again! Notice the tetrahedron has shifted its position from my original example,
#but the shape is the same! The orientation of the points don't matter because the axes have
#no significance.

# = 99.4  Task 4: Stressing out? Read more to find out one great way to reduce stress.
#A good stress value is under 5%. Let's check k=6.
temp <- isoMDS(YAR_DIST, k=6)
temp
#no good, but it turns out that 7 works.
good_result <- isoMDS(YAR_DIST, k=7)
good_result



# [END]


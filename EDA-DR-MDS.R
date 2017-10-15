#EDA-DR-MDS.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-DR-MDS unit.
#
# Version:  0.1
#
# Date:     2017  10  07
# Author:   Ricardo Ramnarine (ricardo.ramnarine@mail.utoronto.ca)
#
# Versions:
#           0.1    (Initial - skeleton template)
#           0.2    (Added section for dependencies of unit)
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

if (!require(here, quietly=TRUE)) {
  devtools::install_github("krlmlr/here")
  library(here)
}


if(!require(scatterplot3d, quietly=TRUE)) {
  install.packages("scatterplot3d")
}

#Load Libraries
library("scatterplot3d")
library(MASS)

#Load Functions
source("EDA-DR-MDS/performMDS.R")

#Load Data
load("./data/GSE3635.RData")


# = 1.1 Multidimensional Scaling (MDS) Dimension Reduction techniques
# There are 2 flavours of MDS, each of which aim to reduce dimensions to as low as possible while
#maintaining a good stress value. More on stress values later.
# 2. Classic/Metric
  #If there is a metric distance between coordinates in the matrix, the MDS technique you should use is
  #Classic/Metric.
  #Metric:= Any pair of objects are considered metric if dist(x,y) >= 0, d(x,y) = 0 <=> x = y, d(x,y) = d(y,x),
  # and d(x,z) <= d(x,y) + d(y,x)
# 3. Nonmetric
  #If there isn't a metric distance between coordinates in the matric, the MDS technique you
  #should use is nonmetric.


#####################################################

# = 2 Multidimensional Scaling

# = 2.1 Classical/Metric Multidimensional Scaling

#Let's create a matrix that satisfies the euclidean distance matrix so we can apply classical MDS.
set.seed(50)
#TODO: convert to function
euclidean_distance_matrix <- matrix(1, nrow = 4, ncol = 4)
euclidean_distance_matrix[row(euclidean_distance_matrix) == col(euclidean_distance_matrix)] <- 0
euclidean_distance_matrix

#Let's calculate the distance matrix
euclidean_distance <- dist(euclidean_distance_matrix)
euclidean_distance

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
d <- dist(measurements[c(-1,-2)])

#sanity check that d is of type "dist", a distance structure
class(d)
#Look at the data in d.
d
#It is clearly not a euclidean distance matrix (it isn't a symmetrical matrix
#with diagionals = 0), so we will apply nonmetric MDS.
#isoMDS is a function that comes with package MASS.
#It performs Kruskal's non-metric MDS on a euclidean distance matrix.
#parameter k is the desired dimension for the solution.
result <- isoMDS(d, k=2)
result
#Notice the value stress of ~0.78. What is it?
#Stress is the difference between actual and predicted distances.
#The closer to 0 the stress value, the better the prediction.
#In 1964, Kruskal gave guidelines of how to interpret what a good stress value was.
#Stress Goodness-of-fit
#0.200 poor
#0.100 fair
#0.050 good
#0.025 excellent
#0.000 perfect
#isoMDS returns a stress percentage. Therefore, anything below 5 is good or better. i.e less than 5%

#Let's create useful labels, i.e factors that characterize which columns each crab belongs
#to (ex. B&F, B&M, O&F, O&M)
d <- as.data.frame(result)
d <- d[-3]
descriptions <- paste(measurements[,1], measurements[,2], sep="&")
#let's create some factors so we can categorize variables in the graph
Groups <- factor(descriptions)
#sanity check
Groups

#plot it using ggplot
ggplot(d, aes(x=points.1, y=points.2, group=Groups)) +
  geom_point(aes(shape=Groups, color=Groups))

#Notice how there seems to be a clusering of crabs from the same groups. This is because
#their characteristics are more related to each other than crabs from other groups.




#9 = Multidimensional Scaling with Biological relevance


set.seed(30)

#Store our data called measurements and check it's dimensions and class

#Step 1: split data into times based on Yeast cycle time point.
#isolating time 0
GSE <- exprs(GSE3635)
V2 <- substr(featureNames(GSE3635), 0, 3) == 'YAR'
V2
V2 <- GSE[V2,]

#sanity check
dim(V2)
class(V2)
#calculate distance matrix
d <- dist(V2)

#sanity check
d
#It is clearly not a euclidean distance matrix (it isn't a symmetrical matrix
#with diagionals = 0), so we will apply Kruskal's non-metric MDS on a euclidean distance matrix.
#parameter k is the desired dimension for the solution.
result_2D <- isoMDS(d, k=2)
result_2D
#our stress value is ~11.9% which is too high. Thus, to get more accurate results,
#we must increase the number of dimensions. i.e k

#turns out that we need to increase the number of dimensions to improve the stress value.
#Of course, visualizing and understanding higher than a 3 dimensional plot isn't realistic.


#since visualizing 7 dimensions isn't very helpful, let's continue with the 3D results, out of curiousty


d_2D <- as.data.frame(result_2D)
d_2D
v <- featureNames(GSE3635)

#create factor with only features that start with YAR from GSE3635
fact <- featureNames(GSE3635)[substr(featureNames(GSE3635), 0, 3) == 'YAR']
#sanity check
fact
#plot it
ggplot(d_2D, aes(x=points.1, y=points.2, group=fact)) +
  geom_point()
#From the plot above, we can't really say much because it's cargo cult. The data isn't reliable with a high stress value

#Let's also plot it in 3D
result_3D <- isoMDS(d, k=3)
result_3D
x_3 <- result_3D$points[,1]
y_3 <- result_3D$points[,2]
z_3 <- result_3D$points[,3]
mystery <- scatterplot3d(x=x_3, y=y_3, z=z_3, color="blue", main = "YAR MDS")
descriptions <- paste(measurements[,1], measurements[,2], sep="&")
#again, can't say much about the data based on these results.


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
temp <- isoMDS(d, k=6)
temp
#no good, but it turns out that 7 works.
good_result <- isoMDS(d, k=7)
good_result



# [END]


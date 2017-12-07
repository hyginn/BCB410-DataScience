# abstract, more generlizing, finding patterns, dataset,

#contents=> link lots of materials, you, just put what important information ,
#summary everything in your own words;

# choose random variable, "zero", threshhold, cover,

# how different kernel affects the graph,

#pearson relation, grand matrix to put it,

# what that means for the structure
# nodes

# calculate the centrality measure,

# basic statistac of graph;  cover simple terminology, what does not mean?



# add Q and A section;
# nodes, coarse, certain ,
# set. seed.

# give more example,

# subset can be larger, b

# consistent code style,






# objective,

# more explanation of code, provide more information,

# task1, some of task implement in the code, paper, interesting stuff, estimate the connectivity,
#repository,

# cover differnt types graph, weighted, and unweightt

# large graph, important to visualize it, useful, use to purge the data;

# more comments , why do i need to , what do i need,

# better variable mean? relavent meaning,

#








# EDA-Graph_data_mining.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA Graph data mining unit.
#
# Version:  0.1
#
# Date:     2017  10  14
#
# Author:   Kevin Lin ( jianbin.lin@mail.utoronto.ca)
#
# Versions History:
#           0.1    (This is first verison edited on Oct 14 2017)

#
# TODO:
#========================================Table of Content====================================================================
#=================== section 1.1:Create synthetic data adjancency matrix           ==========================================
#=================== section 1.2 build gene expression data adjancency matrix      ==========================================
#=================== section 2.1: use "network" package to create network object and visualize the data======================
#=================== section 2.2 use network package to visualize gene expressioin data set =================================
#=================== section 3.1:use igraph package to create igraph object from synthetic data and visualize the data=======
#=================== section 3.2 use igraph to visualize the dataset           ========================================
#
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
#========== section 1.1:Create synthetic data adjancency matrix ===============

# set seed number in order to have same random values
  set.seed(120)

# create a random vector that represents matrix with average value 9 and SD 2.
  vector1 <- rnorm(96,9,2)
  vector1

# create a matrix with 16 rows and 6 column; number of rows represent vertices and column
# represent different experiment value
  matrix1 <- matrix(data=vector1, nrow = 16, ncol =6)
  matrix1

# lets assgin a name to each vertices
  verticesname <- c("V1", "V2", "V3","V4", "V5","V6", "V7", "V8","V9",
                    "V10","V11","V12","V13","V14", "V15","V16")
  verticesname

# since row represent the number of vertices. we need to transpose matrix prior applying cor function

  transposematrix1 <- t(matrix1)
  transposematrix1

# create a correlation matrix
  correlationmatrix = cor(transposematrix1)
  correlationmatrix


# As you can see he value of pearson correlation coefficient is between -1 to 1. The vertice's correaltion to
# itself is always is always 1 since it loops to itself.

# Task 1.1: What does postive correlation coefficient mean and negative correlaton coefficient mean?

# Transfer correlation matrix into adjanceymatrix. Lets choose threhold value to 0.1.
  adjancencymatrix <- correlationmatrix
  adjancencymatrix

for (a in 1:nrow( adjancencymatrix))
  {
    for (b in 1:ncol( adjancencymatrix))
    {
      if( adjancencymatrix[a,b] > 0.1)
      {
        adjancencymatrix[a,b] <-1
      }

      else {
        adjancencymatrix[a,b] <-0
      }
    }

}

  adjancencymatrix

#=======section 1.2 build gene expression data adjancency matrix==========================================

  source("https://bioconductor.org/biocLite.R")
  biocLite("BiocUpgrade")


  if (!require(Biobase, quietly=TRUE)) {
    biocLite("Biobase")
    library(Biobase)
  }
  if (!require(GEOquery, quietly=TRUE)) {
    biocLite("GEOquery")
    library(GEOquery)
  }

  # install package outliers to detect any outliers of dataframe
  install.packages("outliers")
  library(outliers)
  # install package class for clustering purpose
  install.packages("class")
  library(class)

  load("./data/GSE3635.RData")

  gset <- getGEO("GSE3635", GSEMatrix =TRUE, getGPL=FALSE)

  head(gset)

  if (length(gset) > 1) {
    idx <- grep("GPL1914", attr(gset, "names"))
  } else {
    idx <- 1
  }

  gset <- gset[[idx]]

  # list all of genes;
  featureNames(gset)[1:nrow(gset)]

  # list all of 13 samples
  sampleNames(gset)[1:ncol(gset)]


  # assign all of features and columns into an object;
  AllofExpressionValuesObject <- gset[1:6228,1:13]
  AllofExpressionValuesObject

  # assign above object into a matrix using "exprs" function
  AllofExpressionMatrix <- matrix(exprs(AllofExpressionValuesObject), nrow=6228, ncol=13)
  AllofExpressionMatrix


  # verify whether the matrix has "na" value
  is.na(AllofExpressionMatrix)

  # omit the rows that has na value;
  NAomittedmatrix <- na.omit(AllofExpressionMatrix)

  #convert NAomittedmatrix object into a matrix

  AllofExpressionMatrix1 <- matrix((NAomittedmatrix), nrow(NAomittedmatrix), ncol(NAomittedmatrix))
  AllofExpressionMatrix1

  #lets transpose the matrix

  AllofExpressionMatrix2 <- t(AllofExpressionMatrix1)
  AllofExpressionMatrix2


  # detect the outlier of the dataset
  outlier(AllofExpressionMatrix2)

  # let's remove the outliers
  rm.outlier(AllofExpressionMatrix2)

  nrow1 <- nrow(AllofExpressionMatrix2)
  nrow1

  ncol1 <- ncol(AllofExpressionMatrix2)
  ncol1


  # since there are thousands of genes(recall, columns represent genes), Let's extract only 75 genes

  AllofExpressionMatrix3 <- matrix(AllofExpressionMatrix2, nrow=13,ncol=75)
  AllofExpressionMatrix3

  # create correlation matrix
  ExpressionCorrelationMatrix <- cor(AllofExpressionMatrix3)
  ExpressionCorrelationMatrix


  # Let's create adjancency matrix by choosing threhold value to 0.3.
  ExpressionAdjancencyMatrix <- ExpressionCorrelationMatrix

  for (c in 1:nrow(ExpressionCorrelationMatrix))
  {
    for (d in 1:ncol(ExpressionCorrelationMatrix))
    {
      if(ExpressionCorrelationMatrix[c,d] > 0.3)
      {
        ExpressionAdjancencyMatrix[c,d] <-1
      }

      else {
        ExpressionAdjancencyMatrix[c,d] <-0
      }
    }

  }

  ExpressionAdjancencyMatrix

#========== section 2.1: use "network" package to create network object and visualize the data==============================

# install network package
  install.packages("network")
  library("network")
# create a network object using above synthetic adjacency matrix
  network1 <- network(adjancencymatrix, directed=FALSE)

# plot the the network of these graph

  plot(network1,label=verticesname)


# Q2.1.1: Is this weighted graph or unweighted graph?

# Task 2.1.1: What vertices are connected? and which one all not?
# Task 2.1.2: Change threshold value to -1, 0.3, 0.5, 0.9, 1. Then commpare which vertices are connected?

# If we want to know how many edges does this network have, we can
# obtain total number of edges
  totaledges <- network.edgecount(network1)
  totaledges

# Over the time, we might want to know the links between each node has been
# increasing or decreasing, this can be done through network density
  networkdensity <- network.density(network1)
  networkdensity

# If we want to know which vertices are connected to each other,
# we can get edge list matrix. rows reprsents the number of edges and
# columns represent the vertices of edges

  edgelistmatrix <- as.matrix.network(network1,matrix.type = "edgelist")
  edgelistmatrix

# Q2.1.2: how edges does "V1" and "V2" have? Which vertices do they connect to?
# verify your result with graph, are they accurate?

# the strength that connects to each node can be different. Larger
# value means strong connectivity,  let's create values to each edge

  edgevaluelist <-round(runif(totaledges,min=1,max=4))
  edgevaluelist
# assign values to each edge and it becomes weighted graph
  set.edge.value(network1," edgevalue", edgevaluelist)
  plot(network1, label=verticesname,edge.label=edgevaluelist)

# Q2.1.3: Is this weighted graph or unweighted graph?

# we can also work backwards, let's say a network object is provided, we can
# obtain it's adjancymatrix
  networkbackwardmatrix <- as.matrix.network(network1, matrix.type="adjacency")
  networkbackward<-network(networkbackwardmatrix, directed = FALSE)
  plot(networkbackward,label=verticesname)

 # Q2.1.4: Are "network" and "networkbackward" graph same?

  # Task 2.1.3: What vertices are connected? and which one all not?
  # Task 2.1.4: Change threshold value to -1, 0.3, 0.5, 1. Then commpare which vertices are connected?

#======section 2.2 use network package to visualize gene expressioin data set===================
  network3 <- network(ExpressionAdjancencyMatrix, directed=FALSE)
  network3
  genename <- featureNames(gset)[1:75]
  genename

  plot(network3, label = genename)

  # obtain total number of edges in a network object
  totaledges <- network.edgecount(network3)
  totaledges

  # create values to each edge
  x <- c(1,4)
  edgevaluelist <-sample(x,totaledges,replace=T)
  edgevaluelist
  # assign values to each edge and it becomes weighted graph
  set.edge.value(network3," edgevalue", edgevaluelist)
  plot(network3, label=genename,edge.label=edgevaluelist)

  # find graph density and edge list matrix
  networkdensity3 <- network.density(network3)
  networkdensity3

  edgelistmatrix3 <- as.matrix.network(network3,matrix.type = "edgelist")
  edgelistmatrix3

  # This list is useful in biological perspective, it means these genes are co-expressed
  # at same time. Imagine one group of these genes are oncogenes, then we can target them
  # and apply relavant treatment.

  # Task 2.2.1: What genes are connected? and which one all not?
  # Task 2.2.2: Change threshold value to -1, 0.3, 0.5,0.9,1. Then commpare which genes are connected?


#=======section 3.1:use igraph package to create igraph object from synthetic data and visualize the data===================

  install.packages("igraph")
  library("igraph")

# let's make a simple igraph object from a edge list
  igraph1 <- make_graph(edges=c("V1", "V2",
                                    "V2", "V3",
                                    "V3","V4",
                                    "V4", "V5",
                                    "V5","V6",
                                    "V6","V7",
                                    "V7","V8",
                                    "V8","V9",
                                    "V9","V16",
                                    "V10","V11",
                                    "V11", "V12",
                                    "V12","V13",
                                    "V13","V14",
                                    "V14","V15",
                                    "V1","V16",
                                    "V16","V10"), n =16, directed =F)

  plot.igraph(igraph1)

#Q3.1.1: change "directed = T", and what does graph look like? How to change the direction of each edge?

# let's create an igrpah object from previous adjancency matrix.
  igraph2<-graph_from_adjacency_matrix( adjancencymatrix, mode="undirected", weighted = NULL)

  plot.igraph(igraph2, vertex.color="green")

# This graph has self loop since adjancy matrix refers node itself to 1,
# let's delete the self loop
  igraph3 <-simplify(igraph2)
  plot.igraph(simplify(igraph3), vertex.color="green")


# we can try different layout options to visualize the graph
  igraph_options(plot.layout=layout_on_sphere)
  plot.igraph(igraph3, vertex.color="green")
# Exercie3.1.1: change layout to "layout_as_tree", "layout.grid" and "layout_as_star", compare the difference


# If we are interested in finding out connections between vertices
# we can create edgelist matrix and total numebr of edges
  edgelistmatrixigraph <- as_edgelist(igraph3)
  edgelistmatrixigraph

# Q3.1.2: Are edgelist matrixs created by network and igraph pacakge same?

# find out connectivity between two vertices

   vertexconnectivity1 <- vertex_connectivity(igraph3, source=5,target=6,checks = TRUE)
   vertexconnectivity1

# Exercise: find other verices connectivity by changing source and target to differnt vertices ID.

# let's find the graph connectivity
   graphconnectivity1 <- vertex_connectivity(igraph3,checks = TRUE)
   graphconnectivity1

# let's find out either the graph is star-like or not by calculating the centrality
  centrality <- centralization.betweenness(igraph3, directed=FALSE, normalized = TRUE)
  centrality

# Q3.1.3: which is centrality score? Does it mean graph star-like shape?


#=====section 3.2 use igraph to visualize the gene expression dataset======================================
  igraph4<-graph_from_adjacency_matrix(ExpressionAdjancencyMatrix, mode="undirected", weighted = NULL)

  # delete self loop
  igraph5 <-simplify(igraph4)

  # plot graph with green color vertice
  plot.igraph(simplify(igraph5), vertex.color="green")

  # Access the current vertices and their name
  verticetotal <- V(igraph5)
  verticetotal

  # Access the name of vertices
  verticename <- V(igraph5)$name
  verticename

  # it returns NULL meaning it hasnot been assigned yet. let's assign into corresponding gene name

  V(igraph5)$name <- genename
  plot.igraph(simplify(igraph5), vertex.color="green")

  # let's find the centrality this graph
  centrality5 <- centralization.betweenness(igraph5, directed=FALSE, normalized = TRUE)
  centrality5

#  Q3.2.1: Is graph star-like shape base on centrality score?
# let's find out the edgelist matrix and total numebr of edges
    edgelistmatrixigraph <- as_edgelist(igraph5)
    edgelistmatrixigraph

#  Q3.2.2: which genes are co-expressed with gene YAL040C?


# dGOA2features.R
#
# Purpose:  transform GOA annotations to features
#
# Version:  0.1
# Date:     2017 10 12
# Author:   Boris Steipe (boris.steipe@utoronto.ca)
#
# Dependencies:
#           GOSim bioconductor package
# Notes:
#           Consider experimental - no validation performed so far.
#
# License: GPL-3 (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
# Version history:
#    0.1  First code
#
# ToDo:
#    <list ...>
#
# ==============================================================================


# Load the bioconductor package installer
source("https://bioconductor.org/biocLite.R")

if (!require(GOsim, quietly=TRUE)) {
  biocLite("GOSim")
  library(GOSim)
}

# GOSim loads human annotations by default. We load yeast annotations ...
if (!require(org.Sc.sgd.db, quietly=TRUE)) {
  biocLite("org.Sc.sgd.db")
  library(org.Sc.sgd.db)
}

# Choose GOterms to use
setEvidenceLevel(evidences="all",
                 organism=org.Sc.sgdORGANISM,
                 gomap=org.Sc.sgdGO)

# Use Biological Process ontology
setOntology("BP", loadIC=FALSE)

# confirm that we loaded the correct ontology
head(get("gomap", envir=GOSimEnv))

# test, getting one set of annotations
getGOInfo(c("YHR043C"))
# test, getting one semantic similarity
getGeneSim(c("YDL056W", "YDR182W"))
# .. looking good.


# load sample data
load(file = "./data/myGOExSet.RData")

# select 100 Prototype Genes for embedding the GO term space
myBPProto <- selectPrototypes(n = 100)

# Calculate semantic similarities ... this scales approximately linear
# with the number of genes and the number of prototypes. Our sample gene set
# of 280 genes and 100 prototypes takes about 8 minutes. A full annotation
# of 6000 yeast genes and 200 prototypes is expected to take
# (6000/280) * (200/100) * 8 ... about 6 hours. Not too bad.

# In my trial runs, the PCA that getGeneFeaturesPrototypes() performs fails, due
# to the presence of NA and Inf values. Therefore, this code switches PCA off,calculates similarities, then cleans the values and calculate PCA with prcomp().

t0 <- Sys.time()
simGOBP <- getGeneFeaturesPrototypes(genelist = myGOExSet$ID,
                                     prototypes = myBPProto,
                                     pca = FALSE,
                                     normalization = FALSE,
                                     verbose=FALSE)
tFull <- Sys.time() - t0  # 7.3 minutes

x <- simGOBP$features

# Are there Inf values?
sum(is.infinite(as.vector(x)))

# change all Inf to NA
for (iRow in 1:nrow(x)) {
  x[iRow, is.infinite(x[iRow, ])] <- NA
}

# Are there NA values?
sum(is.na(as.vector(x)))

# Impute NA values by replacing it with a random sample from the row
for (iRow in 1:nrow(x)) {
  iNAs <- which(is.na(x[iRow, ]))
  myImputed <- sample(x[iRow, -(iNAs)], length(iNAs))
  x[iRow, iNAs] <- myImputed
}

# Now compute PCA
pcaBP <- prcomp(x)

# sanity check: confirm that the rows have the right order
head(myGOExSet$ID)
head(rownames(pcaBP$x))

plot(pcaBP)
summary(pcaBP)
# First five components have > 95% of the variance

# Trying the same with the MF and CC ontology gave mostly NAs. Need to
# investigate more deeply how to work with the other Ontologies.

# ==============================================================================
# Assemble to features:

x <- pcaBP$x[ , 1:5]

# Add missing Genes to x
(missing <- which(!(myGOExSet$ID %in% rownames(x))))
tmp <- matrix(NA, nrow = length(missing), ncol = 5)
rownames(tmp) <- myGOExSet$ID[missing]
x <- rbind(x, tmp)
colnames(x) <- c("BP1", "BP2", "BP3", "BP4", "BP5")

myGeneFeatures <- cbind(myGOExSet, x[myGOExSet$ID, ])

str(myGeneFeatures)

# save(myGeneFeatures, file = "./data/myGeneFeatures.RData")
# load(file = "./data/myGeneFeatures.RData")


# [END]

# a30_a38_GSE4987.R
source("https://bioconductor.org/biocLite.R")
if (!require("GEOquery")) {
  biocLite("GEOquery")
  library(GEOquery)
}
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}

GSE4987 <- getGEO("GSE4987", GSEMatrix =TRUE, AnnotGPL=TRUE)
if (length(GSE4987) > 1) {
  idx <- grep("GPL1914", attr(GSE4987, "names"))
} else {
  idx <- 1
}
GSE4987 <- GSE4987[[idx]]

# There are 25 samples, each separated by 5 minutes
# Create a data.table of expression values
exprData = data.table(exprs(GSE4987)[-(1:11), ])


# Separate alpha30 and alpha38 datasets
a30 = cbind(featureNames(GSE4987)[-(1:11)], exprData[,1:25])
a38 = cbind(featureNames(GSE4987)[-(1:11)], exprData[,26:50])

# Save
fwrite(a30, "data/a30.txt", sep = "\t")
fwrite(a38, "data/a38.txt", sep = "\t")

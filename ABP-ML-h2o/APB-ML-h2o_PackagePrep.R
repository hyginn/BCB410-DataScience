# PackagePrep.R
source("https://bioconductor.org/biocLite.R")
# The following package is required for the use of yeast ENSEMBL data.
if (!require(ensembldb)) {
  biocLite("ensembldb")
  library(ensembldb)
}

if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("fastmatch")) {
  install.packages("fastmatch")
  library(fastmatch)
}
if (!require("h2o")) {
  install.packages("h2o")
  library(h2o)
}

# [END]

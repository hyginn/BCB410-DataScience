# GSE4987_prep.R

if (!file.exists("data/myGO.txt")) {
  source("GSE4987_GOslim.R")
}

if (!file.exists("data/a30.txt") || !file.exists("data/a38.txt")) {
  source("a30_a38_GSE4987.R")
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

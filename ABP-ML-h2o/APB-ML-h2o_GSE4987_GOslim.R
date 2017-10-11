# GSE4987_GOslim.R
# Prepare GSE data
# Uncomment the necessary lines to see how the data was querried

source("https://bioconductor.org/biocLite.R")
if (!require("GEOquery")) {
  biocLite("GEOquery")
  library(GEOquery)
}
if (!require("biomaRt")) {
  biocLite("biomaRt")
  library(biomaRt)
}
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("fastmatch")) {
  install.packages("fastmatch")
  library(fastmatch)
}

GSE4987 <- getGEO("GSE4987", GSEMatrix =TRUE, AnnotGPL=TRUE)
if (length(GSE4987) > 1) {
  idx <- grep("GPL1914", attr(GSE4987, "names"))
} else {
  idx <- 1
}
GSE4987 <- GSE4987[[idx]]

## Create name
myYeast = GSE4987

# Create a data.table of expression values
exprData = data.table(exprs(myYeast)[-(1:11), ])

# Retrieve GO annotations from biomaRt:
# List available marts
listMarts()
# Select appropriate mart
myMart = useMart(biomart = "ENSEMBL_MART_ENSEMBL")
# List available datasets' descriptions (organism names)
# orgs = listDatasets(myMart)[,"description"]

# grep(x = orgs, pattern = ".*cere.*", value = T)
# grep(x = orgs, pattern = ".*cere.*")

# Find dataset 61
# listDatasets(myMart)[61,]
# Copy the name scerevisiae_gene_ensembl

# Update mart
myMart = useMart(biomart = "ENSEMBL_MART_ENSEMBL",
                 dataset = "scerevisiae_gene_ensembl")

# Annotate yeast genes with GO ID:
# Find the attributes that can be qurried, look for yeast ID and GO terms
# allAtts = listAttributes(myMart)
# atts = allAtts[,"description"]
# grep(atts, pattern = ".*ID.*", value = T)
# Gene stable ID = No. 1
# listAttributes(myMart)[1,]
# grep(atts, pattern = ".*evidence.*", value = T)
# grep(atts, pattern = ".*evidence.*")
#
# grep(atts, pattern = ".*GO.*", value = T)
# grep(atts, pattern = ".*GO.*")
# GO term accession No. 27 AND GOSlim GOA Accession(s) No. 32
# allAtts[c(27, 30, 32, 33),]

# Find filters
# filts = listFilters(myMart)[, "description"]
# grep(filts, pattern = ".*ID.*", value = T)

myGOslim = getBM(mart = myMart, attributes = c("ensembl_gene_id",
                                               "goslim_goa_accession",
                                               "go_linkage_type"),
                 filters = "ensembl_gene_id",
                 values = featureNames(myYeast)[-(1:11)], verbose = T)

# Narrow down by experimental evidence only
# The codes are: IDA, IEP, IGI, IMP, IPI, Plus curator inference: IC, TAS
expEvi = c("IDA", "IEP", "IGI", "IMP", "IPI")
# This method is quite fast
idx = fmatch(myGOslim$go_linkage_type, expEvi, nomatch = 0)
idx = ifelse(test = idx != 0, yes = T, no = F)

myGO = myGOslim[idx,]
# typeof(myGO)

# Convert to data.table, discard evidence codes
myGO = unique(data.table(myGO[,1:2]))

# GOslim label count
# length(unique(myGO$goslim_goa_accession))

# Save the GOslim data
fwrite(myGO, "data/myGO.txt", sep = "\t")

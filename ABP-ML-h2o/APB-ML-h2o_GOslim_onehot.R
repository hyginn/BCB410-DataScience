# GOslim_onehot.R

library(data.table)
library(fastmatch)
library(stringr)

myGO = fread("data/myGO.txt")
a30 = fread("data/a30.txt")

# Create a "wide" format table, with logical "one-hot encoding" of GOslim terms per Gene ID
# Get one-hot encoding using function(x) 1L, fill = 0

wideGO = dcast(data = myGO, formula = ensembl_gene_id ~ goslim_goa_accession,
               fun.aggregate = function(x) 1L, fill = 0)
# Validate:
length(unique(myGO$ensembl_gene_id))
length(unique(myGO$goslim_goa_accession))
dim(wideGO)

# Remove "GO:" from GO accession IDs
test = str_split(string = colnames(wideGO),
                 pattern = ":", n = 2, simplify = T)[,2]
# Make valid names for compatibility with mlr
colnames(wideGO)[-1] = make.names(test[-1])

# Create one-hot label vector from GOslim accession IDs
labels = colnames(wideGO)[-1]

# Separate unannotated genes from GSE4987 and
idx = fmatch(a30$myGenes, wideGO$ensembl_gene_id, nomatch = 0)
# Create training set using the annotated genes
train = a30[idx]
unannot = a30[-idx]

# Change column names to allow for merge
colnames(train)[1] = "GeneID"
colnames(wideGO)[1] = "GeneID"
# Merge wideGO encoding to training set
allData = merge(train, wideGO, by = "GeneID")

# Save
fwrite(allData, "data/GSE4987_GOslim_onehot.txt", sep = "\t")

# OtherTargets.R
# Use time-series yeast gene expression data to classify other features of yeast
# genes (addition to GO terms)

source("PackagePrep.R")

# ==== Retrieve ENSEMBL 88 data for S. Cerevisiae ====
library(AnnotationHub)
AH = AnnotationHub()
grep(unique(AH$species), pattern = ".*cerevis.*", value = T)
grep(unique(AH$species), pattern = ".*cerevis.*")
AH[AH$species == "Saccharomyces Cerevisiae"]

Annot = AH[["AH53745"]]

?`select,EnsDb-method`

# See what data is available
columns(Annot)

# e.g. for domains, the start and end positions and the domain ID can be
# retrieved, in addition to the protein sequence. Selecting the right information
# for training an ML model should be done in accordance to proper feature
# selection approaches.

# ==== Annotate GOExSet data ====
temp = load(file = "./data/myGOExSet.RData")
smallSet = get(temp)
rm(temp)

# Set GeenIDs as keys
myKeys = smallSet$ID

# Retrieve associated protein sequences, exon sequences
# What features are important? (answer this or choose deep learning :) )
# select(x = Annot, keys = head(myKeys),
#        columns = c("GENEBIOTYPE", "TXBIOTYPE", "ISCIRCULAR", "SEQLENGTH",
#                    "SEQSTRAND"),
#        keytype = "GENEID")
columns(Annot)
# Retrieve protein annotation data
select(x = Annot, keys = head(myKeys),
       columns = c("PROTDOMSTART", "PROTDOMEND", "PROTEINDOMAINID"),
       keytype = "GENEID")


# Must create a wide format before training
# Can also add the number of domains or their types instead
# (is domain start and end that important? maybe... how can we use ML to tell?)

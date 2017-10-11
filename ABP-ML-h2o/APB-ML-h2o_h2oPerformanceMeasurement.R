# h2oPerformanceMeasurement.R

# Load required packages
source("PackagePrep.R")
# h2o.init(max_mem_size = "2")

?h2o.performance

# Load any of the already trained models
deepResults = readRDS("data/deepResults.rds")

# We can visualize the performance of the model during testing/validation
h2o.performance(deepResults[[1]])

# We can also measure the performance of the model with new data

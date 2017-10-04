# .init.R
# Functions to initialize this session for exercises
# Boris Steipe
# ====================================================================

# Create a local copy of myScript.R if required.
if (! file.exists("myScript.R") && file.exists(".tmp.R")) {
    file.copy(".tmp.R", "myScript.R")
}

source(".utilities.R")

file.edit("BCB410-DataScience.R")

# [End]

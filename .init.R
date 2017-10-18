# .init.R
# Functions to initialize this session for exercises
# Boris Steipe
# ====================================================================

# Create a local copy of myScript.R if required.
if (! file.exists("myScript.R") && file.exists(".tmp.R")) {
    file.copy(".tmp.R", "myScript.R")
}

# source() all utility scripts
utilityScripts <- list.files("./utilities",
                             pattern = ".R$",
                             full.names = TRUE)
for (i in seq_along(utilityScripts)) {
  source(utilityScripts[i])
}

file.edit("BCB410-DataScience.R")

# [End]

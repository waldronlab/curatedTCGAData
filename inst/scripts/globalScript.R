## Script to disassemble and upload MultiAssayExperiment pieces
## Run script from project folder (curatedTCGAdata)
library(curatedTCGAData)
library(MultiAssayExperiment)
library(AnnotationHubData)
library(magrittr)

repoDir <- normalizePath(Sys.getenv("REPO"))
dataDir <- file.path(repoDir, "data")

if (!dir.exists(dataDir))
    dir.create(dataDir)

# Disassemble
source("make-data.R")

# Document
source("make-documentation.R")

# Upload
source("make-upload.R")

# Create metadata.csv
source("make-metadata.R")

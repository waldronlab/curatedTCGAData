## Script to disassemble and upload MultiAssayExperiment pieces
## Run script from project folder (curatedTCGAdata)
library(curatedTCGAData)
library(MultiAssayExperiment)
library(AnnotationHubData)
library(BiocParallel)
library(magrittr)

repoDir <- normalizePath(Sys.getenv("REPO"))
dataDir <- file.path(repoDir, "data")

setwd(repoDir)

if (!dir.exists(dataDir))
    dir.create(dataDir)

## Find the RDS files in MultiAssayExperiment-TCGA
rdaFiles <-
    list.files(file.path(repoDir,
                         "../MultiAssayExperiment-TCGA/data/bits/"),
               full.names = TRUE, recursive = TRUE, pattern = "*\\.rda$")

# Disassemble
# source("inst/scripts/make-data.R")

# Document
source("inst/scripts/make-documentation.R")

# Upload
source("inst/scripts/make-upload.R")

# Create metadata.csv
source("inst/scripts/make-metadata.R")


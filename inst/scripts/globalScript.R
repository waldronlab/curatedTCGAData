## Script to disassemble and upload MultiAssayExperiment pieces
## Run script from project folder (curatedTCGAdata)
library(curatedTCGAData)
library(MultiAssayExperiment)
library(AnnotationHubData)
library(BiocParallel)
library(magrittr)

repoDir <- normalizePath(Sys.getenv("REPO"))
manDir <- file.path("man")

setwd(repoDir)

## Get all compatible TCGA disease codes
source("inst/scripts/getDiseaseCodes.R")
## Source the converter function (MultiAssayExperiment RDS to Rd)
source("inst/scripts/bits2rd.R")
# Load document generation function
source("inst/scripts/make-documentation.R")
## Load helper function for collecting metadata
source("inst/scripts/getMetadata.R")
# Load metadata function
source("inst/scripts/make-metadata.R")

TCGAcodes <- getDiseaseCodes()

## Folder containing cancer folders
dataBitsLocation <- file.path(repoDir,
                              "../MultiAssayExperiment-TCGA/data/bits/")

## Document by cancer folder
cancerFolders <- file.path(dataBitsLocation, TCGAcodes)

## create metadata.csv in inst/extdata folder
make_metadata(dataBitsLocation)

lapply(cancerFolders, make_documentation, manDir)


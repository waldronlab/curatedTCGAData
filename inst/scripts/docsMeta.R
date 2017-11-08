## Script to generate metadata and documentation
## Run script from project folder (curatedTCGAdata)
suppressPackageStartupMessages({
    library(curatedTCGAData)
    library(RaggedExperiment)
    library(MultiAssayExperiment)
    library(BiocParallel)
    library(magrittr)
})

repoDir <- normalizePath(Sys.getenv("REPO"))
manDir <- file.path("man")

setwd(repoDir)

## Get all compatible TCGA disease codes
load("R/sysdata.rda")

## Source the converter function (MultiAssayExperiment RDS to Rd)
source("inst/scripts/bits2rd.R")
# Load document generation function
source("inst/scripts/make-documentation.R")
## Load helper function for collecting metadata
source("inst/scripts/getMetadata.R")
# Load metadata function
source("inst/scripts/make-metadata.R")

## Generate documents
## Get codes from loaded function
TCGAcodes <-
    diseaseCodes[["Study.Abbreviation"]][diseaseCodes[["Available"]] == "Yes"]

## Folder containing cancer folders
dataBitsLocation <- file.path(repoDir,
    "../MultiAssayExperiment-TCGA/data/bits")

## Document by cancer folder
cancerFolders <- file.path(dataBitsLocation, TCGAcodes)

## create metadata.csv in inst/extdata folder
# message("Generating metadata...")
# make_metadata(dataBitsLocation)

message("Creating documentation pages")
lapply(cancerFolders, make_documentation, manDir)


## Script to disassemble and upload MultiAssayExperiment pieces
## Run script from project folder (curatedTCGAdata)
suppressPackageStartupMessages({
    library(curatedTCGAData)
    library(RaggedExperiment)
    library(MultiAssayExperiment)
    library(AnnotationHubData)
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


## Script to generate metadata and documentation
## Run script from project folder (curatedTCGAdata)
suppressPackageStartupMessages({
    library(curatedTCGAData)
    library(RaggedExperiment)
    library(MultiAssayExperiment)
    library(BiocParallel)
    library(magrittr)
})

if (identical(Sys.getenv("REPO"), ""))
    Sys.setenv(REPO = "~/github/curatedTCGAData")

repoDir <- normalizePath(Sys.getenv("REPO"))
dataDir <- "data/bits"

setwd(repoDir)

## Get all compatible TCGA disease codes
load("R/sysdata.rda")

## Load helpers to environment
source("inst/scripts/tools.R")
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
cancerPath <- file.path(repoDir, "../MultiAssayExperiment-TCGA/", dataDir)

## Document by cancer folder
cancerFolders <- file.path(cancerPath, TCGAcodes)

## create metadata.csv in inst/extdata folder
message("Generating metadata...")
make_metadata()

message("Creating documentation pages")
## set width for `cat`
options(width = 78)

lapply(cancerFolders, make_documentation)


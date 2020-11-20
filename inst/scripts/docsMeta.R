## Script to generate metadata and documentation
## Run script from project folder (curatedTCGAdata)
suppressPackageStartupMessages({
    library(BiocParallel)
    library(S4Vectors)
    library(RaggedExperiment)
    library(TCGAutils)
    library(MultiAssayExperiment)
    library(curatedTCGAData)
})

if (identical(Sys.getenv("REPO"), ""))
    Sys.setenv(REPO = "~/gh/curatedTCGAData")

repoDir <- normalizePath(Sys.getenv("REPO"))

setwd(repoDir)

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
data(diseaseCodes, package = "TCGAutils")
TCGAcodes <- with(diseaseCodes, Study.Abbreviation[Available == "Yes"])

## Folder containing cancer folders
dataDir <- file.path(repoDir, "../MultiAssayExperiment.TCGA/data/bits/")

## add Tags to older data
if (FALSE) {
    meta <- readr::read_csv("inst/extdata/metadata.csv")
    meta$Tags <- vapply(
        strsplit(meta$ResourceName, "_|-"), `[[`, character(1L), 2L
    )
    readr::write_csv(meta, file = "inst/extdata/metadata.csv")
}

## create metadata.csv in inst/extdata folder
message("Generating main 'metadata.csv' file...")
make_metadata(
    directory = "~/gh/MultiAssayExperiment.TCGA/", dataDir = "data/bits",
    version = "2.0.0", ext_pattern = allextpat,
    resource_maintainer = utils::maintainer("curatedTCGAData"),
    resource_biocVersion = BiocManager::version(),
    append = TRUE
)

## check metadata.csv
setwd("..")
AnnotationHubData::makeAnnotationHubMetadata("curatedTCGAData", "metadata.csv")
setwd(repoDir)

message("Creating documentation pages")
## set width for `cat`
options(width = 78)

registered()
params <- MulticoreParam(
    workers = 33, stop.on.error = FALSE, progressbar = TRUE
)

BiocParallel::bplapply(TCGAcodes, function(ccode) {
    make_documentation(
        dataDir = "~/gh/MultiAssayExperiment.TCGA/data/bits",
        cancer = ccode,
        version = "2.0.0",
        manDirectory = "man"
    )
}, BPPARAM = params)

lapply(TCGAcodes, .addSeeAlso, version = "2.0.0")

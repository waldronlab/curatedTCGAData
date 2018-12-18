.getDataFiles <-
function(directory = "~/github/MultiAssayExperiment-TCGA/",
    dataDir = "data/bits", cancerFolder, pattern = allextpat) {
    location <- file.path(directory, dataDirs, cancerFolder)
    list.files(location, pattern = pattern, full.names = TRUE, recursive = TRUE)
}

.get_Description <- function(data_name, cancer) {
    paste(data_name, "data specific to the", toupper(cancer),
           "cohort of the TCGA project")
}

getMetadata <- function(resource_location, resource_maintainer,
                         resource_biocVersion, verbose = TRUE) {
    stopifnot(is.character(resource_maintainer) &&
              is.character(resource_location))
    if (verbose)
        message("Working on: ", resource_location)
    ResourceName <- basename(resource_location)
    Title <- gsub(".rda", "", ignore.case = TRUE, ResourceName)
    Description <- .get_Description(ResourceName)
    BiocVersion <- as.character(resource_biocVersion)
    Genome <- ""
    SourceType <- "TXT"
    SourceUrl <- "http://gdac.broadinstitute.org/"
    SourceVersion <- "1.1.38"
    Species <- "Homo Sapiens"
    TaxonomyId <- "9606"
    Coordinate_1_based <- as.logical(NA)
    DataProvider <- "Eli and Edythe L. Broad Institute of Harvard and MIT"
    Maintainer <- resource_maintainer
    RDataPath <- file.path("curatedTCGAData", ResourceName)
    RDataClass <- .get_ResourceClass(resource_location)
    DispatchClass <- .get_DispatchClass(ResourceName)
    data.frame(Title, Description, BiocVersion, Genome, SourceType, SourceUrl,
               SourceVersion, Species, TaxonomyId, Coordinate_1_based,
               DataProvider, Maintainer, RDataClass, DispatchClass,
               ResourceName, RDataPath, stringsAsFactors = FALSE)
}


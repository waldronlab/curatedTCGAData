.get_ResourceClass <- function(resource_location) {
    load(resource_location)
    objName <- gsub(".rda", "", ignore.case = TRUE,
        basename(resource_location))
    as.character(class(get(objName)))
}

.get_Description <- function(resource_name) {
    dataName <- unlist(strsplit(gsub(".rda", "", ignore.case = TRUE,
        resource_name), "_"))
    paste(dataName[2], "data specific to the", toupper(dataName[1]),
           "cohort of the TCGA project")
}

.get_DispatchClass <- function(resource_name) {
    if (grepl(".rda", resource_name)) {
        return("Rda")
    }
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
    SourceVersion <- "1.1.36"
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


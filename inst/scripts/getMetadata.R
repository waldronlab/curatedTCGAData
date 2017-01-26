.get_ResourceClass <- function(resource_name) {
    resource_path <- paste0("./data/", resource_name)
    load(resource_path)
    objName <- gsub(".rda", "", resource_name)
    as.character(class(get(objName)))
}

.get_Description <- function(resource_name) {
    gsub(".rda", "", resource_name) %>%
    strsplit(., "_") %>%
    unlist() %>%
    {paste(.[2], "data specific to the", toupper(.[1]),
           "cohort of the TCGA project")}
}

.get_DispatchClass <- function(resource_name) {
    if (grepl(".rda", resource_name)) {
        return("Rda")
    }
}

getMetadata <- function(resource_name, resource_maintainer,
                         resource_biocVersion) {
    stopifnot(is.character(resource_maintainer) &&
              is.character(resource_name))
    Title <- gsub(".rda", "", resource_name)
    Description <- .get_Description(resource_name)
    BiocVersion <- as.character(resource_biocVersion)
    Genome <- ""
    SourceType <- "TXT"
    SourceUrl <- "http://gdac.broadinstitute.org/"
    SourceVersion <- "1.1.33"
    Species <- "Homo Sapiens"
    TaxonomyId <- "9606"
    Coordinate_1_based <- as.logical(NA)
    DataProvider <- "Eli and Edythe L. Broad Institute of Harvard and MIT"
    Maintainer <- resource_maintainer
    RDataClass <- .get_ResourceClass(resource_name)
    DispatchClass <- .get_DispatchClass(resource_name)
    ResourceName <- resource_name
    dplyr::data_frame(Title, Description, BiocVersion, Genome, SourceType, SourceUrl,
               SourceVersion, Species, TaxonomyId, Coordinate_1_based,
               DataProvider, Maintainer, RDataClass, DispatchClass,
               ResourceName)
}


.get_resource <- function(resource_name) {
    resource_path <- paste0("./data/", resource_name)
    load(resource_path)
    gsub(".rda", "", resource_name) %>%
    get()
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

get_metadata <- function(resource_name, resource_maintainer,
                         resource_biocVersion) {
    resource_object <- .get_resource(resource_name)
    Title <- gsub(".rda", "", resource_name)
    Description <- .get_Description(resource_name)
    BiocVersion <- as.character(resource_biocVersion)
    Genome <- as.character("")
    SourceType <- as.character("TXT")
    SourceUrl <- as.character("http://gdac.broadinstitute.org/")
    SourceVersion <- as.character("1.1.33")
    Species <- as.character("Homo Sapiens")
    TaxonomyId <- as.character("9606")
    Coordinate_1_based <- as.logical(NA)
    DataProvider <- as.character("Eli and Edythe L. Broad Institute of Harvard and MIT")
    Maintainer <- as.character(resource_maintainer)
    RDataClass <- class(resource_object)
    DispatchClass <- .get_DispatchClass(resource_name)
    ResourceName <- as.character(resource_name)
    data_frame(Title, Description, BiocVersion, Genome, SourceType, SourceUrl,
               SourceVersion, Species, TaxonomyId, Coordinate_1_based,
               DataProvider, Maintainer, RDataClass, DispatchClass,
               ResourceName)
}

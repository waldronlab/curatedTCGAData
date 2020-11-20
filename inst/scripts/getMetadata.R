.getDataFiles <-
    function(directory, dataDir, version, cancerFolder, pattern = allextpat)
{
    stopifnot(!missing(version))
    location <-
        file.path(directory, dataDir, paste0("v", version), cancerFolder)
    list.files(location, pattern = pattern, full.names = TRUE, recursive = TRUE)
}

.get_Description <- function(data_name, cancer) {
    paste(data_name, "data specific to the", toupper(cancer),
           "cohort of the TCGA project")
}

.getRDataClass <- function(dataList) {
    unlist(
    lapply(names(dataList), function(dataName) {
        if (grepl("Methyl", dataName))
            rep(class(dataList[[dataName]]), 2L)
        else
            class(dataList[[dataName]])
    })
    )
}

.get_DispatchClass <- function(resource_files) {
    ext_map <- data.frame(
    ext_pattern = c("\\.[Rr][Dd][Aa]$", "\\.[Rr][Dd][Ss]$", "\\.[Hh]5$"),
    Dispatch = c("Rda", "Rds", "H5File"),
    stringsAsFactors = FALSE
    )
    hitMatrix <- vapply(ext_map[["ext_pattern"]],
        function(pat) grepl(pat, resource_files),
        logical(length(resource_files)))
    ext_map[["Dispatch"]][apply(hitMatrix, 1L, which)]
}


getMetadata <-
    function(
        directory, dataDir, version, ext_pattern,
        resource_maintainer, resource_biocVersion
    )
{
    stopifnot(S4Vectors::isSingleString(directory),
        S4Vectors::isSingleString(dataDir))

    location <- file.path(directory, dataDir, paste0("v", version))
    cancerFolders <- dir(location)

    metasets <- lapply(cancerFolders, function(cancer) {
        message("Working on: ", cancer)
        datafilepaths <- .getDataFiles(
            directory = directory, dataDir = dataDir, version = version,
            cancerFolder = cancer, pattern = ext_pattern
        )
        dfmeta <- .makeMetaDF(datafilepaths, TRUE)
        dataList <- .loadRDAList(dfmeta)
        dataList <- .addMethylation(dfmeta, dataList)
        replen <- length(datafilepaths)

        ResourceName <- basename(datafilepaths)
        Title <- gsub(ext_pattern, "", ResourceName)
        Description <- .get_Description(Title, cancer)
        BiocVersion <- rep(as.character(resource_biocVersion), replen)
        Genome <- rep("", replen)
        SourceType <- rep("TXT", replen)
        SourceUrl <- rep("http://gdac.broadinstitute.org/", replen)
        SourceVersion <- rep(version, replen)
        Species <- rep("Homo sapiens", replen)
        TaxonomyId <- rep("9606", replen)
        Coordinate_1_based <- rep(as.logical(NA), replen)
        DataProvider <-
            rep("Eli and Edythe L. Broad Institute of Harvard and MIT", replen)
        Maintainer <- rep(resource_maintainer, replen)
        RDataPath <- file.path("curatedTCGAData", ResourceName)
        RDataClass <- .getRDataClass(dataList)
        DispatchClass <- .get_DispatchClass(ResourceName)
        data.frame(Title, Description, BiocVersion, Genome, SourceType, SourceUrl,
                   SourceVersion, Species, TaxonomyId, Coordinate_1_based,
                   DataProvider, Maintainer, RDataClass, DispatchClass,
                   ResourceName, RDataPath, stringsAsFactors = FALSE)
    })
    do.call(rbind, metasets)
}


allextpat <- "\\.[RrHh][Dd5][AaSs]?$"

.loadEnvObj <- function(filepath, name) {
    OBJENV <- new.env(parent = emptyenv())
    load(filepath, envir = OBJENV)
    object <- OBJENV[[name]]
    object
}

.loadData <- function(dataname, package) {
    local_dat <- new.env(parent = emptyenv())
    data(list = dataname, package = package, envir = local_dat)
    local_dat[[dataname]]
}

.loadMethyl <- function(methyl_folder) {
    HDF5Array::loadHDF5SummarizedExperiment(methyl_folder,
        prefix = paste0(basename(methyl_folder), "_"))
}

.loadRDAList <- function(metadata_frame) {
    objnames <- .selectInRow(metadata_frame,
        metadata_frame[["experimentFiles"]], "objectNames")

    rdafiles <- .selectInRow(metadata_frame,
        metadata_frame[["experimentFiles"]], "files")

    dataList <- lapply(rdafiles, function(dpath) {
        oname <- .selectInRow(metadata_frame, dpath, "objectNames", "files")
        .loadEnvObj(dpath, oname)
    })
    names(dataList) <- objnames
    dataList
}

.addMethylation <- function(metadata_frame, data_list) {
    methylFolders <- .selectInRow(metadata_frame, "Methylation", "objectNames",
        "dataTypes")
    if (length(methylFolders)) {
        for (folder in methylFolders) {
            methFiles <- .selectInRow(metadata_frame, folder, "files",
                "objectNames")
            pathfold <- unique(dirname(methFiles))
            object <- .loadMethyl(pathfold)
            data_list[[folder]] <- object
        }
    }
    data_list
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

.get_DispatchClass(resource_file) {
    ext_map <- data.frame(
    ext_pattern = c("\\.[Rr][Dd][Aa]$", "\\.[Rr][Dd][Ss]$", "\\.[Hh]5$"),
    Dispatch = c("Rda", "Rds", "H5File")
    )
    ext_map[["Dispatch"]][
        vapply(ext_map[["ext_pattern"]],
            function(pat) grepl(pat, resource_file),
            logical(3L))
    ]
}


.metadataFrame <-
function(directory = "~/github/MultiAssayExperiment-TCGA/",
    dataDir = "data/bits", cancerFolder, ext_pattern = allextpat,
    resource_maintainer = read.dcf("DESCRIPTION", "Maintainer")[[1]],
    resource_biocVersion = BiocManager::version()) {

    lapply(cancerFolder, function(cancer) {
        datafilepaths <- .getDataFiles(directory = directory, dataDir = dataDir,
            cancerFolder = cancer, pattern = ext_pattern)
        dfmeta <- .makeMetaDF(datafilepaths)
            # RDataClass = class(object)
        dataList <- .loadRDAList(dfmeta)
        dataList <- .addMethylation(dfmeta, dataList)

        ResourceName <- basename(datafilepaths)
        Title <- gsub(ext_pattern, "", ResourceName)
        Description <- .get_Description(ResourceName, cancer)
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
        RDataClass <- .getRDataClass(dataList)
        DispatchClass <- .get_DispatchClass(ResourceName)
        data.frame(Title, Description, BiocVersion, Genome, SourceType, SourceUrl,
                   SourceVersion, Species, TaxonomyId, Coordinate_1_based,
                   DataProvider, Maintainer, RDataClass, DispatchClass,
                   ResourceName, RDataPath, stringsAsFactors = FALSE)
    })
}

.makeMetaDF <- function(filepaths) {
    namespat <- "^[A-Z]*_(.*)"

    methLogic <- grepl("Methyl", filepaths)
    basefiles <- gsub(allextpat, "", basename(filepaths))

    if (any(methLogic)) {
        fpaths <- filepaths[!methLogic]
        fpaths <- unname(as(fpaths, "List"))

        basefiles <- basefiles[!methLogic]

        methylpaths <- filepaths[methLogic]
        methylbase <- unique(basename(dirname(methylpaths)))
        methfiles <- unname(splitAsList(methylpaths,
            basename(dirname(methylpaths))))

        filepaths <- c(fpaths, methfiles)
        basefiles <- c(basefiles, methylbase)
    }

    obj_slots <- c("metadata", "colData", "sampleMap")

    dfr <- DataFrame(files = as(filepaths, "List"),
        objectNames = basefiles,
        dataNames = gsub(namespat, "\\1", basefiles),
        dataTypes = vapply(
            strsplit(basefiles, "[_-]"), `[[`, character(1L), 2L)
   )
    dfr[["experimentFiles"]] <-
        !dfr[["dataTypes"]] %in% c(obj_slots, "Methylation")

    dfr
}

.selectInRow <- function(dataframe, term, outcol, colname = NULL) {
    if (!is.null(colname))
        unlist(dataframe[unlist(dataframe[[colname]] == term), outcol])
    else
        unlist(dataframe[term, outcol])
}

.cleanText <- function(x) {
    gsub("%", "\\%", iconv(x, "latin1", "ASCII", sub = "?"), fixed = TRUE)
}

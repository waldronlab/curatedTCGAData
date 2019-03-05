allextpat <- "\\.[RrHh][Dd5][AaSs]?$"

.loadEnvObj <- function(filepath, name) {
    OBJENV <- new.env(parent = emptyenv())
    load(filepath, envir = OBJENV)
    object <- OBJENV[[name]]
    object
}

.loadPkgData <- function(dataname, package) {
    local_dat <- new.env(parent = emptyenv())
    data(list = dataname, package = package, envir = local_dat)
    local_dat[[dataname]]
}

.localMethyl <- function(methyl_folder) {
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
            object <- .localMethyl(pathfold)
            data_list[[folder]] <- object
        }
    }
    data_list
}

.makeMetaDF <- function(filepaths, includeSlots = FALSE) {
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

    obj_slots <-
        if (!includeSlots)
            c("metadata", "colData", "sampleMap")
        else
            NULL

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

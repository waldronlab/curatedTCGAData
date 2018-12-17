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

.metaList <- function(object) {
    dims <- dim(object)
    list(size = format(object.size(object), units = "Mb"),
        rows = dims[[1L]], columns = dims[[2L]],
        colnames = colnames(object),
        rownames = rownames(object),
        class = class(object),
        length = length(object))
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

    DataFrame(files = as(filepaths, "List"),
        objectNames = basefiles,
        dataNames = gsub(namespat, "\\1", basefiles),
        dataTypes = vapply(
            strsplit(basefiles, "[_-]"), `[[`, character(1L), 2L)
    )
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

allextpat <- "\\.[RrHh][Dd5][AaSs]?$"

#' Write an Rd man page for a collection of MultiAssayExperiment bits
#'
#' @param cancerFolder Usually saved in 'MultiAssayExperiment-TCGA/data/bits/'
#' and contains several 'rda' files. The folder name denotes the cancer code
#' @param filename Full path of the filename of the .Rd man page to write
#' @param aliases A list of aliases
#' @param descriptions A list of extra lines to be written to the Description
#'
#' @author Levi Waldron, Marcel Ramos
#' @examples
#' rdaFolder <- file.path("../MultiAssayExperiment-TCGA/data/bits/COAD")
#' bit2rd(rdaFolder)
#'
#' @keywords internal
bits2rd <- function(cancerFolder, filename, aliases = cancerFolder,
        descriptions = "A document describing the TCGA cancer code") {
    stopifnot(S4Vectors::isSingleString(cancerFolder))
    stopifnot(S4Vectors::isSingleString(filename))

    aliases <- paste(aliases, sep = ", ")
    dataDirs <- "data/bits"

    fileNames <- list.files(file.path("../MultiAssayExperiment-TCGA",
        dataDirs, cancerFolder), full.names = TRUE,
        pattern = allextpat, recursive = TRUE)

    datadata <- .makeMetaDF(fileNames)

    slots <- c("metadata", "colData", "sampleMap")
    datadata[["experimentFiles"]] <-
        !datadata[["dataTypes"]] %in% c(slots, "Methylation")

    coldatfile <- unlist(datadata[datadata[["dataTypes"]] == "colData", "files"])
    colDataName <- .selectInRow(datadata, "colData", "objectNames", "dataTypes")
    colDat <- .loadEnvObj(coldatfile, colDataName)
    clinicalNames <- .loadData("clinicalNames", "TCGAutils")
    stdNames <- clinicalNames[[cancerFolder]]
    stdNames <- names(colDat) %in% stdNames
    numExtraCols <- sum(!stdNames)
    stdColDat <- colDat[, stdNames]

    objnames <- .selectInRow(datadata, datadata[["experimentFiles"]],
        "objectNames")

    rdafiles <- .selectInRow(datadata, datadata[["experimentFiles"]], "files")
    dataInfo <- lapply(rdafiles, function(dpath) {
        oname <- .selectInRow(datadata, dpath, "objectNames", "files")
        object <- .loadEnvObj(dpath, oname)
        .metaList(object)
    })
    names(dataInfo) <- objnames

    dataList <- lapply(rdafiles, function(dpath) {
        oname <- .selectInRow(datadata, dpath, "objectNames", "files")
        .loadEnvObj(dpath, oname)
    })
    names(dataList) <- objnames

    methylFolders <- .selectInRow(datadata, "Methylation", "objectNames",
        "dataTypes")

    for (folder in methylFolders) {
        methFiles <- .selectInRow(datadata, folder, "files", "objectNames")
        pathfold <- unique(dirname(methFiles))
        object <- .loadMethyl(pathfold)
        dataInfo[[folder]] <- .metaList(object)
        dataList[[folder]] <- object
    }

    objSizes <- vapply(dataInfo,
        function(datType) { datType$size }, character(1L))

    stopifnot(identical(names(dataList), names(objSizes)))
    objSizesdf <- data.frame(assay = names(dataList), size.Mb = objSizes,
        row.names = NULL)

    studyIdx <- which(diseaseCodes[["Study.Abbreviation"]] %in% cancerFolder)
    studyName <- diseaseCodes[["Study.Name"]][studyIdx]

    expList <- ExperimentList(dataList)
    sink(file = filename)
    cat(paste("\\name{", cancerFolder, "}"))
    cat("\n")
    cat(paste("\\alias{", aliases, "}"))
    cat("\n")
    cat(paste("\\docType{data}"))
    cat("\n")
    cat(paste("\\title{", .cleanText(studyName), "}"))
    cat("\n")
    if (!is.null(descriptions)) {
        cat("\\description{")
        cat("\n")
        for (i in seq_along(descriptions)) {
            cat(descriptions[[i]])
            cat("\n")
        }
        cat("}")
        cat("\n")
    }
    cat("\n")
    cat("\\details{")
    cat("\n")
    cat("\\preformatted{\n")
    cat(paste("> experiments(", cancerFolder, ")"))
    cat("\n")
    show(expList)
    cat("\n")
    cat(paste("> rownames(", cancerFolder, ")"))
    cat("\n")
    show(rownames(expList))
    cat("\n")
    cat(paste("> colnames(", cancerFolder, ")"))
    cat("\n")
    show(colnames(expList))
    cat("\n")
    cat("Sizes of each ExperimentList element:\n")
    cat("\n")
    cat(show( objSizesdf ))
    cat("\n")
    if (!all(is.na(colDat$vital_status) &
             is.na(colDat$vital_status))) {
        cat("---------------------------\n")
        cat("Overall survival time-to-event summary (in years):\n")
        cat("---------------------------\n")
        cat("\n")
        print(survival::survfit(survival::Surv(colDat$days_to_death / 365,
                                               colDat$vital_status) ~ -1))
        cat("\n")
    }
    cat("\n")
    cat("---------------------------\n")
    cat("Available sample meta-data:\n")
    cat("---------------------------\n")
    cat("\n")
    for (iCol in seq_along(stdColDat)) {
        if (length(unique(stdColDat[, iCol])) < 6) {
            stdColDat[, iCol] <- factor(stdColDat[, iCol])
            cat(paste0(colnames(stdColDat)[iCol], ":\n"))
            print(summary(stdColDat[, iCol]))
        } else if (is(stdColDat[, iCol], "numeric")) {
            cat(paste0(colnames(stdColDat)[iCol], ":\n"))
            print(summary(stdColDat[, iCol]))
        }
        cat("\n")
    }
    cat("Including an additional", numExtraCols, "columns\n")
    cat("}}")
    cat("\n")
    cat("\\keyword{datasets}")
    cat("\n")
    sink(NULL)
    return(filename)
}

.loadEnvObj <- function(filepath, ext = ".rda") {
    OBJENV <- new.env(parent = emptyenv())
    load(filepath, envir = OBJENV)
    objName <- gsub(ext, "", basename(filepath))
    object <- OBJENV[[objName]]
    object
}

.cleanText <- function(x) {
    gsub("%", "\\%", iconv(x, "latin1", "ASCII", sub = "?"), fixed = TRUE)
}

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

    if (length(aliases) > 1L)
        aliases <- paste(aliases, sep = ", ")
    dataDirs <- "data/bits"
    fileNames <- list.files(file.path("../MultiAssayExperiment-TCGA",
                            dataDirs, cancerFolder),
                            full.names = TRUE,
                            pattern = "*\\.rda$")
    objectNames <- gsub(".rda", "", basename(fileNames))
    dataTypes <- gsub("^[A-Z]*_", "", objectNames)
    names(fileNames) <- dataTypes

    slots <- c("metadata", "colData", "sampleMap")
    stdObjSlots <- paste0(cancerFolder, "_", slots)
    names(stdObjSlots) <- slots

    coldataIdx <- grep(stdObjSlots[["colData"]], objectNames, fixed = TRUE)
    stopifnot(S4Vectors::isSingleInteger(coldataIdx))

    colDat <- .loadEnvObj(fileNames[coldataIdx])
    load("../TCGAutils/data/clinicalNames.rda")
    stdNames <- clinicalNames[[cancerFolder]]
    stdNames <- names(colDat) %in% stdNames
    stdColDat <- colDat[, stdNames]

    dataFiles <- fileNames[!(vapply(strsplit(names(fileNames), "_|-"),
            `[`, character(1L), 1L) %in% names(stdObjSlots))]

    dataList <- dataInfo <- vector(mode = "list", length(dataFiles))
    names(dataList) <- names(dataInfo) <- names(dataFiles)

    for (i in seq_along(dataFiles)) {
        object <- .loadEnvObj(dataFiles[[i]])
        dims <- dim(object)
        dataInfo[[i]] <- list(size = format(object.size(object), units = "Mb"),
                              rows = dims[[1L]], columns = dims[[2L]],
                              colnames = colnames(object),
                              rownames = rownames(object),
                              class = class(object),
                              length = length(object))
        dataList[[i]] <- object
    }

    objSizes <- vapply(dataInfo, function(datType) {
        datType$size }, character(1L))

    stopifnot(identical(names(dataFiles), names(objSizes)))
    objSizesdf <- data.frame(assay = names(dataFiles), size.Mb = objSizes,
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
    cat("}}")
    cat("\n")
    cat("\\keyword{datasets}")
    cat("\n")
    sink(NULL)
    return(filename)
}

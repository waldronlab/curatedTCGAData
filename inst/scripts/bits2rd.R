.loadEnvObj <- function(filepath, ext = ".rda") {
    OBJENV <- new.env(parent = emptyenv())
    load(filepath, envir = OBJENV)
    objName <- gsub(ext, "", basename(filepath))
    object <- OBJENV[[objName]]
    object
}

#' Write an Rd man page for a collection of MultiAssayExperiment bits
#'
#' @param cancerFolder Usually saved in 'MultiAssayExperiment-TCGA/data/bits/' and
#' contains several 'rda' files. The folder name denotes the cancer code
#' @param filename Full path of the filename of the .Rd man page to write
#' @param aliases A list of aliases
#' @param descriptions A list of extra lines to be written to the Description
#'
#' @examples
#' rdaFolder <- file.path("../MultiAssayExperiment-TCGA/data/bits/COAD")
#' bit2rd(rdaFolder)
#'
#' @keywords internal
bits2rd <- function(cancerFolder, filename, aliases = NULL, descriptions = NULL)
{
    stopifnot(S4Vectors::isSingleString(cancerFolder))
    if (is.null(aliases))
        aliases <- paste(aliases, sep = ", ")
    dataDirs <- "data/bits"
    fileNames <- list.files(file.path(dataDirs, cancerFolder),
                            full.names = TRUE, all.files = TRUE,
                            pattern = "*\\.rda$")
    objectNames <- gsub(".rda", "", basename(fileNames))
    dataTypes <- gsub("^[A-Z]*_", "", objectNames)
    names(fileNames) <- dataTypes

    slots <- c("metadata", "colData", "sampleMap")
    stdObjSlots <- paste0(cancerFolder, "_", slots)
    names(stdObjSlots) <- slots

    coldataIdx <- match(stdObjSlots[["colData"]], objectNames)
    stopifnot(S4Vectors::isSingleInteger(coldataIdx))

    colDat <- .loadEnvObj(fileNames[coldataIdx])
    colDataNonblank <- colDat[, vapply(colDat, 2,
        function(x) { sum(!is.na(x)) > 0 }, logical(length(colDat)))]

    dataInfo <- vector(mode = "list", length(fileNames))
    dataList <- vector(mode = "list", length(fileNames))
    names(dataInfo) <- dataTypes

    .getDataOnlyL <- function(x) {
        !names(x) %in% c("metadata", "colData", "sampleMap")
    }

    for (i in seq_along(dataInfo)) {
        object <- .loadEnvObj(filenames[[i]])
        dims <- dim(object)
        dataInfo[[i]] <- list(size = format(object.size(object), units = "Mb"),
                              rows = dims[[1L]], columns = dims[[2L]],
                              colnames = colnames(object),
                              rownames = rownames(object),
                              class = class(object),
                              length = length(object))
        dataList[[i]] <- Filter(.getDataOnlyL, object)
    }
    objSizes <- vapply(Filter(.getDataOnlyL, dataInfo),
                       function(datType) { datType$size }, integer(1L))
    objSizesdf <- data.frame(assay = dataType, size.Mb = objSizes,
                             row.names = NULL)
    expList <- ExperimentList(dataList)
    sink(file = filename)
    cat(paste("\\name{", cancerFolder, "}"))
    cat("\n")
    cat(paste("\\alias{", aliases, "}"))
    cat("\n")
    cat(paste("\\docType{data}"))
    cat("\n")
    cat(paste("\\title{", .cleanText(cancerFolder), "}"))
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
    cat(paste(">", cancerFolder))
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
    for (iCol in seq_along(colDataNonblank)) {
        if (length(unique(colDataNonblank[, iCol])) < 6) {
            colDataNonblank[, iCol] <-
                factor(colDataNonblank[, iCol])
            cat(paste0(colnames(colDataNonblank)[iCol], ":\n"))
            print(summary(colDataNonblank[, iCol]))
        } else if (is(colDataNonblank[, iCol], "numeric")) {
            cat(paste0(colnames(colDataNonblank)[iCol], ":\n"))
            print(summary(colDataNonblank[, iCol]))
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

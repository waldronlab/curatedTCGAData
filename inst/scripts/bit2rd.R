.objectSize <- function(filepath) {
    OBJENV <- new.env(parent = emptyenv())
    objNAME <- gsub(".rda", "", basename(filepath))
    dataType <- gsub("^[A-Z]*_", "", objNAME)
    if (!dataType %in% c("metadata", "colData", "sampleMap")) {
        load(filepath, envir = OBJENV)
        format(object.size(OBJENV[[objName]]), units = "Mb")
    }
    NULL
}
#' Write an Rd man page for a collection of MultiAssayExperiment bits
#'
#' @param folder Usually saved in 'MultiAssayExperiment-TCGA/data/bits/' and
#' contains several 'rda' files. The folder name denotes the cancer code
#' @param descriptions A list of extra lines to be written to the Description
#'
#' @examples
#' rdaFolder <- file.path("../MultiAssayExperiment-TCGA/data/bits/COAD")
#' bit2rd(rdaFolder)
#'
#' @keywords internal
bit2rd <- function(cancerFolder, aliases = NULL, descriptions = NULL) {
    stopifnot(S4Vectors::isSingleString(folder))
    if (is.null(aliases))
        aliases <- paste(aliases, sep = ", ")
    dataDirs <- "data/bits"
    fileNames <- list.files(file.path(dataDirs, cancerFolder), full.names = TRUE,
                            all.files = TRUE, pattern = "*\\.rda$")
    objectName <- gsub(".rda", "", basename(fileNames))
    dataType <- gsub("^[A-Z]*_", "", objectName)
    names(fileNames) <- dataType
    slots <- c("metadata", "colData", "sampleMap")
    stdObjSlots <- paste0(cancerFolder, "_", slots)
    coldataIdx <- match(stdObjSlots[[3]], objectName)
    stopifnot(S4Vectors::isSingleInteger(coldataIdx))
    OBJENV <- new.env(parent = emptyenv())
    load(fileNames[coldataIdx], envir = OBJENV)
    colDataNonblank <- OBJENV[[objectName[[coldataIdx]]]]
    colDataNonblank <- colDataNonblank[, vapply(colDataNonblank, 2,
        function(x) { sum(!is.na(x)) > 0 }, logical(length(colDataNonblank)))]
    objSizes <- Filter(function(x) {!names(x) %in% slots},
                       lapply(fileNames, .objectSize))
    objSizesdf <- data.frame(assay = dataType, size.Mb = objSizes,
                             row.names = NULL)
}

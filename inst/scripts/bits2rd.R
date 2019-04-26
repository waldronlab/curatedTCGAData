#' Write an Rd man page for a collection of MultiAssayExperiment bits
#'
#' @param cancerPath Usually saved in 'MultiAssayExperiment.TCGA/data/bits/'
#' and contains several 'rda', 'rds', and/or 'H5' files. The folder name
#' denotes the TCGA cancer code.
#' @param filename Full path of the filename of the .Rd man page to write
#' @param aliases A list of aliases
#' @param descriptions A list of extra lines to be written to the Description
#'
#' @author Levi Waldron, Marcel Ramos
#' @examples
#' rdaFolder <- file.path("../MultiAssayExperiment.TCGA/data/bits/COAD")
#' bit2rd(rdaFolder)
#'
#' @keywords internal
bits2rd <- function(cancerPath, filename, aliases = basename(cancerPath),
        descriptions = "A document describing the TCGA cancer code") {
    stopifnot(S4Vectors::isSingleString(filename))

    aliases <- paste(aliases, sep = ", ")
    cancerFolder <- basename(cancerPath)
    stopifnot(S4Vectors::isSingleString(cancerFolder))

    fileNames <- list.files(cancerPath, full.names = TRUE,
        pattern = allextpat, recursive = TRUE)

    datadata <- .makeMetaDF(fileNames)

    coldatfile <- unlist(datadata[datadata[["dataTypes"]] == "colData", "files"])
    colDataName <- .selectInRow(datadata, "colData", "objectNames", "dataTypes")
    colDat <- .loadEnvObj(coldatfile, colDataName)
    clinicalNames <- .loadPkgData("clinicalNames", "TCGAutils")
    stdNames <- clinicalNames[[cancerFolder]]
    stdNames <- names(colDat) %in% stdNames
    numExtraCols <- sum(!stdNames)
    stdColDat <- colDat[, stdNames]

    dataList <- .loadRDAList(datadata)
    dataList <- .addMethylation(datadata, dataList)

    objSizes <- vapply(
        dataList,
        function(obj) { format(object.size(obj), units = "Mb") },
        character(1L)
    )

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

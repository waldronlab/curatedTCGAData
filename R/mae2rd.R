#' Write a .Rd man page for a MultiAssayExperiment object
#'
#' @param object An object of class MultiAssayExperiment
#' @param filename Full path of the filename of the .Rd man page to write
#' @param objname Name of the object being documented
#' @param aliases A list of aliases
#' @param descriptions A list of extra lines to be written to the Description
#' @return The filename is returned after successfully writing the man file
#' @examples
#' example("MultiAssayExperiment")
#' res <- mae2rd(myMultiAssayExperiment, filename = file.path(tempdir(), "MyMAE.Rd"), objname="MyMAE")
#' res
mae2rd <- function(object,
                   filename,
                   objname,
                   title = objname,
                   aliases = objname,
                   descriptions = NULL) {
    if (!is(object, "MultiAssayExperiment"))
        stop("`object` must be an object of class MultiAssayExperiment")
    if (!is(filename, "character"))
        stop("`filename` must be a non-null character vector")
    if (!is(objname, "character"))
        stop("`objname` must be a non-null character vector")
    cleanText <- function(x) {
        gsub("%", "\\%", iconv(x, "latin1", "ASCII", sub = "?"), fixed = TRUE)
    }
    aliases <- paste(aliases, sep = ", ")
    pdata.nonblank <- pData(object)
    pdata.nonblank <-
        pdata.nonblank[, apply(pdata.nonblank, 2, function(x)
            sum(!is.na(x)) > 0)]
    sink(file = filename)
    cat(paste("\\name{", objname, "}"))
    cat("\n")
    cat(paste("\\alias{", aliases, "}"))
    cat("\n")
    cat(paste("\\docType{data}"))
    cat("\n")
    cat(paste("\\title{", cleanText(title), "}"))
    cat("\n")
    if (!is.null(descriptions)) {
        cat("\\description{")
        cat("\n")
        for (i in 1:length(descriptions)) {
            cat(descriptions[[i]])
        }
        cat("}")
        cat("\n")
    }
    cat("\n")
    cat("\\details{")
    cat("\n")
    cat("\\preformatted{\n")
    cat("--------------------------- \n")
    cat(paste("> experiments(", objname, ")"))
    cat("\n")
    show(experiments(object))
    cat("\n")
    cat("--------------------------- \n")
    cat(paste("> rownames(", objname, ")"))
    cat("\n")
    show(rownames(object))
    cat("\n")
    cat("--------------------------- \n")
    cat(paste("> colnames(", objname, ")"))
    cat("\n")
    show(colnames(object))
    cat("\n")
    if (!all(is.na(object$vital_status) &
             is.na(object$vital_status))) {
        time <- object$days_to_death / 365
        cens <- ifelse(object$vital_status == "deceased", 1, 0)
        cat("Overall survival time-to-event summary (in years):")
        cat("\n")
        print(survival::survfit(Surv(time, cens) ~ -1))
        cat("\n")
    }
    cat("--------------------------- \n")
    cat("Available sample meta-data: \n")
    cat("--------------------------- \n")
    cat("\n")
    for (iCol in 1:ncol(pdata.nonblank)) {
        if (length(unique(pdata.nonblank[, iCol])) < 6) {
            pdata.nonblank[, iCol] <-
                factor(pdata.nonblank[, iCol])
            cat(paste(colnames(pdata.nonblank)[iCol], ": \n", sep = ""))
            print(summary(pdata.nonblank[, iCol]))
            cat("\n")
        }
    }
    cat("}}")
    cat("\n")
    cat("\\keyword{datasets}")
    cat("\n")
    sink(NULL)
    return(filename)
}

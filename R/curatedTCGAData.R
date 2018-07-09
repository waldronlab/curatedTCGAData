.assaysAvailable <- function() {
    assaysAvailable <- c("RNASeqGene", "RNASeq2GeneNorm", "miRNASeqGene",
        "CNASNP", "CNVSNP", "CNASeq", "CNACGH", "Methylation", "mRNAArray",
        "miRNAArray", "RPPAArray", "Mutation", "GISTICA", "GISTICT")
    sort(assaysAvailable)
}

.removeExt <- function(fileNames) {
    gsub(".rda", "", fileNames, fixed = TRUE)
}

.getComboSort <- function(...) {
    sort(apply(expand.grid(..., stringsAsFactors = FALSE), MARGIN = 1L,
               FUN = paste, collapse = "_"))
}

.getResources <- function(ExperimentHub, fileNames) {
    resourceName <- .removeExt(fileNames)
    resources <- lapply(resourceName, function(res) {
        loadResources(ExperimentHub, "curatedTCGAData", res)[[1L]]
    })
    resources
}

.searchFromInputs <- function(glob, searchFields) {
    regGlob <- glob2rx(unique(glob))
    res <- unlist(lapply(regGlob, function(x) {
        grep(x, searchFields, ignore.case = TRUE, value = TRUE)
        }))
    if (!length(res))
        stop("No matches found, modify search criteria")
    res
}

.resolveNames <- function(datList) {
    commonNames <- Reduce(intersect, lapply(datList, names))
    mLogic <- vapply(commonNames, function(y) {
        classes <- unlist(lapply(datList, function(x) class(x[[y]])))
        length(unique(classes)) == 1L
    }, logical(1L))
    unMergeable <- names(which(!mLogic))
    lapply(seq_along(datList), function(i, x) {
        varIdx <- match(unMergeable, names(x[[i]]))
        DFnames <- names(x[[i]])
        DFnames[varIdx] <- paste0(unMergeable, ".", i)
        names(x[[i]]) <- DFnames
        x[[i]]
    }, x = datList)
}

#' Create a MultiAssayExperiment from specific assays and cohorts
#'
#' @details This function will check against available resources in
#' ExperimentHub. Currently, only the latest runDate ("2016-01-28") is
#' supported. Use the \code{dry.run = FALSE} to download remote datasets and
#' build an integrative \linkS4class{MultiAssayExperiment} object.
#'
#' @param diseaseCode a character vector containing the name(s) of TCGA cohorts
#' @param assays a character vector containing the name(s) of TCGA assays
#' @param dry.run logical (default TRUE) whether to return the dataset names
#' before actual download
#' @param ... Additional arguments passed on to the
#' \link[ExperimentHub]{ExperimentHub} constructor
#'
#' @return a \linkS4class{MultiAssayExperiment} of the specified assays and
#' cancer codes
#' @export curatedTCGAData
#'
#' @examples
#' curatedTCGAData(diseaseCode = c("GBM", "ACC"), assays = "CNASNP")
#'
curatedTCGAData <-
    function(diseaseCode = "*", assays = "*", dry.run = TRUE, ...) {
    runDate <- "20160128"
    assaysAvail <- .assaysAvailable()
    diseaseCode <- toupper(diseaseCode)
    tcgaCodes <- diseaseCodes[["Study.Abbreviation"]][
        diseaseCodes[["Available"]] == "Yes"]

    assays_file <- system.file("extdata", "metadata.csv",
        package = "curatedTCGAData", mustWork = TRUE)
    eh_assays <- as.character(read.csv(assays_file)[["ResourceName"]])
    if (diseaseCode == "*" && assays == "*" && dry.run) {
        message("Please see the list below for available cohorts and assays")
        cat("Available Cancer codes:\n",
            paste(strwrap(paste(tcgaCodes, collapse = " "),
                          width = 55), collapse = "\n "), "\n")
        cat("Available Data Types:\n",
            paste(strwrap(paste(assaysAvail, collapse = " "),
                          width = 46), collapse = "\n "), "\n")
        return(invisible())
    }

    resultCodes <- .searchFromInputs(diseaseCode, tcgaCodes)
    resultAssays <- .searchFromInputs(assays, assaysAvail)

    isGISTIC <- grepl("^GISTIC", resultAssays)
    if (any(isGISTIC)) {
        fullG <- vapply(resultAssays[isGISTIC], function(x)
            switch(x, GISTICT = "GISTIC_ThresholdedByGene",
                   GISTICA = "GISTIC_AllByGene"), character(1L))
        resultAssays <- replace(resultAssays, isGISTIC, fullG)
    }
    codeAssay <- .getComboSort(resultCodes, resultAssays)
    reg_names <- paste0("^", codeAssay, ".*", runDate, ".rda$")
    names(reg_names) <- codeAssay

    fileMatches <- unlist(
        lapply(reg_names, function(x) grep(x, eh_assays, value = TRUE)))

    if (!length(fileMatches))
        stop("Cancer and data type combination(s) not available")

    if (dry.run) { return(fileMatches) }

    eh <- ExperimentHub(...)
    assay_list <- .getResources(eh, fileMatches)
    names(assay_list) <- .removeExt(fileMatches)

    eh_experiments <- ExperimentList(assay_list)

    ess_names <- c(colData = 1L, sampleMap = 2L, metadata = 3L)
    ess_resources <- paste0(.getComboSort(resultCodes, names(ess_names)), "-",
        runDate, ".rda")
    names(ess_resources) <- gsub("\\.rda", "", ess_resources)

    ess_list <- .getResources(eh, ess_resources)
    names(ess_list) <- vapply(strsplit(ess_resources, "_|-"), `[`,
        character(1L), 2L)

    if (length(resultCodes) > 1L) {
        # Save metadata from all datasets
        colDats <- names(ess_list) %in% "colData"
        colDatNames <- gsub("\\.rda", "", ess_resources[colDats])
        metas <- lapply(ess_list[colDats], metadata)
        names(metas) <- colDatNames
        metas <- do.call(c, metas)

        ess_list <- lapply(ess_names, function(i, grp, funs) {
            grpd <- grepl(grp[i], ess_resources, fixed = TRUE)
            dats <- ess_list[grpd]
            if (identical(funs[[i]], merge)) {
                dats <- .resolveNames(dats)
                mObj <- Reduce(function(x, y) {
                    merge(x, y, by = intersect(names(x), names(y)),
                        all = TRUE, sort = FALSE)
                    }, dats)
                    rownames(mObj) <- mObj[["patientID"]]
                } else {
                mObj <- Reduce(funs[[i]], dats)
                }
            return(mObj)
            }, grp = names(ess_names), funs = list(merge, rbind, c))

        ## Include all metadata from colData(s)
        metadata(ess_list[["colData"]]) <- metas
    }

    MultiAssayExperiment(
        experiments = eh_experiments,
        colData = ess_list[["colData"]],
        sampleMap = ess_list[["sampleMap"]],
        metadata = ess_list[["metadata"]]
    )
}


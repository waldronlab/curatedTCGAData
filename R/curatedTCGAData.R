.assaysAvailable <- function(listfiles) {
    slots <- c("metadata", "colData", "sampleMap")
    assaysAvailable <-
        vapply(strsplit(gsub("(^[A-Z]*_)(.*)", "\\2", listfiles), "-"),
            `[[`, character(1L), 1L)
    assaysAvailable <- unique(assaysAvailable)
    sort(assaysAvailable[!assaysAvailable %in% slots])
}

.removeExt <- function(fileNames) {
    gsub("\\.[RrHh][Dd5][AaSs]?$", "", fileNames)
}

.getComboSort <- function(...) {
    sort(apply(expand.grid(..., stringsAsFactors = FALSE), MARGIN = 1L,
               FUN = paste, collapse = "_"))
}

.conditionToIndex <- function(startVec, testVec, FUN) {
    logmat <- vapply(startVec, FUN, logical(length(testVec)))
    apply(logmat, 1L, any)
}

.loadMethyl <- function(methylpaths) {
    fact <- gsub("_assays\\.[Hh]5|_se\\.[Rr][Dd][Ss]", "", methylpaths)
    methList <- split(sort(methylpaths), fact)
    names(methList) <- unique(fact)
    lapply(methList, function(methfile) {
        assaydat <- query(hub, methfile[1L])[[1L]]
        se <- query(hub, methfile[2L])[[1L]]
        h5array <- HDF5Array(assaydat, "assay")
        assays(se) <- list(counts = h5array)
        se
    })
}

.getResources <- function(ExperimentHub, resTable) {
    fileNames <- setNames(resTable[["RDataPath"]], resTable[["Title"]])
    anyMeth <- grepl("Methyl", fileNames, ignore.case = TRUE)
    resources <- lapply(fileNames[!anyMeth], function(res) {
        query(ExperimentHub, res)[[1L]]
    })

    if (any(anyMeth))
        resources <- c(resources, .loadMethyl(fileNames[anyMeth]))
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
#'
#' @examples
#' curatedTCGAData(diseaseCode = c("GBM", "ACC"), assays = "CNASNP")
#'
#' @export curatedTCGAData
curatedTCGAData <-
    function(diseaseCode = "*", assays = "*", dry.run = TRUE, ...) {
    runDate <- "20160128"

    assays_file <- system.file("extdata", "metadata.csv",
        package = "curatedTCGAData", mustWork = TRUE)
    assay_metadat <- read.csv(assays_file, stringsAsFactors = FALSE)
    eh_assays <- assay_metadat[["ResourceName"]]

    tcgaCodes <- sort(unique(gsub("(^[A-Z]*)_(.*)", "\\1", eh_assays)))
    assaysAvail <- .assaysAvailable(eh_assays)

    if (identical(diseaseCode, "*") && identical(assays, "*") && dry.run) {
        message("Please see the list below for available cohorts and assays")
        cat("Available Cancer codes:\n",
            paste(strwrap(paste(tcgaCodes, collapse = " "),
                          width = 55), collapse = "\n "), "\n")
        cat("Available Data Types:\n",
            paste(strwrap(paste(assaysAvail, collapse = " "),
                          width = 46), collapse = "\n "), "\n")
        return(invisible())
    }

    diseaseCode <- toupper(diseaseCode)
    resultCodes <- .searchFromInputs(diseaseCode, tcgaCodes)

    resultAssays <- .searchFromInputs(assays, assaysAvail)

    codeAssay <- .getComboSort(resultCodes, resultAssays)

    fileIdx <- .conditionToIndex(codeAssay, eh_assays,
        function(x) startsWith(eh_assays, x))
    fileMatches <- assay_metadat[fileIdx, c("Title", "DispatchClass")]

    if (!length(nrow(fileMatches)))
        stop("Cancer and data type combination(s) not available")

    if (dry.run) { return(fileMatches) }

    eh <- ExperimentHub(...)
    assay_list <-
        .getResources(eh, assay_metadat[fileIdx, c("Title", "RDataPath")])

    eh_experiments <- ExperimentList(assay_list)

    ess_names <- c("colData", "metadata", "sampleMap")
    ess_idx <- .conditionToIndex(.getComboSort(resultCodes, ess_names),
        eh_assays, function(x) startsWith(eh_assays, x))

    ess_list <- .getResources(eh,
        assay_metadat[ess_idx, c("Title", "RDataPath")])

    if (length(resultCodes) > 1L) {
        # Save metadata from all datasets
        colDatIdx <- grepl("colData", names(ess_list), ignore.case = TRUE)
        metas <- lapply(ess_list[colDatIdx], metadata)
        metas <- do.call(c, metas)

        ess_list <- lapply(seq_along(ess_names), function(i, grp, funs) {
            grpd <- grepl(grp[i], names(ess_list), fixed = TRUE)
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
            mObj
        }, grp = ess_names, funs = list(merge, c, rbind))
        names(ess_list) <- ess_names

        ## Include all metadata from colData(s)
        metadata(ess_list[["colData"]]) <- metas
    } else {
        names(ess_list) <- gsub("[A-Z]*_(.*)-[0-9]*", "\\1", names(ess_list))
    }

    MultiAssayExperiment(
        experiments = eh_experiments,
        colData = ess_list[["colData"]],
        sampleMap = ess_list[["sampleMap"]],
        metadata = ess_list[["metadata"]]
    )
}

.assaysAvailable <- function() {
    assaysAvailable <- c("RNASeqGene", "RNASeq2GeneNorm", "miRNASeqGene",
        "CNASNP", "CNVSNP", "CNASeq", "Methylation", "CNACGH", "RPPAArray",
        "Mutation", "GISTICA", "GISTICT")
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
        loadResources(ExperimentHub, "curatedTCGAData", res)
    })
    unlist(resources)
}

.searchFromInputs <- function(glob, searchFields) {
    regGlob <- glob2rx(glob)
    unlist(lapply(regGlob, function(x) {
        grep(x, searchFields, ignore.case = TRUE, value = TRUE)
        }))
}

#' Create a MultiAssayExperiment from specific assays and cohorts
#'
#' @param diseaseCode a character vector containing the name(s) of TCGA cohorts
#' @param assays a character vector containing the name(s) of TCGA assays
#' @param runDate a single string of the TCGA firehose running date
#' @param dry.run logical (default TRUE) whether to return the dataset names
#' before actual download
#'
#' @return a \linkS4class{MultiAssayExperiment} of the specified assays and
#' cancer codes
#' @export curatedTCGAData
#'
#' @examples
#' curatedTCGAData(diseaseCode = "TH*", assays = "CN*")
#'
curatedTCGAData <- function(diseaseCode = "*", assays = "*",
                            runDate = "20160128", dry.run = TRUE) {
    assaysAvail <- .assaysAvailable()
    tcgaCodes <- diseaseCodes[["Study.Abbreviation"]][diseaseCodes[["Available"]] == "Yes"]

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
                          width = 46), collapse = "\n "))
        return(NULL)
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

    fileMatches <- lapply(reg_names, function(x) grep(x, eh_assays, value = TRUE))
    noMatch <- lengths(fileMatches) == 0L
    if (any(noMatch)) {
        warning("Cancer and data type combination(s) not available:\n",
            strwrap(paste(names(fileMatches)[noMatch], collapse = ", "),
                width = 46))
    }
    fileMatches <- unlist(fileMatches)

    eh <- ExperimentHub()
    assay_list <- .getResources(eh, fileMatches)
    names(assay_list) <- .removeExt(fileMatches)

    eh_experiments <- ExperimentList(assay_list)

    ess_names <- c("colData", "sampleMap", "metadata")
    ess_resources <- paste0(.getComboSort(resultCodes, ess_names), "-",
        runDate, ".rda")
    ess_list <- .getResources(eh, ess_resources)
# TODO: Test creation of MultiAssayExperiment with merged colData and
# sampleMaps, support multiple cancers
#    MultiAssayExperiment(experiments = eh_experiments, colData = eh_colData,
#                         sampleMap = eh_sampleMap, metadata = eh_metadata)
    return(NULL)
}


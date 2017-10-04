.assaysAvailable <- function() {
    assaysAvailable <- c("RNASeqGene", "RNASeq2GeneNorm", "miRNASeqGene",
        "CNASNP", "CNVSNP", "CNASeq", "Methylation", "CNACGH", "RPPAArray",
        "Mutation", "GISTICA", "GISTICT")
    sort(assaysAvailable)
}

## CHANGE to use REGEX
.getResource <- function(resourceName, eh, eh_assays) {
    if (!all(resourceName %in% eh_assays))
        stop("Requested ExperimentHub resource not found in repository")
    else
        loadResources(eh, "curatedTCGAData", resourceName)
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
    regCode <- glob2rx(diseaseCode)
    resultCodes <- grep(regCode, tcgaCodes,
                       ignore.case = TRUE, value = TRUE)
    regAssay <- glob2rx(assays)
    resultAssays <- grep(regAssay, assaysAvail,
                       ignore.case = TRUE, value = TRUE)
    eh_names <- vapply(resultCodes, function(code) {
        paste0("^", code, "_", resultAssays)},
            character(length(resultAssays)))
    eh_names <- as.vector(eh_names)
    eh <- ExperimentHub()
    eh_pkg <- "curatedTCGAData"
    assay_list <- lapply(eh_reg, .getResource, eh, eh_assays)
    names(assay_list) <- gsub(".rda", "", eh_names)
    assay_list <- Filter(function(x) !is.null(x), assay_list)
    eh_experiments <- ExperimentList(assay_list)
    chr_colData <- paste0(resultCodes, "_colData", ".rda")
    chr_sampleMap <- paste0(resultCodes, "_sampleMap", ".rda")
    chr_metadata <- paste0(resultCodes, "_metadata", ".rda")
#    eh_colData <- loadResources(eh, eh_pkg, chr_colData)[[1]]
#    eh_sampleMap <- loadResources(eh, eh_pkg, chr_sampleMap)[[1]]
#    eh_metadata <- loadResources(eh, eh_pkg, chr_metadata)[[1]]
# TODO: Test creation of MultiAssayExperiment with merged colData and
# sampleMaps
#    MultiAssayExperiment(experiments = eh_experiments, colData = eh_colData,
#                         sampleMap = eh_sampleMap, metadata = eh_metadata)
    return(NULL)
}


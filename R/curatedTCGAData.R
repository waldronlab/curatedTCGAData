.codesAvailable <- function() {
    dxcodeEnv <- new.env(parent = emptyenv())
    data("diseaseCodes", envir = dxcodeEnv)
    diseaseCodes <- dxcodeEnv[["diseaseCodes"]]
    excludedCodes <- c("COADREAD", "GBMLGG", "KIPAN", "STES", "FPPP", "CNTL",
                       "LCML", "MISC")
    diseaseCodes <-
        diseaseCodes[!diseaseCodes[["Study.Abbreviation"]] %in% excludedCodes, ]
    diseaseCodes
}

.assaysAvailable <- function() {
    assaysAvailable <- c("RNASeqGene", "RNASeq2GeneNorm", "miRNASeqGene",
                         "CNASNP", "CNVSNP", "CNASeq", "Methylation",
                         "RPPAArray", "Mutation", "GISTICA", "GISTICT")
    assaysAvailable
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
    tcgaCodes <- diseaseCodes[["Study.Abbreviation"]]
    eh_assays <- system.file("extdata", "metadata.csv",
        package = "curatedTCGAData", mustWork = TRUE)
    eh_assays <- read.csv(eh_assays)[["ResourceName"]]
    if (diseaseCode == "*" && assays == "*" && dry.run) {
        message("Please see the list below for available cohorts and assays")
        return(list(
            diseaseCodes = matrix(tcgaCodes, ncol = 3L, byrow = TRUE),
            assays = matrix(assaysAvail, ncol = 3L, byrow = TRUE))
        )
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


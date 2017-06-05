.assaysAvailable <- function() {
    assaysAvailable <- c("RNASeqGene", "RNASeq2GeneNorm", "miRNASeqGene",
                         "CNASNP", "CNVSNP", "CNAseq", "CNACGH", "Methylation",
                         "RPPAArray", "Mutations", "gistica", "gistict")
    assaysAvailable
}

.getAssay <- function(resouceName, eh, eh_pkg, eh_assays) {
        loadResources(eh, eh_pkg, eh_names)[[1]]
    # if (is.element(eh_name, eh_assays$ResourceName))
    if(0 == 1) {
        loadResources(eh, eh_pkg, eh_names)[[1]]
    } else {
        message("The", cohort, "cohort does not have the", assay_name,
                "assay\n")
    }
}

#' Create a MultiAssayExperiment from specific assays and cohorts
#'
#' @param cohort a character vector containing the name(s) of TCGA cohorts
#' @param assays a character vector containing the name(s) of TCGA assays
#'
#' @return a MultiAssayExperiment of the specified assays and cohorts
#' @export curatedTCGAData
#'
#' @examples
#' get_curatedTCGAData(cohort = "ACC", assays = c("RNASeqGene",
#' "RNASeq2GeneNorm"))
curatedTCGAData <- function(diseaseCode = "*", assays = "*", dry.run = TRUE) {
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
        paste0(code, "_", resultAssays, ".rda")},
        character(length(resultAssays)))
    eh_names <- as.vector(eh_names)
    eh <- ExperimentHub()
    eh_pkg <- "curatedTCGAData"

    assay_list <- lapply(eh_names, .getAssay, eh, eh_pkg, eh_assays)
    assay_list <- .getAssay(resultAssays, resultCodes, eh, eh_pkg, eh_assays)
    names(assay_list) <- assays
    assay_list <- assay_list[!sapply(assay_list, is.null)]
    eh_experiments <- ExperimentList(assay_list)
    chr_pData <- paste0(cohort, "_colData", ".rda")
    chr_sampleMap <- paste0(cohort, "_sampleMap", ".rda")
    chr_metadata <- paste0(cohort, "_metadata", ".rda")
    eh_pData <- loadResources(eh, eh_pkg, chr_pData)[[1]]
    eh_sampleMap <- loadResources(eh, eh_pkg, chr_sampleMap)[[1]]
    eh_metadata <- loadResources(eh, eh_pkg, chr_metadata)[[1]]
    MultiAssayExperiment(experiments = eh_experiments, pData = eh_pData,
                         sampleMap = eh_sampleMap, metadata = eh_metadata)
}


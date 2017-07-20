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
    assaysAvailable <- .assaysAvailable()
    diseaseCodes <- .codesAvailable()
    eh_assays <- readr::read_csv("inst/extdata/metadata.csv",
                          col_types = cols_only(ResourceName = col_character()))
    if (diseaseCode == "*" && assays == "*" && dry.run) {
        message("Please see the list below for available cohorts and assays")
        return(list(diseaseCodes = diseaseCodes, assays = assaysAvailable))
    }
    regCode <- glob2rx(diseaseCode)
    resultCodes <- grep(regCode, diseaseCodes[["Study.Abbreviation"]],
                       ignore.case = TRUE, value = TRUE)
    regAssay <- glob2rx(assays)
    resultAssays <- grep(regAssay, assaysAvailable,
                       ignore.case = TRUE, value = TRUE)
    eh_reg <- lapply(resultCodes, function(code) {
        return(paste0("^", code, "_", resultAssays))
    })
    eh <- ExperimentHub()
    eh_pkg <- "curatedTCGAData"
    assay_list <- lapply(eh_reg, .getAssay, eh, eh_pkg, eh_assays)
    assay_list <- .getAssay(resultAssays, resultCodes, eh, eh_pkg, eh_assays)
    names(assay_list) <- assays
    assay_list <- assay_list[!sapply(assay_list, is.null)]
    eh_experiments <- ExperimentList(assay_list)
    chr_pData <- paste0(cohort, "_pData", ".rda")
    chr_sampleMap <- paste0(cohort, "_sampleMap", ".rda")
    chr_metadata <- paste0(cohort, "_metadata", ".rda")
    eh_pData <- loadResources(eh, eh_pkg, chr_pData)[[1]]
    eh_sampleMap <- loadResources(eh, eh_pkg, chr_sampleMap)[[1]]
    eh_metadata <- loadResources(eh, eh_pkg, chr_metadata)[[1]]
    MultiAssayExperiment(experiments = eh_experiments, pData = eh_pData,
                         sampleMap = eh_sampleMap, metadata = eh_metadata)
}


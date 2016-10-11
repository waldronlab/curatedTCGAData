.getAssay <- function(assay_name, lc_cohort, eh, eh_pkg, eh_assays) {
    eh_name <- paste0(lc_cohort, "_", assay_name, ".rda")
    #if(is.element(eh_name, eh_assays$ResourceName))
    if(0 == 1) {
        loadResources(eh, eh_pkg, eh_name)[[1]]
    } else {
        cat("The", lc_cohort, "cohort does not have the", assay_name, "assay\n")
    }
}

#' Create a MultiAssayExperiment from specific assays and cohorts
#'
#' @param cohort a character vector containing the name(s) of TCGA cohorts
#' @param assays a character vector containing the name(s) of TCGA assays
#'
#' @return a MultiAssayExperiment of the specified assays and cohorts
#' @export
#'
#' @examples
#' get_curatedTCGAData(cohort = "ACC", assays = c("RNASeqGene",
#' "RNASeq2GeneNorm"))
getCuratedTCGAData <- function(cohort = "ACC", assays = c("RNASeqGene",
                                                           "RNASeq2GeneNorm",
                                                           "miRNASeqGene",
                                                           "CNASNP",
                                                           "CNVSNP",
                                                           "CNAseq",
                                                           "Methylation",
                                                           "RPPAArray",
                                                           "Mutations",
                                                           "gistica",
                                                           "gistict")) {
    lc_cohort <- tolower(cohort)
    eh <- ExperimentHub()
    eh_pkg <- "curatedMetagenomicData"
    eh_assays <- read_csv("inst/extdata/metadata.csv",
                          col_types = cols_only(ResourceName = col_character()))
    assay_list <- lapply(assays, .getAssay, lc_cohort, eh, eh_pkg, eh_assays)
    names(assay_list) <- assays
    assay_list <- assay_list[!sapply(assay_list, is.null)]
    eh_experiments <- ExperimentList(assay_list)
    chr_pData <- paste0(lc_cohort, "_pData", ".rda")
    chr_sampleMap <- paste0(lc_cohort, "_sampleMap", ".rda")
    chr_metadata <- paste0(lc_cohort, "_metadata", ".rda")
    eh_pData <- loadResources(eh, eh_pkg, chr_pData)[[1]]
    eh_sampleMap <- loadResources(eh, eh_pkg, chr_sampleMap)[[1]]
    eh_metadata <- loadResources(eh, eh_pkg, chr_metadata)[[1]]
    MultiAssayExperiment(experiments = eh_experiments, pData = eh_pData,
                         sampleMap = eh_sampleMap, metadata = eh_metadata)
}

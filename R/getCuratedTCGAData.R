#' Download TCGA data types from ExperimentHub
#'
#' @param cohort TCGA cohort disease code (e.g., COAD)
#' @export getCuratedTCGAData
#read_csv("https://raw.githubusercontent.com/waldronlab/MultiAssayExperiment-TCGA/master/MAEOinfo.csv")
getCuratedTCGAData <- function(cohort = "ACC", RNASeqGene = TRUE,
                                RNASeq2GeneNorm = TRUE, miRNASeqGene = TRUE,
                                CNASNP = TRUE, CNVSNP = TRUE, CNAseq = TRUE,
                                Methylation = TRUE, RPPAArray = TRUE,
                                Mutations = TRUE, gistica = TRUE,
                                gistict = TRUE) {
    #check the table

}

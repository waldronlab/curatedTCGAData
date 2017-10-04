.assaysAvailable <- function() {
    assaysAvailable <- c("RNASeqGene", "RNASeq2GeneNorm", "miRNASeqGene",
        "CNASNP", "CNVSNP", "CNASeq", "Methylation", "CNACGH", "RPPAArray",
        "Mutation", "GISTICA", "GISTICT")
    sort(assaysAvailable)
}

.getResources <- function(ExperimentHub, files) {
    resources <- lapply(files, function(res) {
        loadResources(ExperimentHub, "curatedTCGAData", res)
    })
    unlist(resources)
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
    ## TODO: loop over regCode/regAssay in grep call
    resultCodes <- grep(regCode, tcgaCodes,
                       ignore.case = TRUE, value = TRUE)
    regAssay <- glob2rx(assays)
    resultAssays <- grep(regAssay, assaysAvail,
                       ignore.case = TRUE, value = TRUE)
    isGISTIC <- grepl("^GISTIC", resultAssays)
    if (any(isGISTIC)) {
        fullG <- vapply(resultAssays[isGISTIC], function(x)
            switch(x, GISTICT = "GISTIC_ThresholdedByGene",
                   GISTICA = "GISTIC_AllByGene"), character(1L))
        resultAssays <- replace(resultAssays, isGISTIC, fullG)
    }
    codeAssay <- sort(apply(expand.grid(resultCodes, resultAssays),
        1L, paste, collapse = "_"))
    reg_names <- paste0("^", codeAssay, ".*", runDate, ".rda$")
    names(reg_names) <- codeAssay

    eh <- ExperimentHub()
    fileMatches <- lapply(reg_names, function(x) grep(x, eh_assays, value = TRUE))
    noMatch <- lengths(fileMatches) == 0L
    if (any(noMatch)) {
        warning("Cancer and data type combination(s) not available:\n",
            strwrap(paste(names(fileMatches)[noMatch], collapse = ", "),
                width = 46))
    }
    fileMatches <- unlist(fileMatches)

    assay_list <- .getResources(eh, files)
    names(assay_list) <- gsub(".rda", "", fileMatches)

    eh_experiments <- ExperimentList(assay_list)

    chr_colData <- paste0(resultCodes, "_colData", "-", runDate, ".rda")
    chr_sampleMap <- paste0(resultCodes, "_sampleMap", "-", runDate, ".rda")
    chr_metadata <- paste0(resultCodes, "_metadata", "-", runDate, ".rda")
#    eh_colData <- loadResources(eh, eh_pkg, chr_colData)[[1]]
#    eh_sampleMap <- loadResources(eh, eh_pkg, chr_sampleMap)[[1]]
#    eh_metadata <- loadResources(eh, eh_pkg, chr_metadata)[[1]]
# TODO: Test creation of MultiAssayExperiment with merged colData and
# sampleMaps
#    MultiAssayExperiment(experiments = eh_experiments, colData = eh_colData,
#                         sampleMap = eh_sampleMap, metadata = eh_metadata)
    return(NULL)
}


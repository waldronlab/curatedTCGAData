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

#' @importFrom utils head
.conditionToIndex <- function(FUN, reference, position) {
    reference <- vapply(strsplit(reference, "-"), head, character(1L), 1L)
    logmat <- vapply(position, FUN, logical(length(reference)), x = reference)
    apply(logmat, 1L, any)
}

.loadMethyl <- function(ehub, methylpaths, verbose) {
    fact <- gsub("_assays\\.[Hh]5|_se\\.[Rr][Dd][Ss]", "", methylpaths)
    methList <- split(sort(methylpaths), fact)
    fnames <- basename(unique(fact))
    names(methList) <- fnames
    lapply(methList, function(methfile, fn) {
        if (verbose)
            message("Working on: ", paste(fn, collapse = ",\n "))
        assaydat <- query(ehub, methfile[1L])[[1L]]
        se <- query(ehub, methfile[2L])[[1L]]
        h5array <- HDF5Array::HDF5Array(assaydat, "assay001")
        SummarizedExperiment::`assays<-`(
            x = se, withDimnames = FALSE,
            value = list(counts = h5array)
        )
    }, fn = names(methList))
}

#' @importFrom methods is
.checkRaggedExperiment <- function(reslist) {
    reclass <- vapply(reslist, is, logical(1L), "RaggedExperiment")
    if (any(reclass)) {
        if (!requireNamespace("RaggedExperiment", quietly = TRUE))
            stop("Install 'RaggedExperiment' to load these data.")
    }
}

.getResources <- function(ExperimentHub, resTable, verbose) {
    fileNames <- stats::setNames(resTable[["RDataPath"]], resTable[["Title"]])
    anyMeth <- grepl("Methyl", fileNames, ignore.case = TRUE)
    resources <- lapply(fileNames[!anyMeth], function(res) {
        if (verbose)
            message("Working on: ", gsub("\\.rda", "", basename(res)))
        query(ExperimentHub, res)[[1L]]
    })

    if (any(anyMeth))
        resources <- c(resources,
            .loadMethyl(ExperimentHub, fileNames[anyMeth], verbose))

    .checkRaggedExperiment(resources)

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

.test_eh <- function(...) {
    tryCatch({
        ExperimentHub(...)
    }, error = function(e) {
        emsg <- conditionMessage(e)
        if (grepl("Timeout", emsg))
            warning("[experimenthub.bioconductor.org] timeout, localHub=TRUE",
                call.=FALSE)
        ExperimentHub(..., localHub = TRUE)
    })
}

.queryResources <- function(ExperimentHub, resTable, verbose) {
    fileNames <- stats::setNames(resTable[["RDataPath"]], resTable[["Title"]])
    lapply(fileNames, function(res) {
        if (verbose)
            message("Working on: ", gsub("\\.rda", "", basename(res)))
        query(ExperimentHub, res)
    })
}

.getResourceInfo <- function(ExperimentHub, resTable, verbose) {
    infos <- .queryResources(ExperimentHub, resTable, verbose)
    resID <- vapply(infos, names, character(1L))
    restab <- AnnotationHub::getInfoOnIds(ExperimentHub, resID)
    restab <-
        restab[, !names(restab) %in% c("fetch_id", "status", "biocversion")]
    sizes <- as.numeric(restab[["file_size"]])
    class(sizes) <- "object_size"
    restab <- as.data.frame(append(
        restab,
        list(file_size = format(sizes, units = "Mb")),
        which(names(restab) == "title")
    ))
    restab[, -length(restab)]
}

#' Create a MultiAssayExperiment from specific assays and cohorts
#'
#' @description curatedTCGAData assembles data on-the-fly from ExperimentHub
#' to provide cohesive \linkS4class{MultiAssayExperiment} container objects.
#' All the user has to do is to provide TCGA disease code(s) and assay types.
#' It is highly recommended to use the companion package `TCGAutils`,
#' developed to work with TCGA data specifically from `curatedTCGAData` and
#' some flat files.
#'
#' @details This function will check against available resources in
#' ExperimentHub. Only the latest runDate ("2016-01-28") is supported.
#' Use the \code{dry.run = FALSE} to download remote datasets and
#' build an integrative \linkS4class{MultiAssayExperiment} object.
#' For a list of 'diseaseCodes', see the \link{curatedTCGAData-package}
#' help page.
#'
#' @param diseaseCode character() A vector of TCGA cancer cohort codes
#'     (e.g., `COAD`)
#'
#' @param assays character() A vector of TCGA assays, glob matches allowed;
#'     see below for more details
#'
#' @param version character(1) Either `1.1.38` or `2.0.1` indicating the
#'     data version to obtain from `ExperimentHub`. Version `2.0.1` includes
#'     various improvements as well as the addition of the `RNASeq2Gene`
#'     assay. See `version` section details.
#'
#' @param dry.run logical(1) Whether to return the dataset names
#'     before actual download (default TRUE)
#'
#' @param ... Additional arguments passed on to the
#'     \code{\link[ExperimentHub:ExperimentHub-class]{ExperimentHub}}
#'     constructor
#'
#' @param verbose logical(1) Whether to show the dataset currenlty being
#'     (down)loaded (default TRUE)
#'
#' @section Available Assays:
#'
#' Below is a list of partial ExperimentList assay names and their respective
#' description. These assays can be entered as part of the \code{assays}
#' argument in the main function. Partial glob matches are allowed such as:
#' \code{'CN*'} for "CNASeq", "CNASNP", "CNVSNP" assays. Credit: Ludwig G.
#' \preformatted{
#'
#' ExperimentList data types   Description
#' ----------------------------------------------------------------------------
#' SummarizedExperiment*
#'   RNASeqGene                Gene expression values
#'   RNASeq2Gene               RSEM TPM gene expression values
#'   RNASeq2GeneNorm           Upper quartile normalized RSEM TPM gene
#'                             expression values
#'   miRNAArray                Probe-level  miRNA expression values
#'   miRNASeqGene              Gene-level log2 RPM miRNA expression values
#'   mRNAArray                 Unified gene-level mRNA expression values
#'   mRNAArray_huex            Gene-level mRNA expression values from Affymetrix
#'                             Human Exon Array
#'   mRNAArray_TX_g4502a       Gene-level mRNA expression values from Agilent
#'                             244K Array
#'   mRNAArray_TX_ht_hg_u133a  Gene-level mRNA expression values from Affymetrix
#'                             Human Genome U133 Array
#'   GISTIC_AllByGene          Gene-level GISTIC2 copy number values
#'   GISTIC_ThresholdedByGene  Gene-level GISTIC2 thresholded discrete copy
#'                             number values
#'   RPPAArray                 Reverse Phase Protein Array normalized protein
#'                             expression values
#' RangedSummarizedExperiment
#'   GISTIC_Peaks              GISTIC2 thresholded discrete copy number values
#'                             in recurrent peak regions
#' SummarizedExperiment with HDF5Array DelayedMatrix
#'   Methylation_methyl27      Probe-level methylation beta values from Illumina
#'                             HumanMethylation 27K BeadChip
#'   Methylation_methyl450     Probe-level methylation beta values from Infinium
#'                             HumanMethylation 450K BeadChip
#' RaggedExperiment
#'   CNASNP                    Segmented somatic Copy Number Alteration calls
#'                             from SNP array
#'   CNVSNP                    Segmented germline Copy Number Variant calls from
#'                             SNP Array
#'   CNASeq                    Segmented somatic Copy Number Alteration calls
#'                             from low pass DNA Sequencing
#'   Mutation*                 Somatic mutations calls
#'   CNACGH_CGH_hg_244a        Segmented somatic Copy Number Alteration calls
#'                             from CGH Agilent Microarray 244A
#'   CNACGH_CGH_hg_415k_g4124a Segmented somatic Copy Number Alteration calls
#'                             from CGH Agilent Microarray 415K
#' * All can be converted to RangedSummarizedExperiment (except RPPAArray) with
#' TCGAutils
#'
#' }
#'
#' @section version:
#'
#' The new version `2.0.1` includes various improvements including an
#' additional assay that provides `RNASeq2Gene` data as RSEM TPM gene
#' expression values (issue #38). Additional changes include genomic
#' information for `RaggedExperiment` type data objects where '37' is now
#' 'GRCh37' as reported in issue #40. Datasets (e.g., OV, GBM) that contain
#' multiple assays that could be merged are now provided as merged assays
#' (issue #27). We corrected an issue where `mRNAArray` assays were returning
#' `DataFrame`s instead of `matrix` type data (issue #31). Version `1.1.38`
#' provides the original run of `curatedTCGAData` and is provided due to legacy
#' reasons.
#'
#' @seealso curatedTCGAData-package
#'
#' @return a \linkS4class{MultiAssayExperiment} of the specified assays and
#' cancer codes or informative data.frame of resources when `dry.run` is `TRUE`
#'
#' @examples
#'
#' curatedTCGAData(
#'     diseaseCode = c("GBM", "ACC"), assays = "CNASNP", version = "2.0.1"
#' )
#'
#' curatedTCGAData("BRCA", "GISTIC*", "2.0.1")
#'
#' @md
#'
#' @export curatedTCGAData
curatedTCGAData <-
    function(
        diseaseCode = "*", assays = "*", version,
        dry.run = TRUE, verbose = TRUE, ...
    )
{
    runDate <- "20160128"

    if (missing(version) || !version %in% c("1.1.38", "2.0.1", "2.1.0"))
        stop(
            "'version' is not one of '1.1.38', '2.0.1', '2.1.0';",
            " see '?curatedTCGAData'"
        )

    assays_file <- system.file("extdata", "metadata.csv",
        package = "curatedTCGAData", mustWork = TRUE)
    assay_metadat <- read.csv(assays_file, stringsAsFactors = FALSE)
    assay_metadat <-
        assay_metadat[assay_metadat[["SourceVersion"]] == version, ]
    eh_assays <- assay_metadat[["ResourceName"]]

    tcgaCodes <- sort(unique(gsub("(^[A-Z]*)_(.*)", "\\1", eh_assays)))
    assaysAvail <- .assaysAvailable(eh_assays)

    diseaseCode <- toupper(diseaseCode)
    resultCodes <- .searchFromInputs(diseaseCode, tcgaCodes)

    resultAssays <- .searchFromInputs(assays, assaysAvail)

    codeAssay <- .getComboSort(resultCodes, resultAssays)

    fileIdx <- .conditionToIndex(endsWith, eh_assays, codeAssay)
    fileMatches <- assay_metadat[fileIdx, c("Title", "DispatchClass")]

    if (!length(nrow(fileMatches)))
        stop("Cancer and data type combination(s) not available")

    eh <- .test_eh(...)

    if (dry.run) {
        message("See '?curatedTCGAData' for 'diseaseCode' and 'assays' inputs")
        return(
            .getResourceInfo(
                eh, assay_metadat[fileIdx, c("Title", "RDataPath")], FALSE
            )
        )
    }
    assay_list <- .getResources(
        eh, assay_metadat[fileIdx, c("Title", "RDataPath")], verbose
    )

    eh_experiments <- ExperimentList(assay_list)

    ess_names <- c("colData", "metadata", "sampleMap")
    ess_idx <- .conditionToIndex(
        startsWith, eh_assays, .getComboSort(resultCodes, ess_names)
    )

    ess_list <- .getResources(eh,
        assay_metadat[ess_idx, c("Title", "RDataPath")], verbose)

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

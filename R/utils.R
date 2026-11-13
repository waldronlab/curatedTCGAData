## Taken from TCGAutils::TCGAbarcode
.part_bcode <- function(barcodes) {
    filler <- .uniqueDelim(barcodes)
    barcodeMat <- do.call(rbind, strsplit(barcodes, filler))
    apply(barcodeMat[, 1:3, drop = FALSE], 1L, paste, collapse = filler)
}

.uniqueDelim <- function (ids)  {
    nonnum <- gsub("[[:alnum:]]", "", ids)
    dels <- unique(unlist(strsplit(nonnum, "")))
    if (!length(dels))
        dels <- ""
    dels
}

## CPTAC proteomics from the PDC, added via CPTACdata::appendCPTAC

.CPTAC_FRACTIONS <- c("Proteome", "Phosphoproteome", "Glycoproteome")

## Rows of CPTACdata::cptacProjects for the requested cohorts and fractions;
## warns about cohorts in 'warnCodes' (those the user named explicitly)
## without CPTAC data of any requested fraction; other cohorts are dropped silently
.cptacStudies <- function(
    diseaseCodes, fractions, warnCodes = diseaseCodes,
    projects = CPTACdata::cptacProjects
) {
    studies <- projects
    codes <- sub("^TCGA-", "", studies[["tcga_project"]])
    studies <- studies[
        codes %in% diseaseCodes &
            studies[["analytical_fraction"]] %in% fractions,
        ,
        drop = FALSE
    ]
    missingCodes <-
        setdiff(
            intersect(diseaseCodes, warnCodes),
            sub("^TCGA-", "", studies[["tcga_project"]])
        )
    if (length(missingCodes))
        warning(
            "No CPTAC ", paste(fractions, collapse = "/"), " data for: ",
            paste(missingCodes, collapse = ", "),
            "; see '?CPTACdata::cptacProjects'",
            call. = FALSE
        )
    studies
}

## Experiment names given to CPTAC studies in the MultiAssayExperiment,
## prefixed by cohort as with the TCGA assays, e.g., 'BRCA_CPTAC_Proteome'
.cptacExperimentNames <- function(studies) {
    paste0(
        sub("^TCGA-", "", studies[["tcga_project"]]), "_",
        studies[["experiment"]]
    )
}

## Studies without a PDC quantification matrix (data_type is NA) are dropped
## with a warning before calling CPTACdata::appendCPTAC()
.appendCPTAC <- function(mae, studies, verbose) {
    noMatrix <- is.na(studies[["data_type"]])
    if (any(noMatrix))
        warning(
            "No PDC quantification matrix; skipped: ",
            paste(
                unique(.cptacExperimentNames(studies[noMatrix, , drop = FALSE])),
                collapse = ", "
            ),
            call. = FALSE
        )
    studies <- studies[!noMatrix, , drop = FALSE]
    combos <- unique(studies[, c("tcga_project", "analytical_fraction")])
    for (i in seq_len(nrow(combos))) {
        code <- sub("^TCGA-", "", combos[["tcga_project"]][[i]])
        before <- names(mae)
        mae <- withCallingHandlers(
            CPTACdata::appendCPTAC(
                mae, project = code,
                fraction = combos[["analytical_fraction"]][[i]]
            ),
            message = function(m) {
                if (!verbose)
                    invokeRestart("muffleMessage")
            }
        )
        expNames <- names(mae)
        added <- !expNames %in% before
        expNames[added] <- paste0(code, "_", expNames[added])
        names(mae) <- expNames
    }
    mae
}

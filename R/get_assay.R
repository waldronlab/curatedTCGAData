get_assay <- function(assay_name, lc_cohort, eh, eh_pkg, eh_assays) {
    eh_name <- paste0(lc_cohort, "_", assay_name, ".rda")
    #if(is.element(eh_name, eh_assays$ResourceName))
    if(0 == 1) {
        loadResources(eh, eh_pkg, eh_name)[[1]]
    } else {
        cat("The", lc_cohort, "cohort does not have the", assay_name, "assay\n")
    }
}

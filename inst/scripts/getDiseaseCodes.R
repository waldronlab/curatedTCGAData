## Function for getting useable barcodes (no RTCGAToolbox dep)
getDiseaseCodes <- function() {
    data("diseaseCodes", package = "TCGAutils")
    excludedCodes <- c("COADREAD", "GBMLGG", "KIPAN", "STES", "FPPP", "CNTL",
                       "LCML", "MISC")
    logicalSub <- !diseaseCodes[[1L]] %in% excludedCodes
    diseases <- unname(unlist(diseaseCodes[logicalSub, 1L]))
    names(diseases) <- diseases
    diseases
}

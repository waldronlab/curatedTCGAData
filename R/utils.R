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

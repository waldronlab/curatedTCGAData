make_documentation <- function(cancerDirectory, manDirectory) {
    cancerCode <- basename(cancerDirectory)
    manName <- file.path(manDirectory, paste0(cancerCode, ".Rd"))
    message(paste("Documenting:", cancerCode))
    bits2rd(cancerFolder = cancerCode,
            filename = manName)
}

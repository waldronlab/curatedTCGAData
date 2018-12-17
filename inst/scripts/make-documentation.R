make_documentation <- function(cancerDirectory, manDirectory = "man") {
    cancerCode <- basename(cancerDirectory)
    manName <- file.path(manDirectory, paste0(cancerCode, ".Rd"))
    message(paste("Documenting:", cancerCode))
    bits2rd(cancerFolder = cancerCode,
            filename = manName)
}

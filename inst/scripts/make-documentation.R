make_documentation <- function(cancerDirectory, manDirectory) {
    fileNames <- list.files(cancerDirectory, full.names = TRUE,
                            pattern = "*\\.rda$")
    cancerCode <- basename(cancerDirectory)
    manName <- file.path(manDirectory, paste0(cancerCode, ".Rd"))
    message(paste("Documenting:", cancerCode))
    bits2rd(cancerFolder = cancerDirectory,
            filename = manName)
}

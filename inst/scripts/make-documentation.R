make_documentation <- function(cancerDirectory, manDirectory = "man") {
    cancer <- basename(cancerDirectory)
    manName <- file.path(manDirectory, paste0(cancer, ".Rd"))
    message(paste("Documenting:", cancer))
    bits2rd(cancerPath = cancerDirectory, filename = manName)
}

make_documentation <-
    function(dataDir, cancer, version, manDirectory = "man")
{
    dataver <- character(1L)
    if (!missing(version))
        dataver <- paste0("v", version)
    vtag <- if (nchar(dataver)) paste0("-", dataver) else dataver
    manname <- file.path(manDirectory, paste0(cancer, vtag, ".Rd"))
    message(paste("Documenting:", cancer))
    bits2rd(
        dataDir = dataDir,
        cancer = cancer,
        filename = manname,
        version = version
    )
}

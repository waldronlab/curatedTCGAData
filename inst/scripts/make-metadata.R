make_metadata <-
function(
    directory,
    dataDir,
    version,
    ext_pattern,
    resource_maintainer = utils::maintainer("curatedTCGAData"),
    resource_biocVersion = BiocManager::version(),
    append = FALSE
)
{
    exdata <- "inst/extdata"
    metafile <- file.path(exdata, "metadata.csv")

    if (!dir.exists(exdata))
        dir.create(exdata)

    if (file.exists(metafile) && !append)
        file.remove(metafile)

    metadat <- getMetadata(directory = directory, dataDir = dataDir,
        version = version, ext_pattern = ext_pattern,
        resource_maintainer = resource_maintainer,
        resource_biocVersion = resource_biocVersion)

    if (append)
        metadat <- rbind(readr::read_csv(metafile), metadat)

    readr::write_csv(metadat, "inst/extdata/metadata.csv", col_names = TRUE)
}

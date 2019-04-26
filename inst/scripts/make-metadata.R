make_metadata <-
function(
    directory = "~/github/MultiAssayExperiment.TCGA/",
    dataDir = "data/bits",
    ext_pattern = allextpat,
    resource_maintainer = utils::maintainer("curatedTCGAData"),
    resource_biocVersion = BiocManager::version())
{
    exdata <- "inst/extdata"
    metafile <- file.path(exdata, "metadata.csv")

    if (!dir.exists(exdata))
        dir.create(exdata)

    if (file.exists(metafile))
        file.remove(metafile)

    metadat <- getMetadata(directory = directory, dataDir = dataDir,
        ext_pattern = ext_pattern, resource_maintainer = resource_maintainer,
        resource_biocVersion = resource_biocVersion)

    readr::write_csv(metadat, "inst/extdata/metadata.csv", col_names = TRUE)
}

make_metadata <- function(directory = "~/github/MultiAssayExperiment-TCGA/",
    dataDir = "data/bits", ext_pattern = allextpat,
    resource_maintainer = read.dcf("DESCRIPTION", "Maintainer")[[1]],
    resource_biocVersion = BiocManager::version()) {
    if (!dir.exists("inst/extdata"))
        dir.create("inst/extdata")
    if (file.exists("inst/extdata/metadata.csv"))
        file.remove("inst/extdata/metadata.csv")
    metadat <- getMetadata(directory = directory, dataDir = dataDir,
        ext_pattern = ext_pattern, resource_maintainer = resource_maintainer,
        resource_biocVersion = resource_biocVersion)
    readr::write_csv(metadat, "inst/extdata/metadata.csv", col_names = TRUE)
}

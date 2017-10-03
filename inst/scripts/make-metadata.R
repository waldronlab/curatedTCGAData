make_metadata <- function(bitsFolder) {
    if (!dir.exists("inst/extdata"))
        dir.create("inst/extdata")
    if (file.exists("inst/extdata/metadata.csv"))
        file.remove("inst/extdata/metadata.csv")
    resource_list <- list.files(bitsFolder, recursive = TRUE,
        full.names = TRUE)
    resource_maintainer <- read.dcf("DESCRIPTION", "Maintainer")[[1]]
    resource_biocVersion <- BiocInstaller::biocVersion()
    metadat <- do.call(dplyr::bind_rows,
           lapply(resource_list, getMetadata,
                    resource_maintainer, resource_biocVersion))
    readr::write_csv(metadat, "inst/extdata/metadata.csv", append = TRUE,
                     col_names = TRUE)
}

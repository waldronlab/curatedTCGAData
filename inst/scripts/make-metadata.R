make_metadata <- function(bitsFolder) {
    if (!dir.exists("inst/extdata"))
        dir.create("inst/extdata")
    if (file.exists("inst/extdata/metadata.csv"))
        file.remove("inst/extdata/metadata.csv")
    metadat <- do.call(dplyr::bind_rows,
           lapply(resource_list, getMetadata,
                    resource_maintainer, resource_biocVersion))
    readr::write_csv(metadat, "inst/extdata/metadata.csv", col_names = TRUE)
}

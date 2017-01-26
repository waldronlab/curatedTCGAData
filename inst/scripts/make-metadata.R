## Load helper function for collecting metadata
source("getMetadata.R")

make_metadata <- function() {
    if (!dir.exists("inst/extdata"))
        dir.create("inst/extdata")
    if (file.exists("inst/extdata/metadata.csv"))
        file.remove("inst/extdata/metadata.csv")
    resource_list <- dir("data")
    resource_maintainer <- read.dcf("DESCRIPTION", "Maintainer")[[1]]
    resource_biocVersion <- BiocInstaller::biocVersion()
    bplapply(resource_list, getMetadata, resource_maintainer,
             resource_biocVersion) %>%
    Reduce(dplyr::bind_rows, .) %>%
    readr::write_csv(., "inst/extdata/metadata.csv", append = TRUE,
                     col_names = TRUE)
}

make_metadata()

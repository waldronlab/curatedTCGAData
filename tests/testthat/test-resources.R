context("ensure resources are present")

test_that("metadata numbers match ExperimentHub", {
    assays_file <- system.file("extdata", "metadata.csv",
        package = "curatedTCGAData", mustWork = TRUE)
    meta <- read.csv(assays_file, stringsAsFactors = FALSE)
    ehub <- query(ExperimentHub(), "curatedTCGAData")

    ## expect that records in metadata and ExperimentHub
    expect_equal(nrow(meta), length(ehub))
})

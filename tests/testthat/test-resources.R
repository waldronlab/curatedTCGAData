context("ensure resources are present")

test_that("metadata numbers match ExperimentHub", {
    assays_file <- system.file("extdata", "metadata.csv",
        package = "curatedTCGAData", mustWork = TRUE)
    metadataFile <- read.csv(assays_file, stringsAsFactors = FALSE)
    actual_resources <- unique(metadataFile[["RDataPath"]])
    EHub <- query(ExperimentHub(), "curatedTCGAData")
    expect_equal(length(actual_resources), length(EHub))
})

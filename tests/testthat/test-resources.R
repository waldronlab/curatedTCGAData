context("ensure resources are present")

test_that("metadata numbers match ExperimentHub", {
    assays_file <- system.file("extdata", "metadata.csv",
        package = "curatedTCGAData", mustWork = TRUE)
    metadataFile <- read.csv(assays_file, stringsAsFactors = FALSE)
    EHub <- query(ExperimentHub(), "curatedTCGAData")
    ## SKCM patch
    metaskc <- grepl("SKCM", metadataFile[["Title"]])
    ehubskcm <- grepl("SKCM", EHub$title)
    additional <- sum(ehubskcm) - sum(metaskc)
    ## END patch
    expect_equal(nrow(metadataFile), length(EHub)-additional)
})

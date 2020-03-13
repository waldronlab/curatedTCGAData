context("Test that the localHub works")

test_that("localHub is used when no internet available", {

    .ehub <- function(..., localHub = FALSE) {
        if (localHub)
            TRUE
        else
            stop("Timeout")
    }

    library(ExperimentHub)
    with_mock(ExperimentHub = .ehub, {
        expect_warning(.test_eh())
        expect_true(.test_eh())
    })

})


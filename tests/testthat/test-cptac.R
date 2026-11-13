context("CPTAC proteomics integration")

test_that(".cptacStudies selects cohorts and fractions", {
    skip_if_not_installed("CPTACdata")
    studies <- .cptacStudies(c("BRCA", "OV"), "Proteome")
    expect_setequal(studies[["tcga_project"]], c("TCGA-BRCA", "TCGA-OV"))
    expect_true(all(studies[["analytical_fraction"]] == "Proteome"))
    expect_setequal(
        .cptacExperimentNames(studies),
        c("BRCA_CPTAC_Proteome", "OV_CPTAC_JHU_Proteome",
            "OV_CPTAC_PNNL_Proteome")
    )
    expect_warning(
        studies <- .cptacStudies(c("BRCA", "ACC"), "Phosphoproteome"),
        "No CPTAC Phosphoproteome data for: ACC"
    )
    expect_identical(studies[["tcga_project"]], "TCGA-BRCA")
    expect_warning(
        studies <- .cptacStudies("ACC", .CPTAC_FRACTIONS), "ACC"
    )
    expect_identical(nrow(studies), 0L)
})

test_that(".appendCPTAC prefixes added experiments by cohort", {
    skip_if_not_installed("CPTACdata")
    patients <- c("TCGA-A1-0001", "TCGA-A1-0002", "TCGA-B1-0001")
    samples <- paste0(patients, "-01A")
    mae <- MultiAssayExperiment(
        experiments = ExperimentList(
            BRCA_RNASeq2GeneNorm = matrix(1:4, 2, dimnames = list(
                c("g1", "g2"), samples[1:2])),
            OV_RNASeq2GeneNorm = matrix(1:2, 2, dimnames = list(
                c("g1", "g2"), samples[3]))
        ),
        colData = S4Vectors::DataFrame(row.names = patients),
        sampleMap = S4Vectors::DataFrame(
            assay = factor(c(rep("BRCA_RNASeq2GeneNorm", 2L),
                "OV_RNASeq2GeneNorm")),
            primary = patients, colname = samples
        )
    )
    calls <- list()
    fakeAppend <- function(mae, project, fraction, dry_run = FALSE) {
        calls[[length(calls) + 1L]] <<- c(project, fraction)
        message("Adding...")
        sample <- if (project == "BRCA") samples[1] else samples[3]
        experiment <- matrix(0, 1, dimnames = list("P1", sample))
        nm <- paste0("CPTAC_", fraction)
        c(mae, stats::setNames(list(experiment), nm),
            sampleMap = S4Vectors::DataFrame(
                assay = factor(nm), primary = substr(sample, 1, 12),
                colname = sample
            ))
    }
    local_mocked_bindings(appendCPTAC = fakeAppend, .package = "CPTACdata")
    studies <- .cptacStudies(c("BRCA", "OV"), c("Proteome", "Phosphoproteome"))
    expect_silent(res <- .appendCPTAC(mae, studies, verbose = FALSE))
    expect_length(calls, 4L)
    expect_setequal(
        names(res),
        c("BRCA_RNASeq2GeneNorm", "OV_RNASeq2GeneNorm",
            "BRCA_CPTAC_Proteome", "BRCA_CPTAC_Phosphoproteome",
            "OV_CPTAC_Proteome", "OV_CPTAC_Phosphoproteome")
    )
    expect_identical(
        colnames(res)[["OV_CPTAC_Proteome"]], "TCGA-B1-0001-01A"
    )
    expect_true(validObject(res))
    expect_message(.appendCPTAC(mae, studies, verbose = TRUE), "Adding")
})

test_that("cptacFraction is validated", {
    expect_error(
        curatedTCGAData("BRCA", "RNASeq2GeneNorm", "2.1.1",
            cptacFraction = "Lipidome"),
        "should be one of"
    )
})

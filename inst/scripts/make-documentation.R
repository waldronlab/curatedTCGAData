## Source the converter function (MultiAssayExperiment RDS to Rd)
source("../inst/scripts/mae2rd.R")

rdsDir <- dirname(rdsFiles[[1L]])
manDir <- file.path("../man")

makeDocumentation <- function(rdsDirectory, manDirectory) {
    fileNames <- list.files(rdsDirectory, full.names = TRUE,
                            pattern = "*MAEO\\.rds$")
    cancerCode <- toupper(sub("MAEO.rds", "", basename(fileNames),
                              fixed = TRUE))
    manNames <- file.path(manDirectory, paste0(cancerCode, ".Rd"))
    for (i in seq_along(fileNames)) {
        obj <- readRDS(fileNames[i])
        message(paste("Documenting:", cancerCode[i]))
        mae2rd(object = obj,
               filename = manNames[i],
               objname = cancerCode[i])
    }
}

## Document
makeDocumentation(rdsDir, manDir)

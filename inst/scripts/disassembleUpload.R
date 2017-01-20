## Script to disassemble and upload MultiAssayExperiment pieces
## Run script from project folder (curatedTCGAdata)
library(curatedTCGAData)
library(MultiAssayExperiment)
library(AnnotationHubData)
library(magrittr)


repoDir <- normalizePath(Sys.getenv("REPO"))
dataDir <- file.path(repoDir, "data")

if (!dir.exists(dataDir))
    dir.create(dataDir)

rdsFiles <-
    list.files(file.path(repoDir,
                         "../MultiAssayExperiment-TCGA/data/built/"),
               full.names = TRUE, pattern = "*MAEO\\.rds$")
rdsDir <- dirname(rdsFiles[[1L]])
manDir <- file.path("../man")

## For loop for disassembly
for (singleFile in rdsFiles) {
    prepend <- basename(singleFile) %>% gsub("MAEO.rds", "", .) %>%
        paste0(., "_")

    readRDS(singleFile) %>%
         disassemble(., prepend = prepend, directory = dataDir)

}

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

## Upload files in folder
dataFiles <- list.files(file = dataDir, full.names = TRUE)

for (singleFile in dataFiles) {

AnnotationHubData:::upload_to_S3(file = singleFile,
                         remotename = basename(singleFile),
                         bucket = "experimenthub/curatedTCGAData")
}


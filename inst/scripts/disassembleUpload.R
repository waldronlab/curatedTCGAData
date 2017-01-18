## Script to disassemble and upload MultiAssayExperiment pieces
## Run script from project folder (curatedTCGAdata)
library(curatedTCGAData)
library(MultiAssayExperiment)
library(AnnotationHubData)

dataDir <- file.path("data")

if (!dir.exists(dataDir))
    dir.create(dataDir))

rdsFiles <-
    file.path("~/Documents/github/MultiAssayExperiment-TCGA/data/built")
rdsDir <- dirname(rdsFiles)
manDir <- file.path("man")

## For loop for disassembly
for (singleFile in rdsFiles) {
    prepend <- basename(singleFile) %>% gsub("MAEO.rds", "", .) %>%
        paste0(., "_")

    readRDS(singleFile) %>% disassemble(., prepend = prepend,
                                        directory = "./data")

}

makeDocumentation <- function(rdsDirectory, manDirectory) {
    fileNames <- list.files(rdsDirectory, full.names = TRUE,
                            pattern = "*MAEO\\.rds$")
    cancerCode <- toupper(sub("MAEO.rds", "", basename(fnames), fixed = TRUE))
    manNames <- file.path(manDirectory, paste0(cancerCode, ".Rd"))
    for (i in seq_along(fileNames)) {
        obj <- readRDS(fileNames[i])
        message(paste("Documenting:", cancerCode[i]))
        curatedTCGAData::mae2rd(object = obj,
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


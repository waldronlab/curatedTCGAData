## Generate documents
## Get codes from loaded function
TCGAcodes <-
    diseaseCodes[["Study.Abbreviation"]][diseaseCodes[["Available"]] == "Yes"]

## Folder containing cancer folders
dataBitsLocation <- file.path(repoDir,
    "../MultiAssayExperiment-TCGA/data/bits")

## Document by cancer folder
cancerFolders <- file.path(dataBitsLocation, TCGAcodes)

message("Creating documentation pages")
lapply(cancerFolders, make_documentation, manDir)


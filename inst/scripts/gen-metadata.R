## Get codes from loaded function
TCGAcodes <-
    diseaseCodes[["Study.Abbreviation"]][diseaseCodes[["Available"]] == "Yes"]

## Folder containing cancer folders
dataBitsLocation <- file.path(repoDir,
    "../MultiAssayExperiment-TCGA/data/bits")

## create metadata.csv in inst/extdata folder
message("Generating metadata...")
make_metadata(dataBitsLocation)


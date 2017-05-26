## The code used to run this TCGA pipeline and to create all the data objects
## can be found in this GitHub repository:
# browseURL("https://github.com/waldronlab/MultiAssayExperiment-TCGA")

## Note: The `MultiAssayExperiment-TCGA` repository has this GitHub package
## dependency:
# browseURL("https://github.com/waldronlab/TCGAutils")

## Please see the following script for installing the proper dependencies:
# browseURL(paste0("https://github.com/waldronlab/",
# "MultiAssayExperiment-TCGA/blob/master/R/installLibraries.R"))

## Some manual work may be required when downloaded the curated datasets
## After all data is downloaded and in the proper folders, the user
## only needs to run one command.

## Move to the repository directory
## system("cd MultiAssayExperiment-TCGA/exec")

## Run the script that builds the datasets
## system("./globalScript.sh")

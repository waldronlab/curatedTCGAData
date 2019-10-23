## The code used to run this TCGA pipeline and to create all the data objects
## can be found in this GitHub Package repository:
# BiocManager::install("waldronlab/MultiAssayExperiment.TCGA")

## Note: The `MultiAssayExperiment.TCGA` package has a number of package
## dependencies. See the `DESCRIPTION` file:
# browseURL("https://github.com/waldronlab/MultiAssayExperiment.TCGA/blob/master/DESCRIPTION")

## Some manual work may be required when downloaded the curated datasets
## After all data is downloaded and in the proper folders, the user
## only needs to run one command.

## Move to the repository directory
## system("cd MultiAssayExperiment.TCGA/exec")

## Run the script that builds the datasets
## system("./runPipeline.sh")

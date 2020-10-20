## CHANGES IN VERSION 1.12.0

### Bug fixes and minor improvements

* Output dataset options as table when `dry.run` is enabled in the main
function.
* Check for `RaggedExperiment` dependency when loading data that uses the
data representation (@vjcitn, #39)

## CHANGES IN VERSION 1.10.0

### New features

* Control verbosity of `curatedTCGAData` function with the `verbose` argument
(@lgeistlinger, #35)
* Fallback added for when `ExperimentHub` service is offline (@vjcitn, #36)
* Data export section added to main vignette due to new features in
`MultiAssayExperiment`

## CHANGES IN VERSION 1.8.0

### Bug fixes and minor improvements

* Add a caveats section for working with TCGA data in the vignette
(@vjcitn suggest)
* Provide examples for `TCGAutils::TCGAsampleSelect` in the vignette

## CHANGES IN VERSION 1.6.0

### New features

* Methylation data represented as a `DelayedMatrix` with associated HDF5 files
within a `SummarizedExperiment` object. They can be downloaded using the
`Methyl*` assay keyword.
* Big changes to documentation with table added for detailing `ExperimentList`
data types (thanks to @lgeistlinger)
* Vignette now includes some helper functions provided by `TCGAutils`

### Bug fixes and minor improvements

* `TCGAutils` is available for use with `curatedTCGAData`
* 'GISTIC_Peak' datasets are force downloaded due to caching mechanism issues
* Issue of missing 'GISTIC' data for `LAML` and `SKCM` due to incorrect file
links has been resolved (#29 @pcheng84)
* Updated data documentation

## CHANGES IN VERSION 1.4.0

### New features

* `dry.run` argument in `curatedTCGAData` returns a list of metadata file
matches

### Bug fixes and minor improvements

* Main function now uses the `MultiAssayExperiment` constructor function

## CHANGES IN VERSION 1.2.0

### New features

* Released to Bioconductor
* Supports multiple cancers with auto-merge of `colData`
* Added full list of data types

### Bug fixes and minor improvements

* Code clean up (helper functions)
* Use metadata in main function to return list of datasets

## CHANGES IN VERSION 0.99.38

### New features

* Updated `curatedTCGAData` function
* Supports multiple cancers
* Includes curated `colData` datasets
* Updated vignette, documentation from new builds
* Updated metadata from pipeline

### Bug fixes and minor improvements

* Updated NAMESPACE and DESCRIPTION
* Included test for number of resources in EH and in metadata
* Improved input handling of main function

## CHANGES IN VERSION 0.2.0

### Bug fixes and minor improvements

* Updated `DESCRIPTION` file to reflect authorship.
* Progress towards submission to ExperimentHub

## CHANGES IN VERSION 0.1.0

* Added a `NEWS` file to track changes to the package.

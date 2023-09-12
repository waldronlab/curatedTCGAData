## Changes in version 1.24.0 

### Bug fixes and minor improvements

* Create an on-the-fly `sampleMap` for `RNASeq2GeneNorm*` data version `2.1.1`.
Data source has munged `colnames` and sample maps were not updated in the
latest upload (#59, @LiNk-NY) 

## Changes in version 1.22.0 

### New features

* Data version 2.1.1 is now availble. It contains updates to `RNASeq2GeneNorm*`, 
and `RNASeq2Gene*`, as well as fixes to the curated subtypes in the `colData`
for `OV` and `SKCM`.

### Bug fixes and minor improvements

* When the `assays` argument was `RNASeq2Gene`, `curatedTCGAData` would
incorrectly include `RNASeq2GeneNorm` assays. Users who want to return both
assay types should enter `RNASeq2Gene*` instead (with an asterisk).
* When more than one `BiocVersion` in the metadata was available, the
`curatedTCGAData` function would return duplicate results. This has been fixed
to obtain either the matching `BiocVersion` or the latest version in the
metadata.

## Changes in version 1.14.0

### New features

* The `version` argument now allows users to select either `1.1.38` or
`2.0.1`.
* Version `2.0.1` includes `RNASeq2Gene` data as RSEM TPM gene
expression values (#38, @mherberg).
* Genomic information updated for `RaggedExperiment` type data objects where
'37' is now 'GRCh37' (#40, @vjcitn).
* Datasets (e.g., OV, GBM) that contain multiple assays that could be merged
are now provided as merged assays (#27, @lwaldron).
* The vignette now includes sections on how to use the `TCGAprimaryTumors` and
`getWithColData` functions.

### Bug fixes and minor improvements

* `mRNAArray` assays now return `matrix` type data instead of `DataFrame`
(#31, @lgeistlinger, @vjcitn).
* Removed force download of GISTIC resources
* Published article now available with `citation("curatedTCGAData")`

## Changes in version 1.12.0

### Bug fixes and minor improvements

* Output dataset options as table when `dry.run` is enabled in the main
function.
* Check for `RaggedExperiment` dependency when loading data that uses the
data representation (@vjcitn, #39)

## Changes in version 1.10.0

### New features

* Control verbosity of `curatedTCGAData` function with the `verbose` argument
(@lgeistlinger, #35)
* Fallback added for when `ExperimentHub` service is offline (@vjcitn, #36)
* Data export section added to main vignette due to new features in
`MultiAssayExperiment`

## Changes in version 1.8.0

### Bug fixes and minor improvements

* Add a caveats section for working with TCGA data in the vignette
(@vjcitn suggest)
* Provide examples for `TCGAutils::TCGAsampleSelect` in the vignette

## Changes in version 1.6.0

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

## Changes in version 1.4.0

### New features

* `dry.run` argument in `curatedTCGAData` returns a list of metadata file
matches

### Bug fixes and minor improvements

* Main function now uses the `MultiAssayExperiment` constructor function

## Changes in version 1.2.0

### New features

* Released to Bioconductor
* Supports multiple cancers with auto-merge of `colData`
* Added full list of data types

### Bug fixes and minor improvements

* Code clean up (helper functions)
* Use metadata in main function to return list of datasets

## Changes in version 0.99.38

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

## Changes in version 0.2.0

### Bug fixes and minor improvements

* Updated `DESCRIPTION` file to reflect authorship.
* Progress towards submission to ExperimentHub

## Changes in version 0.1.0

* Added a `NEWS` file to track changes to the package.

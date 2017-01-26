# curatedTCGAData

`curatedTCGAData` is a `Bioconductor` package that uses `ExperimentHub` to 
access processed and curated data from The Cancer Genome Atlas (TCGA) in the 
form of MultiAssayExperiment objects.

## Clinical Curation

todo

## Subtype Curation

Among the different TCGA cohorts (n = 33) there were various molecular subtypes detailed (methylation, mRNA, etc.) in the [primary publications](https://www.zotero.org/groups/tcga_research_network_publications/items). Yet, to our knowledge no publicly available datasets contain subtype information clinical histology. As such, we have integrated both clinical and molecular subtype information by curating the clinical variables as detailed above and incorporating subtype information from the supplements of the primary publications. All subtype curation was done by hand and where supplemental information was not available in a publication the coresponding author was emailed and asked to provide it. With the addition of the molecular subtype information it becomes possible to examine subtype characterization across cohorts and will hopeful provide deeper insight into oncogenisis. 

## Getting Started

Install `curatedTCGAData` through `Bioconductor` using `BiocInstaller` and open the package vignette.

```
BiocInstaller::biocLite("waldronlab/curatedTCGAData")
browseVignettes("curatedTCGAData")
```

## Reporting Bugs

We did something wrong? Create a new 
[issue](https://github.com/waldronlab/curatedTCGAData/issues) and we'll get to it ASAP.

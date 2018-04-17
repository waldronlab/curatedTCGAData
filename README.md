# curatedTCGAData

`curatedTCGAData` is an experiment data package in both release and development
versions of Bioconductor. It makes use of `ExperimentHub` to access
pre-processed and curated data from **T**he **C**ancer **G**enome **A**tlas
(**TCGA**) as MultiAssayExperiment objects.

## Clinical Curation

The clinical datasets taken from `TCGA` include a number of variables including
demographic and pathology variables. Curation was done to merge additional
level one data and subtype information. Any empty variables were removed and
their names were saved in the `colData` metadata. Ongoing efforts include merging
the different levels of variables in the `colData` and thus reducing the
repetition of some clinical variables.

## Subtype Curation

Among the different *TCGA* cohorts (n = 33) there were various molecular subtypes
detailed (methylation, mRNA, etc.) in the [primary publications][]. Currently,
no publicly available datasets contain clinical subtype information. As such,
we have integrated both clinical and molecular subtype information by curating
the clinical variables as detailed above and incorporating subtype information
from the supplements of the primary publications. All subtype curation was done
by hand and where supplemental information was not available in a publication
the coresponding author was emailed and asked to provide it. With the addition
of the molecular subtype information it becomes possible to examine subtype
characterization across cohorts and will hopeful provide deeper insight into
oncogenisis. 

## Getting Started

Install `curatedTCGAData` from [Bioconductor][] using `BiocInstaller`:

```
BiocInstaller::biocLite("curatedTCGAData")
browseVignettes("curatedTCGAData")
```

## Reporting Bugs

We appreciate all feedback to our experiment data package. 
Please file an [issue on GitHub][] and we will get to it ASAP.

[primary publications]: https://www.zotero.org/groups/tcga_research_network_publications/items
[issue on GitHub]: https://github.com/waldronlab/curatedTCGAData/issues
[Bioconductor]: https://bioconductor.org/

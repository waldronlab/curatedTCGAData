# curatedTCGAData <a href='https://waldronlab.io/'><img src='https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/curatedTCGAData/curatedTCGAData.png' align="right" height="139" /></a>

[![BioC status](http://www.bioconductor.org/shields/build/release/data-experiment/curatedTCGAData.svg)](http://bioconductor.org/checkResults/release/data-experiment-LATEST/curatedTCGAData/)
[![Travis Build Status](https://api.travis-ci.org/waldronlab/curatedTCGAData.svg?branch=master)](https://travis-ci.org/waldronlab/curatedTCGAData)
[![Downloads](http://www.bioconductor.org/shields/downloads/curatedTCGAData.svg)](https://bioconductor.org/packages/stats/data-experiment/curatedTCGAData)

https://travis-ci.org/waldronlab/curatedTCGAData.svg?branch=RELEASE_3_9

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

## Genome versions

See the [NCI wiki](https://web.archive.org/web/20170711175956/https://wiki.nci.nih.gov/display/TCGA/Platform#Platform-PlatformsandAssociatedReferenceGenomeAssemblies) and [summary on FireHose](https://confluence.broadinstitute.org/display/GDAC/FAQ#FAQ-Q%C2%A0Whatreferencegenomebuildareyouusing) for information on genome builds for all aligned data types.

## Getting Started

Install `curatedTCGAData` from [Bioconductor][] using `BiocManager`:

```
if (!require("BiocManager"))
    install.packages("BiocManager")

library(BiocManager)

install(version = "devel")
install("curatedTCGAData")

browseVignettes("curatedTCGAData")
```

## Reporting Bugs

We appreciate all feedback to our experiment data package. 
Please file an [issue on GitHub][] and we will get to it ASAP.

[primary publications]: https://www.zotero.org/groups/tcga_research_network_publications/items
[issue on GitHub]: https://github.com/waldronlab/curatedTCGAData/issues
[Bioconductor]: https://bioconductor.org/

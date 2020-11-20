#!/bin/bash

export REPO="$HOME/gh/curatedTCGAData/"

$HOME/src/svn/r-devel/R/bin/Rscript $REPO/inst/scripts/docsMeta.R --verbose


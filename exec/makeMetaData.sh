#!/bin/bash

export REPO="/home/$USER/Documents/github/curatedTCGAData/"

/home/$USER/src/svn/r-release/R/bin/Rscript $REPO/inst/scripts/makeMetaDocs.R --verbose


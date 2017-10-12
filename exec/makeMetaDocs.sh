#!/bin/bash

export REPO="/home/$USER/Documents/github/curatedTCGAData/"

/home/$USER/src/svn/r-release/R/bin/Rscript $REPO/inst/scripts/load-resources.R --verbose
# /home/$USER/src/svn/r-release/R/bin/Rscript $REPO/inst/scripts/gen-metadata.R --verbose
/home/$USER/src/svn/r-release/R/bin/Rscript $REPO/inst/scripts/gen-docs.R --verbose


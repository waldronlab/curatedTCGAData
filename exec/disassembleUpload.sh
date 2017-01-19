#!/bin/bash

REPO='/home/$USER/Documents/github/curatedTCGAData/'

/home/$USER/src/svn/r-devel/R/bin/Rscript $REPO/inst/scripts/disassembleUpload.R --verbose


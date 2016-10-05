rdsdir <- "~/Documents/maeo"
mandir <- "~/git/curatedTCGAData/man"

make_documentation <- function(rdsdir, mandir) {
    library(curatedTCGAData)
    library(MultiAssayExperiment)
    fnames <-
        list.files(rdsdir, full.names = TRUE, pattern = "^.*MAEO\\.rds$")
    objnames <-
        toupper(sub("MAEO.rds", "", basename(fnames), fixed = TRUE))
    mannames <- file.path(mandir, paste0(objnames, ".Rd"))
    for (i in seq_along(fnames)) {
##        if (file.exists(mannames[i]))
##            next
        obj <- readRDS(fnames[i])
        message(paste("Documenting:", objnames[i]))
        curatedTCGAData::mae2rd(object = obj,
                                filename = mannames[i],
                                objname = objnames[i])
    }
}

make_documentation(rdsdir = rdsdir, mandir = mandir)

rdsdir <- "~/maeo"
mandir <- "~/git/curatedTCGAData/man"

make_documentation <- function(rdsdir, mandir){
library(curatedTCGAData)
fnames <- list.files(rdsdir, full.names=TRUE, pattern = "^.*MAEO\\.rds$")
objnames <- toupper(sub("MAEO.rds", "", basename(fnames), fixed=TRUE))
mannames <- file.path(mandir, paste0(objnames, ".Rd"))
for (i in seq_along(fnames)){
  obj <- readRDS(fnames[i])
  mae2rd(object = obj, filename = mannames[i], objname=objnames[i])
}
}
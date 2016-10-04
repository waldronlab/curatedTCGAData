## download.file("http://s3.amazonaws.com/multiassayexperiments/accMAEO.rds", destfile="accMAEO.rds")
## mae <- readRDS("accMAEO.rds")
## decompose(mae[, , 2:3])
#' Decompose a MultiAssayExperiment object and write the components to disk
#'
#' @param object An object of class MultiAssayExperiment
#' @param prepend A character string to pre-pend to the filename
#' @param directory The directory where files will be written to
#' @return A character vector of the filenames created
#' @examples
#' example("MultiAssayExperiment")
#' res <- decompose(myMultiAssayExperiment, directory = tempdir())
#' res
decompose <- function(object,
                      prepend = "myMAE_",
                      directory = ".") {
  pwd <- getwd()
  if (!dir.exists(directory))
    dir.create(directory)
  setwd(directory)
  fnames <- paste0(prepend, names(experiments(object)), ".rds")
  for (i in 1:length(experiments(object))) {
    message(paste0("Writing: ", fnames[i]))
    saveRDS(experiments(object)[[i]], file = fnames[i], compress = "bzip2")
  }
  fnames <-
    c(fnames,
      paste0(prepend, "pData.rds"),
      paste0(prepend, "sampleMap.rds"))
  saveRDS(pData(object),
          file = paste0(prepend, "pData.rds"),
          compress = "bzip2")
  saveRDS(sampleMap(object),
          file = paste0(prepend, "sampleMap.rds"),
          compress = "bzip2")
  if (!is.null(metadata(object))) {
    fnames <- c(fnames, paste0(prepend, "metadata.rds"))
    saveRDS(metadata(object),
            file = paste0(prepend, "metadata.rds"),
            compress = "bzip2")
  }
  setwd(pwd)
  return(fnames)
}

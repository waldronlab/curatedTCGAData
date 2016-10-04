#' Decompose a MultiAssayExperiment object and write the components to disk
#'
#' @param object An object of class MultiAssayExperiment
#' @param prepend A character string to pre-pend to the filename
#' @param directory The directory where files containing serialized objects will
#'   be written to
#' @return A character vector of the filenames created
#' @examples
#' example("MultiAssayExperiment")
#' res <- disassemble(myMultiAssayExperiment, directory = tempdir())
#' res
disassemble <- function(object,
                        prepend = "myMAE_",
                        directory = ".") {
    pwd <- getwd()
    if (!dir.exists(directory))
        dir.create(directory)
    setwd(directory)
    fnames <- paste0(prepend, names(experiments(object)), ".rda")
    for (i in 1:length(experiments(object))) {
        message(paste0("Writing: ", fnames[i]))
        save(experiments(object)[[i]],
                file = fnames[i],
                compress = "bzip2")
    }
    fnames <-
        c(fnames,
          paste0(prepend, "pData.rda"),
          paste0(prepend, "sampleMap.rda"))
    save(pData(object),
            file = paste0(prepend, "pData.rda"),
            compress = "bzip2")
    save(
        sampleMap(object),
        file = paste0(prepend, "sampleMap.rda"),
        compress = "bzip2"
    )
    if (!is.null(metadata(object))) {
        fnames <- c(fnames, paste0(prepend, "metadata.rda"))
        save(
            metadata(object),
            file = paste0(prepend, "metadata.rda"),
            compress = "bzip2"
        )
    }
    setwd(pwd)
    fnames <- file.path(directory, fnames)
    return(fnames)
}

#' Decompose a MultiAssayExperiment object and write the components to disk
#'
#' @param object An object of class MultiAssayExperiment
#' @param prepend A character string to pre-pend to the filename
#' @param directory The directory where files containing rda objects will
#'   be written to
#' @return A two-column matrix containing in its two columns 1) the filenames created and 2) the object names
#' @examples
#' example("MultiAssayExperiment")
#' res <- disassemble(myMultiAssayExperiment, directory = tempdir())
#' res
#' @export disassemble
disassemble <- function(object,
                        prepend = "myMAE_",
                        directory = ".") {
    if (!is(object, "MultiAssayExperiment"))
        stop("`object` must be an object of class MultiAssayExperiment")
    if (!is(prepend, "character"))
        stop("`prepend` must be a non-null character vector")
    if (!is(directory, "character"))
        stop("`directory` must be a non-null character vector")
    if (!dir.exists(directory))
        dir.create(directory)
    objnames <- paste0(prepend, names(experiments(object)))
    fnames <- file.path(directory, paste0(objnames, ".rda"))
    for (i in seq_along(experiments(object))) {
        message(paste0("Writing: ", fnames[i]))
        objname <- objnames[i]
        assign(x = objname, value = experiments(object)[[i]])
        save(list = objname,
             file = fnames[i],
             compress = "bzip2")
    }
    fname <- file.path(directory, paste0(prepend, "pData.rda"))
    fnames <- c(fnames, fname)
    objname <- paste0(prepend, "pData")
    objnames <- c(objnames, objname)
    assign(x = objname, value = pData(object))
    save(
        list = objname,
        file = fname,
        compress = "bzip2"
    )
    fname <- file.path(directory, paste0(prepend, "sampleMap.rda"))
    fnames <- c(fnames, fname)
    objname <- paste0(prepend, "sampleMap")
    objnames <- c(objnames, objname)
    assign(x = objname, value = sampleMap(object))
    save(
        list = objname,
        file = fname,
        compress = "bzip2"
    )
    if (!is.null(metadata(object))) {
        fname <- file.path(directory, paste0(prepend, "metadata.rda"))
        fnames <- c(fnames, fname)
        objname <- paste0(prepend, "metadata")
        objnames <- c(objnames, objname)
        assign(x = objname, value = metadata(object))
        save(
            list = objname,
            file = fname,
            compress = "bzip2"
        )
    }
    return(cbind(fnames, objnames))
}


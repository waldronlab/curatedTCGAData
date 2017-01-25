# Keep TCGA cancer code as upper case
dataFiles <- dir("data")

newFileNames <- vapply(strsplit(dataFiles, "_"), function(fileName) {
                           fileName <- paste0(toupper(fileName[[1]]), "_", fileName[[2]])
                           fileName
}, character(1L))

stopifnot(identical(tolower(dataFiles), tolower(newFileNames)))

file.rename(dataFiles, newFileNames)


## Upload files in folder
dataFiles <- list.files(path = dataDir, full.names = TRUE)

for (singleFile in dataFiles) {

AnnotationHubData:::upload_to_S3(file = singleFile,
                         remotename = basename(singleFile),
                         bucket = "experimenthub/curatedTCGAData/")
}


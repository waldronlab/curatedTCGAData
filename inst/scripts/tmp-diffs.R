## Compare AWS to on disk
aws_files <- system("aws s3 ls s3://experimenthub/curatedTCGAData/",
                    intern = TRUE)
aws_files <- aws_files[-1]
aws_files <- sapply(strsplit(aws_files, " +"), `[`, 4)

on_disk <- dir("data")

NotUploaded <- setdiff(aws_files, on_disk)

saveRDS(NotUploaded, file = "missingDataOut.Rds")

## Download
aws_diff_dl <-
    paste0("aws s3 cp s3://experimenthub/curatedTCGAData/",
           NotUploaded, " data/", NotUploaded)

sapply(aws_diff_dl, system)


make_upload <- function() {
    create_dir("./exec")
    append <- file.exists("./exec/upload2AWS.sh")
    aws_prefix <- "aws s3 cp ../data/"
    aws_suffix <- " s3://experimenthub/curatedTCGAData/ --acl public-read"
    dir("./data") %>%
    paste(aws_prefix, ., aws_suffix, sep = "") %>%
    cat(., file = "./exec/upload2AWS.sh", sep = "\n", append = append)
}

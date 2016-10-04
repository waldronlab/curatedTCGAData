make_data <- function(rds_file) {
    create_dir("./data")
    prepend <- basename(rds_file) %>% gsub("MAEO.rds", "", .) %>% paste0(., "_")
    readRDS(rds_file) %>%
    disassemble(., prepend = prepend, directory = "./data")
    invisible(NULL)
}

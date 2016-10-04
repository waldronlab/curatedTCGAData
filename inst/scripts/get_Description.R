get_Description <- function(resource_name) {
    gsub(".rda", "", resource_name) %>%
    strsplit(resource_name, "_") %>%
    unlist() %>%
    paste(.[2], "data specific to the", .[1], "cohort of the TCGA project")
}

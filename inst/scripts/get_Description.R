get_Description <- function(resource_name) {
    gsub(".rda", "", resource_name) %>%
    strsplit(., "_") %>%
    unlist() %>%
    {paste(.[2], "data specific to the", toupper(.[1]),
           "cohort of the TCGA project")}
}

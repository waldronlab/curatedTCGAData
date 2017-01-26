name_helper <- function(maeo_name, maeo_dir) {
    paste0("./", maeo_dir, "/", maeo_name) %>%
        readRDS() ->
        maeo_obj

    toupper(maeo_name) %>%
        gsub("MAEO.RDS", ".Rd", .) %>%
        paste0("./man/", .) ->
        rd_file

    toupper(maeo_name) %>%
        gsub("MAEO.RDS", "", .) ->
        cohort_name

    mae2rd(maeo_obj, rd_file, cohort_name)
}

make_documentation <- function(maeo_dir) {
    dir(maeo_dir) %>%
    lapply(name_helper, maeo_dir)
}

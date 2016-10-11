#' @keywords internal
source_scripts <- function() {
    list.files("./inst/scripts",
               full.names = TRUE) %>%
    lapply(., source) %>% invisible()
    invisible(NULL)
}

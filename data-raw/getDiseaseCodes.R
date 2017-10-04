## Extract cancer codes from TCGA project
library(rvest)
ccTable <- "https://gdc.cancer.gov/resources-tcga-users/tcga-code-tables/tcga-study-abbreviations"
htcc <- read_html(ccTable)
diseaseCodes <- html_table(htcc, fill = TRUE)[[2L]]
names(diseaseCodes) <- make.names(colnames(diseaseCodes))

excludedCodes <- c("COADREAD", "GBMLGG", "KIPAN", "STES", "FPPP", "CNTL",
                   "LCML", "MISC")
logicalSub <- !diseaseCodes[["Study.Abbreviation"]] %in% excludedCodes
diseaseCodes[["Available"]] <- factor(logicalSub,  levels = c("TRUE", "FALSE"),
    labels = c("Yes", "No"))

diseaseCodes <- diseaseCodes[order(diseaseCodes[["Study.Abbreviation"]]), ]
devtools::use_data(diseaseCodes, internal = TRUE, overwrite = TRUE)

## For easy subsetting use:
diseaseCodes[["Study.Abbreviation"]][diseaseCodes$Available == "Yes"]

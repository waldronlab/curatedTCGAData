.cMAE <- function(mae, x, name = "newelement") {
  el <- ExperimentList(tmp = x)
  names(el)[1] <- name
  c(mae, el)
}

.hasMir <- function(x) {
  mean(c(FALSE, grepl("^hsa", rownames(x))), na.rm = TRUE) > 0.9
}
.hasSymbols <- function(x) {
  mean(c(
    FALSE,
    grepl("^[A-Z0-9]{1,6}|^C[0-9]orf[0-9]{1,4}", rownames(x))
  ), na.rm = TRUE) > 0.9
}
.isSummarizedExperiment <- function(x) {
  is(x, "SummarizedExperiment") & !is(x, "RangedSummarizedExperiment")
}

.makeListRanges <- function(x, gn) {
  res <- list(unmapped = x[!x %in% names(gn)])
  x <- x[x %in% names(gn)]
  gn <- gn[match(x, names(gn))]
  res$mapped <- gn
  return(res)
}

.getRangesOfSYMBOLS <- function(x) {
  suppressPackageStartupMessages({
    library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    library(org.Hs.eg.db)
    library(GenomeInfoDb)
  })
  entrez <-
    mapIds(org.Hs.eg.db, x, keytype = "SYMBOL", column = "ENTREZID")
  gn <- genes(TxDb.Hsapiens.UCSC.hg19.knownGene)
  names(gn) <-
    mapIds(org.Hs.eg.db,
           names(gn),
           keytype = "ENTREZID",
           column = "SYMBOL")
  gn <- keepStandardChromosomes(granges(gn), pruning.mode = "coarse")
  seqlevelsStyle(gn) <- "NCBI"
  ## returns a list of length 2: "unmapped" is a character vector providing
  ## unmapped symbols, "mapped" is a GRanges object with ranges of mapped symbols.
  return(.makeListRanges(x, gn))
}

.getRangesOfMir <- function(x) {
  suppressPackageStartupMessages({
    library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    library(mirbase.db)
  })
  mr <- microRNAs(TxDb.Hsapiens.UCSC.hg19.knownGene)
  names(mr) <- mr$mirna_id
  mr <- keepStandardChromosomes(granges(mr), pruning.mode = "coarse")
  seqlevelsStyle(mr) <- "NCBI"
  return(.makeListRanges(x, mr))
}

#' Simplify curatedTCGAData objects by replacing RaggedExperiment objects
#'
#' @param obj A MultiAssayExperiment object obtained from curatedTCGAData
#' @param removeRaggedExperiments if TRUE (default), remove RaggedExperiment objects
#' from the returned MultiAssayExperiment
#' @return
#' A MultiAssayExperiment object with RaggedExperiments converted to RangedSummarizedExperiment
#' with rows corresponding to gene symbole.
#'
#' "Mutations" objects become a genes x patients RangedSummarizedExperiment containing 1 if
#' there is a non-silent mutation somewhere in the gene, and 0 otherwise.
#' "CNA" and "CNV" segmented copy number are reduced using a weighted mean in the rare
#' cases of overlapping (non-disjoint) copy number regions.
#' @export simplifyTCGAData
#' @details
#' Relies on TxDb.Hsapiens.UCSC.hg19.knownGene and org.Hs.eg.db to map to hg19 NCBI build.
#' @examples
#' accmae <- curatedTCGAData("ACC", c("CNASNP", "Mutation"), dry.run = FALSE)
#' simplifyTCGAData(accmae)
simplifyTCGAData <- function(obj, removeRaggedExperiments = TRUE) {
  suppressPackageStartupMessages({
    library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    library(org.Hs.eg.db)
    library(GenomeInfoDb)
  })
  gn <- genes(TxDb.Hsapiens.UCSC.hg19.knownGene)
  gn <- keepStandardChromosomes(granges(gn), pruning.mode = "coarse")
  seqlevelsStyle(gn) <- "NCBI"
  names(gn) <-
    mapIds(org.Hs.eg.db,
           names(gn),
           keytype = "ENTREZID",
           column = "SYMBOL")
  ##
  weightedmean <- function(scores, ranges, qranges)
    ## weighted average score per query range
    sum(scores * width(ranges)) / sum(width(ranges))
  ##
  nonsilent <- function(scores, ranges, qranges)
    any(scores != "Silent")
  ##
  isRE <-
    function(x)
      vapply(experiments(x), function(y)
        is(y, "RaggedExperiment"), TRUE)
  ##
  isMut <- function(x)
    grepl("Mutation", names(x))
  ##
  for (i in which(isMut(obj))) {
    mutations <-
      qreduceAssay(obj[[i]], gn, nonsilent, "Variant_Classification")
    rownames(mutations) <- names(gn)
    mutations[is.na(mutations)] <- 0
    remove.rows <- is.na(rownames(mutations))
    mutations <-
      SummarizedExperiment(mutations[!remove.rows,], rowRanges = gn[!remove.rows])
    el <- ExperimentList(x = mutations)
    names(el) <- paste0(names(obj)[i], "_simplified")
    obj <- c(obj, el)
    rm(el, mutations)
  }
  for (i in which(isRE(obj) & !isMut(obj))) {
    suppressWarnings(cn <-
                       qreduceAssay(obj[[i]], gn, weightedmean, "Segment_Mean"))
    rownames(cn) <- names(gn)
    remove.rows <- is.na(rownames(cn))
    cn <-
      SummarizedExperiment(cn[!remove.rows,], rowRanges = gn[!remove.rows])
    el <- ExperimentList(x = cn)
    names(el) <- paste0(names(obj)[i], "_simplified")
    obj <- c(obj, el)
  }
  if (removeRaggedExperiments) {
    obj <- obj[, ,!isRE(obj)]
  }
  return(obj)
}

#' Convert SummarizedExperiment elements with gene symbols to RangedSummarizedExperiment
#'
#' @param obj A MultiAssayExperiment object obtained from curatedTCGAData
#' @param removeOriginals If TRUE (default), remove the SummarizedExperiment objects
#' that have been converted to RangedSummarizedExperiment
#'
#' @return a MultiAssayExperiment where any of the original SummarizedExperiment containing
#' gene symbols as rownames have been replaced or supplemented by a RangedSummarizedExperiment
#' for miR that could be mapped to GRanges, and another SummarizedExperiment for miR that
#' could not be mapped to GRanges.
#' @export symbolsToRanges
#' @seealso mirToRanges
#' @details   Any SummarizedExperiment elements with gene symbols as rownames will have ranges added.
#' Symbols where ranges can't be found are put in a new SummarizedExperiment.
#' @examples
#' symbolsToRanges(miniACC)
symbolsToRanges <- function(obj, removeOriginals = TRUE) {
  can.fix <- vapply(experiments(obj), function(y) {
    .hasSymbols(y) & .isSummarizedExperiment(y)
  }, TRUE)
  ##
  for (i in which(can.fix)) {
    lookup <- .getRangesOfSYMBOLS(rownames(obj[[i]]))
    rse <- obj[[i]][names(lookup$mapped),]
    rowRanges(rse) <- lookup$mapped
    obj <- .cMAE(obj, rse, name = paste0(names(obj)[i], "_ranged"))
    if (length(lookup$unmapped > 0)) {
      se <- obj[[i]][lookup$unmapped,]
      obj <- .cMAE(obj, se, name = paste0(names(obj)[i], "_unranged"))
    }
  }
  if (removeOriginals & any(can.fix))
    obj <- obj[, ,-which(can.fix)]
  return(obj)
}

#' Convert SummarizedExperiment elements with microRNA to RangedSummarizedExperiment
#'
#' @param obj A MultiAssayExperiment object obtained from curatedTCGAData
#' @param removeOriginals If TRUE (default), remove the SummarizedExperiment objects
#' that have been converted to RangedSummarizedExperiment
#' @return a MultiAssayExperiment where any of the original SummarizedExperiment containing
#' gene symbols as rownames have been replaced or supplemented by a RangedSummarizedExperiment
#' for miR that could be mapped to GRanges, and another SummarizedExperiment for miR that
#' could not be mapped to GRanges.
#' @export mirToRanges
#' @seealso symbolsToRanges
#' @examples
#' accmae <- curatedTCGAData("ACC", "miRNASeqGene", dry.run = FALSE)
#' mirToRanges(accmae)
mirToRanges <- function(obj, removeOriginals = TRUE) {
  can.fix <- vapply(experiments(obj), function(y) {
    .hasMir(y) & .isSummarizedExperiment(y)
  }, TRUE)
  ##
  for (i in which(can.fix)) {
    lookup <- .getRangesOfMir(rownames(obj[[i]]))
    rse <- obj[[i]][names(lookup$mapped), ]
    rowRanges(rse) <- lookup$mapped
    obj <- .cMAE(obj, rse, paste0(names(obj)[i], "_ranged"))
    if (length(lookup$unmapped > 0)) {
      se <- obj[[i]][lookup$unmapped, ]
      obj <- .cMAE(obj, se, paste0(names(obj)[i], "_unranged"))
    }
    if (removeOriginals & any(can.fix))
      obj <- obj[, , -which(can.fix)]
  }
  return(obj)
}

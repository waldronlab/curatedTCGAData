\name{COAD-v2.1.0}
\alias{ COAD-v2.1.0 }
\docType{data}
\title{ Colon adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( COAD )
ExperimentList class object of length 17:
 [1] COAD_CNASeq-20160128: RaggedExperiment with 40530 rows and 136 columns
 [2] COAD_CNASNP-20160128: RaggedExperiment with 459490 rows and 914 columns
 [3] COAD_CNVSNP-20160128: RaggedExperiment with 90534 rows and 914 columns
 [4] COAD_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 449 columns
 [5] COAD_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 66 rows and 449 columns
 [6] COAD_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 449 columns
 [7] COAD_mRNAArray-20160128: SummarizedExperiment with 17814 rows and 172 columns
 [8] COAD_Mutation-20160128: RaggedExperiment with 62530 rows and 154 columns
 [9] COAD_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 326 columns
 [10] COAD_RNASeq2GeneNorm_illuminaga-20160128: SummarizedExperiment with 20501 rows and 191 columns
 [11] COAD_RNASeq2GeneNorm_illuminahiseq-20160128: SummarizedExperiment with 20501 rows and 326 columns
 [12] COAD_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 10 columns
 [13] COAD_RPPAArray-20160128: SummarizedExperiment with 208 rows and 360 columns
 [14] COAD_miRNASeqGene-20160128: SummarizedExperiment with 705 rows and 221 columns
 [15] COAD_RNASeq2GeneNorm-20160128: SummarizedExperiment with 18025 rows and 498 columns
 [16] COAD_Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 202 columns
 [17] COAD_Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 333 columns

> rownames( COAD )
CharacterList of length 17
[["COAD_CNASeq-20160128"]] character(0)
[["COAD_CNASNP-20160128"]] character(0)
[["COAD_CNVSNP-20160128"]] character(0)
[["COAD_GISTIC_AllByGene-20160128"]] character(0)
[["COAD_GISTIC_Peaks-20160128"]] 23 24 25 26 27 1 28 ... 18 19 20 21 22 65 66
[["COAD_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["COAD_mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 PNMA1 ... SNRPD2 AQP7 CTSC
[["COAD_Mutation-20160128"]] character(0)
[["COAD_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["COAD_RNASeq2GeneNorm_illuminaga-20160128"]] A1BG A1CF ... psiTPTE22 tAKR
...
<7 more elements>

> colnames( COAD )
CharacterList of length 17
[["COAD_CNASeq-20160128"]] TCGA-A6-2671-01A-01D-1405-02 ...
[["COAD_CNASNP-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["COAD_CNVSNP-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["COAD_GISTIC_AllByGene-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["COAD_GISTIC_Peaks-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["COAD_GISTIC_ThresholdedByGene-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["COAD_mRNAArray-20160128"]] TCGA-A6-2671-11A-01R-1758-07 ...
[["COAD_Mutation-20160128"]] TCGA-A6-2672-01A-01W-0833-10 ...
[["COAD_RNASeq2Gene-20160128"]] TCGA-3L-AA1B-01A-11R-A37K-07 ...
[["COAD_RNASeq2GeneNorm_illuminaga-20160128"]] TCGA-A6-2671-01A-01R-1410-07...
...
<7 more elements>

Sizes of each ExperimentList element:

                                         assay size.Mb
1                         COAD_CNASeq-20160128  1.1 Mb
2                         COAD_CNASNP-20160128 12.5 Mb
3                         COAD_CNVSNP-20160128  2.7 Mb
4               COAD_GISTIC_AllByGene-20160128 88.5 Mb
5                   COAD_GISTIC_Peaks-20160128  0.4 Mb
6       COAD_GISTIC_ThresholdedByGene-20160128 88.3 Mb
7                      COAD_mRNAArray-20160128 25.6 Mb
8                       COAD_Mutation-20160128 23.6 Mb
9                    COAD_RNASeq2Gene-20160128 53.6 Mb
10    COAD_RNASeq2GeneNorm_illuminaga-20160128 32.5 Mb
11 COAD_RNASeq2GeneNorm_illuminahiseq-20160128 53.6 Mb
12                    COAD_RNASeqGene-20160128  4.1 Mb
13                     COAD_RPPAArray-20160128  0.7 Mb
14                  COAD_miRNASeqGene-20160128  1.3 Mb
15               COAD_RNASeq2GeneNorm-20160128 70.8 Mb
16          COAD_Methylation_methyl27-20160128  4.9 Mb
17         COAD_Methylation_methyl450-20160128   75 Mb


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   58.00   68.00   66.95   77.00   90.00       2 

tumor_tissue_site:
colon  NA's 
  456     1 





date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1998    2007    2009    2008    2010    2013 

radiation_therapy:
  no  yes NA's 
 378    9   70 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.058   2.000  50.000      25 

race:
american indian or alaska native                            asian 
                               1                               11 
       black or african american                            white 
                              59                              213 
                            NA's 
                             173 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     4                    270                    183 

Including an additional 2604 columns
}}
\keyword{datasets}

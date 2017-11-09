\name{ COAD }
\alias{ COAD }
\docType{data}
\title{ Colon adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( COAD )
ExperimentList class object of length 13: 
 [1] CNASeq-20160128: RaggedExperiment with 40530 rows and 136 columns 
 [2] CNASNP-20160128: RaggedExperiment with 459490 rows and 914 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 90534 rows and 914 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 449 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 449 columns 
 [6] Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 202 columns 
 [7] Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 333 columns 
 [8] miRNASeqGene-20160128: SummarizedExperiment with 705 rows and 221 columns 
 [9] mRNAArray-20160128: SummarizedExperiment with 17814 rows and 172 columns 
 [10] Mutation-20160128: RaggedExperiment with 62530 rows and 154 columns 
 [11] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 191 columns 
 [12] RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 10 columns 
 [13] RPPAArray-20160128: SummarizedExperiment with 208 rows and 360 columns 

> rownames( COAD )
CharacterList of length 13
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation_methyl27-20160128"]] cg00000292 cg00002426 ... cg27665659
[["Methylation_methyl450-20160128"]] cg00000029 cg00000108 ... rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 PNMA1 ... SLC39A6 SNRPD2 AQP7 CTSC
[["Mutation-20160128"]] character(0)
...
<3 more elements>

> colnames( COAD )
CharacterList of length 13
[["CNASeq-20160128"]] TCGA-A6-2671-01A-01D-1405-02 ...
[["CNASNP-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["CNVSNP-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["Methylation_methyl27-20160128"]] TCGA-A6-2672-11A-01D-1551-05 ...
[["Methylation_methyl450-20160128"]] TCGA-3L-AA1B-01A-11D-A36Y-05 ...
[["miRNASeqGene-20160128"]] TCGA-A6-2675-01A-02T-1722-13 ...
[["mRNAArray-20160128"]] TCGA-A6-2671-11A-01R-1758-07 ...
[["Mutation-20160128"]] TCGA-A6-2672-01A-01W-0833-10 ...
...
<3 more elements>

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128  1.1 Mb
2                    CNASNP-20160128 12.5 Mb
3                    CNVSNP-20160128  2.7 Mb
4          GISTIC_AllByGene-20160128  4.9 Mb
5  GISTIC_ThresholdedByGene-20160128  4.9 Mb
6      Methylation_methyl27-20160128  4.9 Mb
7     Methylation_methyl450-20160128   75 Mb
8              miRNASeqGene-20160128  0.1 Mb
9                 mRNAArray-20160128  1.1 Mb
10                 Mutation-20160128 23.6 Mb
11          RNASeq2GeneNorm-20160128  1.3 Mb
12               RNASeqGene-20160128  1.3 Mb
13                RPPAArray-20160128    0 Mb


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

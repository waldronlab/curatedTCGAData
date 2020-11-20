\name{UCEC-v2.0.0}
\alias{ UCEC-v2.0.0 }
\docType{data}
\title{ Uterine Corpus Endometrial Carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( UCEC )
ExperimentList class object of length 16:
 [1] UCEC_CNASeq-20160128: RaggedExperiment with 36400 rows and 213 columns
 [2] UCEC_CNASNP-20160128: RaggedExperiment with 619412 rows and 1083 columns
 [3] UCEC_CNVSNP-20160128: RaggedExperiment with 127094 rows and 1078 columns
 [4] UCEC_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 539 columns
 [5] UCEC_GISTIC_Peaks-20160128: SummarizedExperiment with 202 rows and 539 columns
 [6] UCEC_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 539 columns
 [7] UCEC_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 433 columns
 [8] UCEC_mRNAArray-20160128: SummarizedExperiment with 17814 rows and 54 columns
 [9] UCEC_Mutation-20160128: RaggedExperiment with 184861 rows and 248 columns
 [10] UCEC_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 190 columns
 [11] UCEC_RNASeq2GeneNorm_illuminaga-20160128: SummarizedExperiment with 20501 rows and 380 columns
 [12] UCEC_RNASeq2GeneNorm_illuminahiseq-20160128: SummarizedExperiment with 20501 rows and 190 columns
 [13] UCEC_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 269 columns
 [14] UCEC_RPPAArray-20160128: SummarizedExperiment with 208 rows and 440 columns
 [15] UCEC_Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 118 columns
 [16] UCEC_Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 466 columns

> rownames( UCEC )
CharacterList of length 16
[["UCEC_CNASeq-20160128"]] character(0)
[["UCEC_CNASNP-20160128"]] character(0)
[["UCEC_CNVSNP-20160128"]] character(0)
[["UCEC_GISTIC_AllByGene-20160128"]] ACAP3 ...
[["UCEC_GISTIC_Peaks-20160128"]] character(0)
[["UCEC_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["UCEC_miRNASeqGene-20160128"]] hsa-let-7a-1 ... hsa-mir-99b
[["UCEC_mRNAArray-20160128"]] ELMO2 CREB3L1 ... AQP7 CTSC
[["UCEC_Mutation-20160128"]] character(0)
[["UCEC_RNASeq2Gene-20160128"]] A1BG A1CF ... psiTPTE22 tAKR
...
<6 more elements>

> colnames( UCEC )
CharacterList of length 16
[["UCEC_CNASeq-20160128"]] TCGA-A5-A0G5-01A-11D-A043-02 ...
[["UCEC_CNASNP-20160128"]] TCGA-2E-A9G8-01A-11D-A402-01 ...
[["UCEC_CNVSNP-20160128"]] TCGA-2E-A9G8-01A-11D-A402-01 ...
[["UCEC_GISTIC_AllByGene-20160128"]] TCGA-2E-A9G8-01A-11D-A...
[["UCEC_GISTIC_Peaks-20160128"]] TCGA-2E-A9G8-01A-11D-A402-...
[["UCEC_GISTIC_ThresholdedByGene-20160128"]] TCGA-2E-A9G8-0...
[["UCEC_miRNASeqGene-20160128"]] TCGA-2E-A9G8-01A-11R-A404-...
[["UCEC_mRNAArray-20160128"]] TCGA-A5-A0G2-01A-11R-A040-07 ...
[["UCEC_Mutation-20160128"]] TCGA-A5-A0G3-01A-11W-A062-09 ...
[["UCEC_RNASeq2Gene-20160128"]] TCGA-2E-A9G8-01A-11R-A40A-0...
...
<6 more elements>

Sizes of each ExperimentList element:

                                         assay  size.Mb
1                         UCEC_CNASeq-20160128     1 Mb
2                         UCEC_CNASNP-20160128  16.9 Mb
3                         UCEC_CNVSNP-20160128   3.7 Mb
4               UCEC_GISTIC_AllByGene-20160128 106.9 Mb
5                   UCEC_GISTIC_Peaks-20160128     1 Mb
6       UCEC_GISTIC_ThresholdedByGene-20160128 106.9 Mb
7                   UCEC_miRNASeqGene-20160128   3.7 Mb
8                      UCEC_mRNAArray-20160128   9.6 Mb
9                       UCEC_Mutation-20160128  73.1 Mb
10                   UCEC_RNASeq2Gene-20160128  32.3 Mb
11    UCEC_RNASeq2GeneNorm_illuminaga-20160128    62 Mb
12 UCEC_RNASeq2GeneNorm_illuminahiseq-20160128  32.3 Mb
13                    UCEC_RNASeqGene-20160128  44.7 Mb
14                     UCEC_RPPAArray-20160128   0.8 Mb
15          UCEC_Methylation_methyl27-20160128   4.9 Mb
16         UCEC_Methylation_methyl450-20160128  75.1 Mb


---------------------------
Available sample meta-data:
---------------------------

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   367.0   709.0   881.8  1063.0  3423.0     457 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -6.0   543.0   948.5  1195.1  1753.2  6859.0      92 

tumor_tissue_site:
   endometrial other  specify 
           547              1 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1995    2007    2009    2009    2010    2013       9 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    145     404     729    1405    2096    4144     535 

radiation_therapy:
  no  yes NA's 
 295  228   25 

histological_type:
endometrioid endometrial adenocarcinoma 
                                    411 
          mixed serous and endometrioid 
                                     22 
      serous endometrial adenocarcinoma 
                                    115 

residual_tumor:
  r0   r1   r2   rx NA's 
 376   22   16   41   93 

Including an additional 1779 columns
}}
\keyword{datasets}

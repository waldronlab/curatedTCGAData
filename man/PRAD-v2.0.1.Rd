\name{PRAD-v2.0.1}
\alias{ PRAD-v2.0.1 }
\docType{data}
\title{ Prostate adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( PRAD )
ExperimentList class object of length 12:
 [1] PRAD_CNASeq-20160128: RaggedExperiment with 23398 rows and 230 columns
 [2] PRAD_CNASNP-20160128: RaggedExperiment with 573776 rows and 1029 columns
 [3] PRAD_CNVSNP-20160128: RaggedExperiment with 117345 rows and 1023 columns
 [4] PRAD_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 492 columns
 [5] PRAD_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 58 rows and 492 columns
 [6] PRAD_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 492 columns
 [7] PRAD_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 547 columns
 [8] PRAD_Mutation-20160128: RaggedExperiment with 12348 rows and 332 columns
 [9] PRAD_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 550 columns
 [10] PRAD_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 550 columns
 [11] PRAD_RPPAArray-20160128: SummarizedExperiment with 195 rows and 352 columns
 [12] PRAD_Methylation-20160128: SummarizedExperiment with 485577 rows and 549 columns

> rownames( PRAD )
CharacterList of length 12
[["PRAD_CNASeq-20160128"]] character(0)
[["PRAD_CNASNP-20160128"]] character(0)
[["PRAD_CNVSNP-20160128"]] character(0)
[["PRAD_GISTIC_AllByGene-20160128"]] character(0)
[["PRAD_GISTIC_Peaks-20160128"]] 29 30 1 31 32 33 34 ... 21 60 61 22 23 62 63
[["PRAD_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["PRAD_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["PRAD_Mutation-20160128"]] character(0)
[["PRAD_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["PRAD_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
...
<2 more elements>

> colnames( PRAD )
CharacterList of length 12
[["PRAD_CNASeq-20160128"]] TCGA-CH-5741-01A-11D-1572-02 ...
[["PRAD_CNASNP-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["PRAD_CNVSNP-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["PRAD_GISTIC_AllByGene-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["PRAD_GISTIC_Peaks-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["PRAD_GISTIC_ThresholdedByGene-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["PRAD_miRNASeqGene-20160128"]] TCGA-2A-A8VL-01A-21R-A37H-13 ...
[["PRAD_Mutation-20160128"]] TCGA-2A-A8VL-01A-21D-A377-08 ...
[["PRAD_RNASeq2Gene-20160128"]] TCGA-2A-A8VL-01A-21R-A37L-07 ...
[["PRAD_RNASeq2GeneNorm-20160128"]] TCGA-2A-A8VL-01A-21R-A37L-07 ...
...
<2 more elements>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    PRAD_CNASeq-20160128  0.7 Mb
2                    PRAD_CNASNP-20160128 15.6 Mb
3                    PRAD_CNVSNP-20160128  3.4 Mb
4          PRAD_GISTIC_AllByGene-20160128 96.6 Mb
5              PRAD_GISTIC_Peaks-20160128  0.4 Mb
6  PRAD_GISTIC_ThresholdedByGene-20160128 96.4 Mb
7              PRAD_miRNASeqGene-20160128  4.6 Mb
8                  PRAD_Mutation-20160128 21.5 Mb
9               PRAD_RNASeq2Gene-20160128 88.7 Mb
10          PRAD_RNASeq2GeneNorm-20160128 88.7 Mb
11                PRAD_RPPAArray-20160128  0.6 Mb
12              PRAD_Methylation-20160128 75.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   488 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  10.00   10.00    3.02    1.99      NA 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  41.00   56.00   61.00   61.02   66.00   78.00      11 

vital_status:
  0   1 
488  10 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  146.0   743.8  1102.0  1579.0  2315.5  3502.0     488 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   23.0   523.5   926.0  1076.5  1458.0  5024.0      10 

tumor_tissue_site:
prostate 
     498 


pathology_N_stage:
  n0   n1 NA's 
 346   79   73 

gender:
male 
 498 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2000    2009    2011    2010    2012    2013      31 

radiation_therapy:
  no  yes NA's 
 395   59   44 

histological_type:
   prostate adenocarcinoma acinar type prostate adenocarcinoma, other subtype 
                                   483                                     15 

residual_tumor:
  r0   r1   r2   rx NA's 
 316  147    5   15   15 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4447  0.0000 15.0000      91 

gleason_score:
  6   7   8   9  10 
 45 248  64 137   4 

psa_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.030   0.100   1.742   0.110 323.000      57 

days_to_psa:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -164.0   191.0   512.0   685.6   926.0  3447.0      53 

race:
                    asian black or african american                     white 
                        2                         7                       147 
                     NA's 
                      342 

ethnicity:
not hispanic or latino                   NA's 
                   152                    346 

Including an additional 1126 columns
}}
\keyword{datasets}

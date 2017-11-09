\name{ PRAD }
\alias{ PRAD }
\docType{data}
\title{ Prostate adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( PRAD )
ExperimentList class object of length 10: 
 [1] CNASeq-20160128: RaggedExperiment with 23398 rows and 230 columns 
 [2] CNASNP-20160128: RaggedExperiment with 573776 rows and 1029 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 117345 rows and 1023 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 492 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 492 columns 
 [6] Methylation-20160128: SummarizedExperiment with 485577 rows and 549 columns 
 [7] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 547 columns 
 [8] Mutation-20160128: RaggedExperiment with 12348 rows and 332 columns 
 [9] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 550 columns 
 [10] RPPAArray-20160128: SummarizedExperiment with 195 rows and 352 columns 

> rownames( PRAD )
CharacterList of length 10
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363

> colnames( PRAD )
CharacterList of length 10
[["CNASeq-20160128"]] TCGA-CH-5741-01A-11D-1572-02 ...
[["CNASNP-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["CNVSNP-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["Methylation-20160128"]] TCGA-2A-A8VL-01A-21D-A378-05 ...
[["miRNASeqGene-20160128"]] TCGA-2A-A8VL-01A-21R-A37H-13 ...
[["Mutation-20160128"]] TCGA-2A-A8VL-01A-21D-A377-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-2A-A8VL-01A-21R-A37L-07 ...
[["RPPAArray-20160128"]] TCGA-2A-A8VL-01A-11-A43M-20 ...

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128  0.7 Mb
2                    CNASNP-20160128 15.6 Mb
3                    CNVSNP-20160128  3.4 Mb
4          GISTIC_AllByGene-20160128  4.9 Mb
5  GISTIC_ThresholdedByGene-20160128  4.9 Mb
6               Methylation-20160128 75.1 Mb
7              miRNASeqGene-20160128  0.1 Mb
8                  Mutation-20160128 21.5 Mb
9           RNASeq2GeneNorm-20160128  1.3 Mb
10                RPPAArray-20160128    0 Mb

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

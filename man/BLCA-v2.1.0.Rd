\name{BLCA-v2.1.0}
\alias{ BLCA-v2.1.0 }
\docType{data}
\title{ Bladder Urothelial Carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( BLCA )
ExperimentList class object of length 13:
 [1] BLCA_CNASeq-20160128: RaggedExperiment with 35692 rows and 232 columns
 [2] BLCA_CNASNP-20160128: RaggedExperiment with 479978 rows and 806 columns
 [3] BLCA_CNVSNP-20160128: RaggedExperiment with 104349 rows and 797 columns
 [4] BLCA_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 408 columns
 [5] BLCA_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 71 rows and 408 columns
 [6] BLCA_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 408 columns
 [7] BLCA_Mutation-20160128: RaggedExperiment with 39312 rows and 130 columns
 [8] BLCA_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 427 columns
 [9] BLCA_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 67 columns
 [10] BLCA_RPPAArray-20160128: SummarizedExperiment with 208 rows and 344 columns
 [11] BLCA_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 429 columns
 [12] BLCA_RNASeq2GeneNorm-20160128: SummarizedExperiment with 18205 rows and 427 columns
 [13] BLCA_Methylation-20160128: SummarizedExperiment with 485577 rows and 434 columns

> rownames( BLCA )
CharacterList of length 13
[["BLCA_CNASeq-20160128"]] character(0)
[["BLCA_CNASNP-20160128"]] character(0)
[["BLCA_CNVSNP-20160128"]] character(0)
[["BLCA_GISTIC_AllByGene-20160128"]] character(0)
[["BLCA_GISTIC_Peaks-20160128"]] 38 1 2 3 39 4 40 41 ... 69 70 34 35 36 37 71
[["BLCA_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["BLCA_Mutation-20160128"]] character(0)
[["BLCA_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["BLCA_RNASeqGene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["BLCA_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
...
<3 more elements>

> colnames( BLCA )
CharacterList of length 13
[["BLCA_CNASeq-20160128"]] TCGA-BL-A0C8-01A-11D-A10R-02 ...
[["BLCA_CNASNP-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["BLCA_CNVSNP-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["BLCA_GISTIC_AllByGene-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["BLCA_GISTIC_Peaks-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["BLCA_GISTIC_ThresholdedByGene-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["BLCA_Mutation-20160128"]] TCGA-BL-A0C8-01A-11D-A10S-08 ...
[["BLCA_RNASeq2Gene-20160128"]] TCGA-2F-A9KO-01A-11R-A38B-07 ...
[["BLCA_RNASeqGene-20160128"]] TCGA-BL-A0C8-01A-11R-A10U-07 ...
[["BLCA_RPPAArray-20160128"]] TCGA-2F-A9KO-01A-21-A459-20 ...
...
<3 more elements>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    BLCA_CNASeq-20160128    1 Mb
2                    BLCA_CNASNP-20160128 13.1 Mb
3                    BLCA_CNVSNP-20160128    3 Mb
4          BLCA_GISTIC_AllByGene-20160128 80.7 Mb
5              BLCA_GISTIC_Peaks-20160128  0.4 Mb
6  BLCA_GISTIC_ThresholdedByGene-20160128 80.5 Mb
7                  BLCA_Mutation-20160128 56.1 Mb
8               BLCA_RNASeq2Gene-20160128 69.4 Mb
9                BLCA_RNASeqGene-20160128   13 Mb
10                BLCA_RPPAArray-20160128  0.6 Mb
11             BLCA_miRNASeqGene-20160128  3.6 Mb
12          BLCA_RNASeq2GeneNorm-20160128 61.6 Mb
13              BLCA_Methylation-20160128 75.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   232 observations deleted due to missingness 
       n events median 0.95LCL 0.95UCL
[1,] 180    180   1.12   0.992    1.33


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  34.00   60.00   69.00   68.08   76.00   90.00       1 

vital_status:
  0   1 
230 182 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   19.0   235.8   410.5   552.8   654.5  3183.0     232 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -64.0   398.2   639.0  1016.7  1458.8  5050.0     182 

tumor_tissue_site:
bladder 
    412 

pathologic_stage:
  stage i  stage ii stage iii  stage iv      NA's 
        2       131       141       136         2 



pathology_M_stage:
  m0   m1   mx NA's 
 196   11  202    3 

gender:
female   male 
   108    304 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1999    2009    2011    2010    2012    2013      18 

radiation_therapy:
  no  yes NA's 
 366   20   26 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   80.00   90.00   83.09   90.00  100.00     276 

histological_type:
muscle invasive urothelial carcinoma (pt2 or above) 
                                                409 
                                               NA's 
                                                  3 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.15   20.00   30.00   39.04   50.00  730.00     188 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.088   2.000  97.000     115 

race:
                    asian black or african american                     white 
                       44                        23                       327 
                     NA's 
                       18 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     9                    371                     32 

Including an additional 1695 columns
}}
\keyword{datasets}

\name{KICH-v2.1.0}
\alias{ KICH-v2.1.0 }
\docType{data}
\title{ Kidney Chromophobe }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( KICH )
ExperimentList class object of length 11:
 [1] KICH_CNASNP-20160128: RaggedExperiment with 57729 rows and 132 columns
 [2] KICH_CNVSNP-20160128: RaggedExperiment with 10164 rows and 132 columns
 [3] KICH_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 66 columns
 [4] KICH_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 2 rows and 66 columns
 [5] KICH_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 66 columns
 [6] KICH_Mutation-20160128: RaggedExperiment with 7559 rows and 66 columns
 [7] KICH_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 91 columns
 [8] KICH_RPPAArray-20160128: SummarizedExperiment with 193 rows and 63 columns
 [9] KICH_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 91 columns
 [10] KICH_RNASeq2GeneNorm-20160128: SummarizedExperiment with 18122 rows and 91 columns
 [11] KICH_Methylation-20160128: SummarizedExperiment with 485577 rows and 66 columns

> rownames( KICH )
CharacterList of length 11
[["KICH_CNASNP-20160128"]] character(0)
[["KICH_CNVSNP-20160128"]] character(0)
[["KICH_GISTIC_AllByGene-20160128"]] character(0)
[["KICH_GISTIC_Peaks-20160128"]] 1 2
[["KICH_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["KICH_Mutation-20160128"]] character(0)
[["KICH_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["KICH_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
[["KICH_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["KICH_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
...
<1 more element>

> colnames( KICH )
CharacterList of length 11
[["KICH_CNASNP-20160128"]] TCGA-KL-8323-01A-21D-2308-01 ...
[["KICH_CNVSNP-20160128"]] TCGA-KL-8323-01A-21D-2308-01 ...
[["KICH_GISTIC_AllByGene-20160128"]] TCGA-KL-8323-01A-21D-2308-01 ...
[["KICH_GISTIC_Peaks-20160128"]] TCGA-KL-8323-01A-21D-2308-01 ...
[["KICH_GISTIC_ThresholdedByGene-20160128"]] TCGA-KL-8323-01A-21D-2308-01 ...
[["KICH_Mutation-20160128"]] TCGA-KL-8323-01A-21D-2310-10 ...
[["KICH_RNASeq2Gene-20160128"]] TCGA-KL-8323-01A-21R-2315-07 ...
[["KICH_RPPAArray-20160128"]] TCGA-KL-8324-01A-21-A421-20 ...
[["KICH_miRNASeqGene-20160128"]] TCGA-KL-8323-01A-21R-2314-13 ...
[["KICH_RNASeq2GeneNorm-20160128"]] TCGA-KL-8323-01 ... TCGA-KO-8417-01
...
<1 more element>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    KICH_CNASNP-20160128  1.6 Mb
2                    KICH_CNVSNP-20160128  0.3 Mb
3          KICH_GISTIC_AllByGene-20160128 15.9 Mb
4              KICH_GISTIC_Peaks-20160128    0 Mb
5  KICH_GISTIC_ThresholdedByGene-20160128 15.8 Mb
6                  KICH_Mutation-20160128  2.6 Mb
7               KICH_RNASeq2Gene-20160128 16.8 Mb
8                 KICH_RPPAArray-20160128  0.1 Mb
9              KICH_miRNASeqGene-20160128  0.9 Mb
10          KICH_RNASeq2GeneNorm-20160128 14.8 Mb
11              KICH_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   57 observations deleted due to missingness 
     n events median 0.95LCL 0.95UCL
[1,] 9      9   2.34    1.99      NA


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  17.00   42.00   50.00   51.52   61.75   86.00 

vital_status:
 0  1 
56 10 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    325     725     855    1001    1158    2172      57 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     30    1366    2713    2320    3138    4676      10 

tumor_tissue_site:
kidney 
    66 

pathologic_stage:
  stage i  stage ii stage iii  stage iv 
       21        25        14         6 


pathology_N_stage:
n0 n1 n2 nx 
40  3  2 21 

pathology_M_stage:
  m0   m1   mx NA's 
  34    2    9   21 

gender:
female   male 
    27     39 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2000    2004    2005    2005    2007    2011 

radiation_therapy:
no 
66 

karnofsky_performance_score:
  90  100 NA's 
   3   10   53 

histological_type:
kidney chromophobe 
                66 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   24.00   25.09   31.00   75.00      55 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1949    1964    1974    1974    1984    1997      58 

race:
                    asian black or african american                     white 
                        2                         4                        58 
                     NA's 
                        2 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     4                     32                     30 

Including an additional 718 columns
}}
\keyword{datasets}

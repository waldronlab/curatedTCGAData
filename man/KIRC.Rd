\name{ KIRC }
\alias{ KIRC }
\docType{data}
\title{ Kidney renal clear cell carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( KIRC )
ExperimentList class object of length 13: 
 [1] KIRC_CNASNP-20160128: RaggedExperiment with 488691 rows and 1059 columns 
 [2] KIRC_CNVSNP-20160128: RaggedExperiment with 85036 rows and 1059 columns 
 [3] KIRC_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 528 columns 
 [4] KIRC_GISTIC_Peaks-20160128: SummarizedExperiment with 27 rows and 528 columns 
 [5] KIRC_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 528 columns 
 [6] KIRC_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 326 columns 
 [7] KIRC_mRNAArray-20160128: SummarizedExperiment with 17814 rows and 72 columns 
 [8] KIRC_Mutation-20160128: RaggedExperiment with 26369 rows and 437 columns 
 [9] KIRC_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 606 columns 
 [10] KIRC_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 537 columns 
 [11] KIRC_RPPAArray-20160128: SummarizedExperiment with 217 rows and 478 columns 
 [12] KIRC_Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 418 columns 
 [13] KIRC_Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 480 columns 

> rownames( KIRC )
CharacterList of length 13
[["KIRC_CNASNP-20160128"]] character(0)
[["KIRC_CNVSNP-20160128"]] character(0)
[["KIRC_GISTIC_AllByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["KIRC_GISTIC_Peaks-20160128"]] character(0)
[["KIRC_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["KIRC_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["KIRC_mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 ... SNRPD2 AQP7 CTSC
[["KIRC_Mutation-20160128"]] character(0)
[["KIRC_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
[["KIRC_RNASeqGene-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
...
<3 more elements>

> colnames( KIRC )
CharacterList of length 13
[["KIRC_CNASNP-20160128"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ...
[["KIRC_CNVSNP-20160128"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ...
[["KIRC_GISTIC_AllByGene-20160128"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ...
[["KIRC_GISTIC_Peaks-20160128"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ...
[["KIRC_GISTIC_ThresholdedByGene-20160128"]] TCGA-3Z-A93Z-01A-11D-A36W-01...
[["KIRC_miRNASeqGene-20160128"]] TCGA-3Z-A93Z-01A-11R-A37P-13 ...
[["KIRC_mRNAArray-20160128"]] TCGA-A3-3306-01A-01R-0864-07 ...
[["KIRC_Mutation-20160128"]] TCGA-A3-3308-01A-01D-0966-08 ...
[["KIRC_RNASeq2GeneNorm-20160128"]] TCGA-3Z-A93Z-01A-11R-A37O-07 ...
[["KIRC_RNASeqGene-20160128"]] TCGA-A3-3306-01A-01R-0864-07 ...
...
<3 more elements>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    KIRC_CNASNP-20160128 13.4 Mb
2                    KIRC_CNVSNP-20160128  2.6 Mb
3          KIRC_GISTIC_AllByGene-20160128  4.9 Mb
4              KIRC_GISTIC_Peaks-20160128  0.1 Mb
5  KIRC_GISTIC_ThresholdedByGene-20160128  4.9 Mb
6              KIRC_miRNASeqGene-20160128  0.1 Mb
7                 KIRC_mRNAArray-20160128  1.1 Mb
8                  KIRC_Mutation-20160128  8.2 Mb
9           KIRC_RNASeq2GeneNorm-20160128  1.3 Mb
10               KIRC_RNASeqGene-20160128  1.3 Mb
11                KIRC_RPPAArray-20160128  0.1 Mb
12     KIRC_Methylation_methyl27-20160128  4.9 Mb
13    KIRC_Methylation_methyl450-20160128 75.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   360 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 177.00  177.00    2.24    1.77    2.61 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.00   52.00   61.00   60.57   70.00   90.00       1 

vital_status:
  0   1 
360 177 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   333.0   819.0   961.2  1432.0  3615.0     360 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   710.5  1454.5  1536.9  2172.0  4537.0     177 

tumor_tissue_site:
kidney 
   537 

pathologic_stage:
  stage i  stage ii stage iii  stage iv      NA's 
      269        57       125        84         2 


pathology_N_stage:
 n0  n1  nx 
240  17 280 

pathology_M_stage:
  m0   m1   mx NA's 
 426   79   30    2 

gender:
female   male 
   191    346 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1998    2004    2006    2006    2007    2013 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     191    1172    1117    1887    2799     510 

radiation_therapy:
  no  yes NA's 
 142    2  393 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00   90.00   85.56  100.00  100.00     483 

histological_type:
kidney clear cell renal carcinoma 
                              537 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   14.00   30.00   28.33   40.00   65.00     516 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1946    1966    1978    1979    1996    2001     525 

race:
                    asian black or african american 
                        8                        56 
                    white                      NA's 
                      466                         7 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    26                    359                    152 

Including an additional 2250 columns
}}
\keyword{datasets}

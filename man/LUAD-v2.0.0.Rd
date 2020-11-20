\name{LUAD-v2.0.0}
\alias{ LUAD-v2.0.0 }
\docType{data}
\title{ Lung adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( LUAD )
ExperimentList class object of length 15:
 [1] LUAD_CNASeq-20160128: RaggedExperiment with 399592 rows and 249 columns
 [2] LUAD_CNASNP-20160128: RaggedExperiment with 501475 rows and 1095 columns
 [3] LUAD_CNVSNP-20160128: RaggedExperiment with 115836 rows and 1095 columns
 [4] LUAD_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 516 columns
 [5] LUAD_GISTIC_Peaks-20160128: SummarizedExperiment with 150 rows and 516 columns
 [6] LUAD_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 516 columns
 [7] LUAD_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 498 columns
 [8] LUAD_mRNAArray-20160128: SummarizedExperiment with 17814 rows and 32 columns
 [9] LUAD_Mutation-20160128: RaggedExperiment with 72541 rows and 230 columns
 [10] LUAD_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 576 columns
 [11] LUAD_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 576 columns
 [12] LUAD_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 162 columns
 [13] LUAD_RPPAArray-20160128: SummarizedExperiment with 223 rows and 365 columns
 [14] LUAD_Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 89 columns
 [15] LUAD_Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 492 columns

> rownames( LUAD )
CharacterList of length 15
[["LUAD_CNASeq-20160128"]] character(0)
[["LUAD_CNASNP-20160128"]] character(0)
[["LUAD_CNVSNP-20160128"]] character(0)
[["LUAD_GISTIC_AllByGene-20160128"]] ACAP3 ...
[["LUAD_GISTIC_Peaks-20160128"]] character(0)
[["LUAD_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["LUAD_miRNASeqGene-20160128"]] hsa-let-7a-1 ... hsa-mir-99b
[["LUAD_mRNAArray-20160128"]] ELMO2 CREB3L1 ... AQP7 CTSC
[["LUAD_Mutation-20160128"]] character(0)
[["LUAD_RNASeq2Gene-20160128"]] A1BG A1CF ... psiTPTE22 tAKR
...
<5 more elements>

> colnames( LUAD )
CharacterList of length 15
[["LUAD_CNASeq-20160128"]] TCGA-05-4249-01A-01D-1103-02 ...
[["LUAD_CNASNP-20160128"]] TCGA-05-4244-01A-01D-1877-01 ...
[["LUAD_CNVSNP-20160128"]] TCGA-05-4244-01A-01D-1877-01 ...
[["LUAD_GISTIC_AllByGene-20160128"]] TCGA-05-4244-01A-01D-1...
[["LUAD_GISTIC_Peaks-20160128"]] TCGA-05-4244-01A-01D-1877-...
[["LUAD_GISTIC_ThresholdedByGene-20160128"]] TCGA-05-4244-0...
[["LUAD_miRNASeqGene-20160128"]] TCGA-05-4384-01A-01T-1754-...
[["LUAD_mRNAArray-20160128"]] TCGA-05-4244-01A-01R-1107-07 ...
[["LUAD_Mutation-20160128"]] TCGA-05-4249-01A-01D-1105-08 ...
[["LUAD_RNASeq2Gene-20160128"]] TCGA-05-4244-01A-01R-1107-0...
...
<5 more elements>

Sizes of each ExperimentList element:

                                    assay  size.Mb
1                    LUAD_CNASeq-20160128  10.8 Mb
2                    LUAD_CNASNP-20160128  13.7 Mb
3                    LUAD_CNVSNP-20160128   3.4 Mb
4          LUAD_GISTIC_AllByGene-20160128 102.5 Mb
5              LUAD_GISTIC_Peaks-20160128   0.7 Mb
6  LUAD_GISTIC_ThresholdedByGene-20160128 102.5 Mb
7              LUAD_miRNASeqGene-20160128   4.2 Mb
8                 LUAD_mRNAArray-20160128   6.6 Mb
9                  LUAD_Mutation-20160128  92.9 Mb
10              LUAD_RNASeq2Gene-20160128  92.7 Mb
11          LUAD_RNASeq2GeneNorm-20160128  92.7 Mb
12               LUAD_RNASeqGene-20160128  27.9 Mb
13                LUAD_RPPAArray-20160128   0.7 Mb
14     LUAD_Methylation_methyl27-20160128   4.9 Mb
15    LUAD_Methylation_methyl450-20160128  75.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   336 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 184.00  184.00    1.70    1.37    2.00 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  33.00   59.00   66.00   65.22   72.00   88.00      31 

vital_status:
  0   1 
332 188 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   297.8   619.0   791.4  1120.0  4961.0     336 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   459.5   670.0   968.7  1139.0  7248.0     193 

tumor_tissue_site:
lung 
 520 





gender:
female   male 
   279    241 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1991    2007    2010    2008    2011    2013      19 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    96.5   141.0   327.6   386.0  1178.0     513 

radiation_therapy:
  no  yes NA's 
 413   61   46 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   80.00   90.00   78.55  100.00  100.00     382 


number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.15   20.50   40.00   41.79   50.00  154.00     165 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1930    1956    1965    1965    1972    1999     241 

residual_tumor:
  r0   r1   r2   rx NA's 
 347   13    4   26  130 

race:
american indian or alaska native 
                               1 
                           asian 
                               8 
       black or african american 
                              53 
                           white 
                             392 
                            NA's 
                              66 

ethnicity:
    hispanic or latino not hispanic or latino 
                     7                    388 
                  NA's 
                   125 

Including an additional 2607 columns
}}
\keyword{datasets}

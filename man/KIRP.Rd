\name{ KIRP }
\alias{ KIRP }
\docType{data}
\title{ Kidney renal papillary cell carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( KIRP )
ExperimentList class object of length 13: 
 [1] KIRP_CNASNP-20160128: RaggedExperiment with 300681 rows and 593 columns 
 [2] KIRP_CNVSNP-20160128: RaggedExperiment with 46914 rows and 590 columns 
 [3] KIRP_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 288 columns 
 [4] KIRP_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 28 rows and 288 columns 
 [5] KIRP_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 288 columns 
 [6] KIRP_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 326 columns 
 [7] KIRP_mRNAArray-20160128: SummarizedExperiment with 17814 rows and 16 columns 
 [8] KIRP_Mutation-20160128: RaggedExperiment with 15585 rows and 161 columns 
 [9] KIRP_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 323 columns 
 [10] KIRP_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 14 columns 
 [11] KIRP_RPPAArray-20160128: SummarizedExperiment with 195 rows and 216 columns 
 [12] KIRP_Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 21 columns 
 [13] KIRP_Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 321 columns 

> rownames( KIRP )
CharacterList of length 13
[["KIRP_CNASNP-20160128"]] character(0)
[["KIRP_CNVSNP-20160128"]] character(0)
[["KIRP_GISTIC_AllByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["KIRP_GISTIC_Peaks-20160128"]] character(0)
[["KIRP_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["KIRP_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["KIRP_mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 ... SNRPD2 AQP7 CTSC
[["KIRP_Mutation-20160128"]] character(0)
[["KIRP_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
[["KIRP_RNASeqGene-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
...
<3 more elements>

> colnames( KIRP )
CharacterList of length 13
[["KIRP_CNASNP-20160128"]] TCGA-2K-A9WE-01A-11D-A381-01 ...
[["KIRP_CNVSNP-20160128"]] TCGA-2K-A9WE-01A-11D-A381-01 ...
[["KIRP_GISTIC_AllByGene-20160128"]] TCGA-2K-A9WE-01A-11D-A381-01 ...
[["KIRP_GISTIC_Peaks-20160128"]] TCGA-2K-A9WE-01A-11D-A381-01 ...
[["KIRP_GISTIC_ThresholdedByGene-20160128"]] TCGA-2K-A9WE-01A-11D-A381-01...
[["KIRP_miRNASeqGene-20160128"]] TCGA-2K-A9WE-01A-11R-A38N-13 ...
[["KIRP_mRNAArray-20160128"]] TCGA-AL-3466-01A-01R-1193-07 ...
[["KIRP_Mutation-20160128"]] TCGA-A4-7286-01A-11D-2136-08 ...
[["KIRP_RNASeq2GeneNorm-20160128"]] TCGA-2K-A9WE-01A-11R-A38C-07 ...
[["KIRP_RNASeqGene-20160128"]] TCGA-AL-3466-01A-02R-1351-07 ...
...
<3 more elements>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    KIRP_CNASNP-20160128  8.2 Mb
2                    KIRP_CNVSNP-20160128  1.4 Mb
3          KIRP_GISTIC_AllByGene-20160128  4.9 Mb
4              KIRP_GISTIC_Peaks-20160128  0.1 Mb
5  KIRP_GISTIC_ThresholdedByGene-20160128  4.9 Mb
6              KIRP_miRNASeqGene-20160128  0.1 Mb
7                 KIRP_mRNAArray-20160128  1.1 Mb
8                  KIRP_Mutation-20160128 10.6 Mb
9           KIRP_RNASeq2GeneNorm-20160128  1.3 Mb
10               KIRP_RNASeqGene-20160128  1.3 Mb
11                KIRP_RPPAArray-20160128    0 Mb
12     KIRP_Methylation_methyl27-20160128  4.9 Mb
13    KIRP_Methylation_methyl450-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   247 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  44.00   44.00    1.76    1.35    3.60 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  28.00   53.25   61.50   61.52   71.00   88.00       5 

vital_status:
  0   1 
247  44 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  122.0   341.2   641.0   989.8  1498.5  2941.0     247 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   438.2   774.5  1070.2  1513.0  5925.0      45 

tumor_tissue_site:
kidney 
   291 

pathologic_stage:
  stage i  stage ii stage iii  stage iv      NA's 
      173        21        52        15        30 


pathology_N_stage:
  n0   n1   n2   nx NA's 
  50   24    4  212    1 

pathology_M_stage:
  m0   m1   mx NA's 
  95    9  172   15 

gender:
female   male 
    77    214 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1996    2008    2011    2010    2012    2013      25 

days_to_last_known_alive:
  34 NA's 
   1  290 

radiation_therapy:
  no  yes NA's 
 209    1   81 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00   90.00   87.66  100.00  100.00     214 

histological_type:
kidney papillary renal cell carcinoma 
                                  291 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   15.00   24.50   31.73   41.25  185.00     215 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1941    1961    1970    1972    1984    2010     235 

race:
american indian or alaska native                            asian 
                               2                                6 
       black or african american                            white 
                              61                              207 
                            NA's 
                              15 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    12                    243                     36 

Including an additional 1686 columns
}}
\keyword{datasets}

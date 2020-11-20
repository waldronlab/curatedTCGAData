\name{ LUSC }
\alias{ LUSC }
\docType{data}
\title{ Lung squamous cell carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( LUSC )
ExperimentList class object of length 16: 
 [1] LUSC_CNACGH-20160128: RaggedExperiment with 87417 rows and 407 columns 
 [2] LUSC_CNASNP-20160128: RaggedExperiment with 543091 rows and 1035 columns 
 [3] LUSC_CNVSNP-20160128: RaggedExperiment with 134864 rows and 1032 columns 
 [4] LUSC_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 501 columns 
 [5] LUSC_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 78 rows and 501 columns 
 [6] LUSC_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 501 columns 
 [7] LUSC_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 387 columns 
 [8] LUSC_mRNAArray_huex-20160128: SummarizedExperiment with 18632 rows and 112 columns 
 [9] LUSC_mRNAArray_TX_g4502a-20160128: SummarizedExperiment with 17814 rows and 154 columns 
 [10] LUSC_mRNAArray_TX_ht_hg_u133a-20160128: SummarizedExperiment with 12042 rows and 133 columns 
 [11] LUSC_Mutation-20160128: RaggedExperiment with 65305 rows and 178 columns 
 [12] LUSC_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 552 columns 
 [13] LUSC_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 240 columns 
 [14] LUSC_RPPAArray-20160128: SummarizedExperiment with 223 rows and 328 columns 
 [15] LUSC_Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 160 columns 
 [16] LUSC_Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 412 columns 

> rownames( LUSC )
CharacterList of length 16
[["LUSC_CNACGH-20160128"]] character(0)
[["LUSC_CNASNP-20160128"]] character(0)
[["LUSC_CNVSNP-20160128"]] character(0)
[["LUSC_GISTIC_AllByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["LUSC_GISTIC_Peaks-20160128"]] chr1:1-31262836 ... chr22:45736500-51304566
[["LUSC_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["LUSC_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["LUSC_mRNAArray_huex-20160128"]] C9orf152 ELMO2 RPS11 ... SLC39A6 CTSC AQP7
[["LUSC_mRNAArray_TX_g4502a-20160128"]] ELMO2 CREB3L1 RPS11 ... AQP7 CTSC
[["LUSC_mRNAArray_TX_ht_hg_u133a-20160128"]] AACS FSTL1 ELMO2 ... CTSC AQP7
...
<6 more elements>

> colnames( LUSC )
CharacterList of length 16
[["LUSC_CNACGH-20160128"]] TCGA-18-3406-01A-01D-1519-02 ...
[["LUSC_CNASNP-20160128"]] TCGA-18-3406-01A-01D-0978-01 ...
[["LUSC_CNVSNP-20160128"]] TCGA-18-3406-01A-01D-0978-01 ...
[["LUSC_GISTIC_AllByGene-20160128"]] TCGA-18-3406-01A-01D-0978-01 ...
[["LUSC_GISTIC_Peaks-20160128"]] TCGA-18-3406-01A-01D-0978-01 ...
[["LUSC_GISTIC_ThresholdedByGene-20160128"]] TCGA-18-3406-01A-01D-0978-01 ...
[["LUSC_miRNASeqGene-20160128"]] TCGA-18-5592-01A-01T-1634-13 ...
[["LUSC_mRNAArray_huex-20160128"]] TCGA-18-3406-01A-01R-1030-03 ...
[["LUSC_mRNAArray_TX_g4502a-20160128"]] TCGA-18-3406-01A-01R-0980-07 ...
[["LUSC_mRNAArray_TX_ht_hg_u133a-20160128"]] TCGA-18-3406-01A-01R-1031-01 ...
...
<6 more elements>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    LUSC_CNACGH-20160128  2.5 Mb
2                    LUSC_CNASNP-20160128 14.8 Mb
3                    LUSC_CNVSNP-20160128  3.9 Mb
4          LUSC_GISTIC_AllByGene-20160128  4.9 Mb
5              LUSC_GISTIC_Peaks-20160128  0.1 Mb
6  LUSC_GISTIC_ThresholdedByGene-20160128  4.9 Mb
7              LUSC_miRNASeqGene-20160128  0.1 Mb
8            LUSC_mRNAArray_huex-20160128  1.2 Mb
9       LUSC_mRNAArray_TX_g4502a-20160128  1.1 Mb
10 LUSC_mRNAArray_TX_ht_hg_u133a-20160128  0.8 Mb
11                 LUSC_Mutation-20160128   81 Mb
12          LUSC_RNASeq2GeneNorm-20160128  1.3 Mb
13               LUSC_RNASeqGene-20160128  1.3 Mb
14                LUSC_RPPAArray-20160128    0 Mb
15     LUSC_Methylation_methyl27-20160128  4.9 Mb
16    LUSC_Methylation_methyl450-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   289 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 215.00  215.00    1.51    1.25    1.88 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  39.00   62.00   68.00   67.26   73.00   90.00      10 

vital_status:
  0   1 
284 220 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   280.0   550.0   872.3  1110.5  5287.0     289 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     394     757    1049    1374    4765     221 

tumor_tissue_site:
lung 
 504 



pathology_N_stage:
 n0  n1  n2  n3  nx 
320 133  40   5   6 


gender:
female   male 
   131    373 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2005    2009    2008    2011    2013      25 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   298.8   706.0   904.8  1051.8  3724.0     436 

radiation_therapy:
  no  yes NA's 
 387   53   64 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    80.0    60.3    90.0   100.0     338 

histological_type:
                      lung basaloid squamous cell carcinoma 
                                                         15 
                     lung papillary squamous cell caricnoma 
                                                          6 
                    lung small cell squamous cell carcinoma 
                                                          1 
lung squamous cell carcinoma- not otherwise specified (nos) 
                                                        482 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   31.12   50.00   52.91   64.50  240.00      77 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1933    1952    1960    1960    1968    1997     183 

residual_tumor:
  r0   r1   r2   rx NA's 
 401   12    4   23   64 

race:
                    asian black or african american                     white 
                        9                        31                       351 
                     NA's 
                      113 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     8                    319                    177 

Including an additional 2238 columns
}}
\seealso{\link{LUSC-v2.0.0}}
\keyword{datasets}

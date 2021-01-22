\name{ACC-v2.0.1}
\alias{ ACC-v2.0.1 }
\docType{data}
\title{ Adrenocortical carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( ACC )
ExperimentList class object of length 11:
 [1] ACC_CNASNP-20160128: RaggedExperiment with 79861 rows and 180 columns
 [2] ACC_CNVSNP-20160128: RaggedExperiment with 21052 rows and 180 columns
 [3] ACC_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 90 columns
 [4] ACC_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 42 rows and 90 columns
 [5] ACC_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 90 columns
 [6] ACC_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 80 columns
 [7] ACC_Mutation-20160128: RaggedExperiment with 20166 rows and 90 columns
 [8] ACC_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 79 columns
 [9] ACC_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 79 columns
 [10] ACC_RPPAArray-20160128: SummarizedExperiment with 192 rows and 46 columns
 [11] ACC_Methylation-20160128: SummarizedExperiment with 485577 rows and 80 columns

> rownames( ACC )
CharacterList of length 11
[["ACC_CNASNP-20160128"]] character(0)
[["ACC_CNVSNP-20160128"]] character(0)
[["ACC_GISTIC_AllByGene-20160128"]] character(0)
[["ACC_GISTIC_Peaks-20160128"]] 19 1 20 21 22 2 24 ... 41 42 14 15 16 43 44
[["ACC_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["ACC_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["ACC_Mutation-20160128"]] character(0)
[["ACC_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["ACC_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
[["ACC_RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363
...
<1 more element>

> colnames( ACC )
CharacterList of length 11
[["ACC_CNASNP-20160128"]] TCGA-OR-A5J1-01A-11D-A29H-01 ...
[["ACC_CNVSNP-20160128"]] TCGA-OR-A5J1-01A-11D-A29H-01 ...
[["ACC_GISTIC_AllByGene-20160128"]] TCGA-OR-A5J1-01A-11D-A29H-01 ...
[["ACC_GISTIC_Peaks-20160128"]] TCGA-OR-A5J1-01A-11D-A29H-01 ...
[["ACC_GISTIC_ThresholdedByGene-20160128"]] TCGA-OR-A5J1-01A-11D-A29H-01 ...
[["ACC_miRNASeqGene-20160128"]] TCGA-OR-A5J1-01A-11R-A29W-13 ...
[["ACC_Mutation-20160128"]] TCGA-OR-A5J1-01A-11D-A29I-10 ...
[["ACC_RNASeq2Gene-20160128"]] TCGA-OR-A5J1-01A-11R-A29S-07 ...
[["ACC_RNASeq2GeneNorm-20160128"]] TCGA-OR-A5J1-01A-11R-A29S-07 ...
[["ACC_RPPAArray-20160128"]] TCGA-OR-A5J2-01A-21-A39K-20 ...
...
<1 more element>

Sizes of each ExperimentList element:

                                   assay size.Mb
1                    ACC_CNASNP-20160128  2.2 Mb
2                    ACC_CNVSNP-20160128  0.6 Mb
3          ACC_GISTIC_AllByGene-20160128 20.5 Mb
4              ACC_GISTIC_Peaks-20160128  0.1 Mb
5  ACC_GISTIC_ThresholdedByGene-20160128 20.4 Mb
6              ACC_miRNASeqGene-20160128  0.8 Mb
7                  ACC_Mutation-20160128 12.6 Mb
8               ACC_RNASeq2Gene-20160128 14.9 Mb
9           ACC_RNASeq2GeneNorm-20160128 14.9 Mb
10                ACC_RPPAArray-20160128  0.1 Mb
11              ACC_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   58 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  34.00   34.00    1.51    1.34    3.03 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.00   35.50   48.50   47.16   60.00   83.00 

vital_status:
 0  1 
58 34 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   399.0   551.5   854.6  1202.2  2405.0      58 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  383.0   917.8  1453.5  1781.6  2339.2  4673.0      34 

tumor_tissue_site:
adrenal 
     92 

pathologic_stage:
  stage i  stage ii stage iii  stage iv      NA's 
        9        44        19        18         2 

pathology_T_stage:
  t1   t2   t3   t4 NA's 
   9   49   11   21    2 

pathology_N_stage:
  n0   n1 NA's 
  80   10    2 

gender:
female   male 
    60     32 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1997    2005    2008    2007    2011    2012 

radiation_therapy:
  no  yes NA's 
  71   18    3 

histological_type:
   adrenocortical carcinoma- myxoid type 
                                       1 
adrenocortical carcinoma- oncocytic type 
                                       4 
    adrenocortical carcinoma- usual type 
                                      87 

residual_tumor:
  r0   r1   r2   rx NA's 
  64    7   12    6    3 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.548   0.500  52.000      61 

race:
                    asian black or african american                     white 
                        2                         1                        78 
                     NA's 
                       11 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     8                     40                     44 

Including an additional 806 columns
}}
\keyword{datasets}

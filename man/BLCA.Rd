\name{ BLCA }
\alias{ BLCA }
\docType{data}
\title{ Bladder Urothelial Carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( BLCA )
ExperimentList class object of length 11: 
 [1] CNASeq-20160128: RaggedExperiment with 35692 rows and 232 columns 
 [2] CNASNP-20160128: RaggedExperiment with 479978 rows and 806 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 104349 rows and 797 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 408 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 408 columns 
 [6] Methylation-20160128: SummarizedExperiment with 485577 rows and 434 columns 
 [7] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 429 columns 
 [8] Mutation-20160128: RaggedExperiment with 39312 rows and 130 columns 
 [9] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 427 columns 
 [10] RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 67 columns 
 [11] RPPAArray-20160128: SummarizedExperiment with 208 rows and 344 columns 

> rownames( BLCA )
CharacterList of length 11
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RNASeqGene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
...
<1 more element>

> colnames( BLCA )
CharacterList of length 11
[["CNASeq-20160128"]] TCGA-BL-A0C8-01A-11D-A10R-02 ...
[["CNASNP-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["CNVSNP-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["Methylation-20160128"]] TCGA-2F-A9KO-01A-11D-A38H-05 ...
[["miRNASeqGene-20160128"]] TCGA-2F-A9KO-01A-11R-A38M-13 ...
[["Mutation-20160128"]] TCGA-BL-A0C8-01A-11D-A10S-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-2F-A9KO-01A-11R-A38B-07 ...
[["RNASeqGene-20160128"]] TCGA-BL-A0C8-01A-11R-A10U-07 ...
...
<1 more element>

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128    1 Mb
2                    CNASNP-20160128 13.1 Mb
3                    CNVSNP-20160128    3 Mb
4          GISTIC_AllByGene-20160128  4.9 Mb
5  GISTIC_ThresholdedByGene-20160128  4.9 Mb
6               Methylation-20160128 75.1 Mb
7              miRNASeqGene-20160128  0.1 Mb
8                  Mutation-20160128 56.1 Mb
9           RNASeq2GeneNorm-20160128  1.3 Mb
10               RNASeqGene-20160128  1.3 Mb
11                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   232 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
180.000 180.000   1.125   0.992   1.332 


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

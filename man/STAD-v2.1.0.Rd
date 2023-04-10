\name{STAD-v2.1.0}
\alias{ STAD-v2.1.0 }
\docType{data}
\title{ Stomach adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( STAD )
ExperimentList class object of length 14:
 [1] STAD_CNASeq-20160128: RaggedExperiment with 31824 rows and 214 columns
 [2] STAD_CNASNP-20160128: RaggedExperiment with 443042 rows and 906 columns
 [3] STAD_CNVSNP-20160128: RaggedExperiment with 118389 rows and 904 columns
 [4] STAD_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 441 columns
 [5] STAD_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 84 rows and 441 columns
 [6] STAD_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 441 columns
 [7] STAD_Mutation-20160128: RaggedExperiment with 148520 rows and 289 columns
 [8] STAD_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 450 columns
 [9] STAD_RNASeqGene-20160128: SummarizedExperiment with 26120 rows and 271 columns
 [10] STAD_RPPAArray-20160128: SummarizedExperiment with 195 rows and 357 columns
 [11] STAD_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 430 columns
 [12] STAD_RNASeq2GeneNorm-20160128: SummarizedExperiment with 18664 rows and 450 columns
 [13] STAD_Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 73 columns
 [14] STAD_Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 397 columns

> rownames( STAD )
CharacterList of length 14
[["STAD_CNASeq-20160128"]] character(0)
[["STAD_CNASNP-20160128"]] character(0)
[["STAD_CNVSNP-20160128"]] character(0)
[["STAD_GISTIC_AllByGene-20160128"]] character(0)
[["STAD_GISTIC_Peaks-20160128"]] 1 37 38 2 3 39 40 ... 81 34 82 35 83 84 85
[["STAD_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["STAD_Mutation-20160128"]] character(0)
[["STAD_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["STAD_RNASeqGene-20160128"]] AADACL3 AADACL4 AB007962 ... VCY XKRY ZFY
[["STAD_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
...
<4 more elements>

> colnames( STAD )
CharacterList of length 14
[["STAD_CNASeq-20160128"]] TCGA-B7-5816-01A-21D-1598-02 ...
[["STAD_CNASNP-20160128"]] TCGA-3M-AB46-01A-11D-A40Z-01 ...
[["STAD_CNVSNP-20160128"]] TCGA-3M-AB46-01A-11D-A40Z-01 ...
[["STAD_GISTIC_AllByGene-20160128"]] TCGA-3M-AB46-01A-11D-A40Z-01 ...
[["STAD_GISTIC_Peaks-20160128"]] TCGA-3M-AB46-01A-11D-A40Z-01 ...
[["STAD_GISTIC_ThresholdedByGene-20160128"]] TCGA-3M-AB46-01A-11D-A40Z-01 ...
[["STAD_Mutation-20160128"]] TCGA-B7-5816-01A-21D-1600-08 ...
[["STAD_RNASeq2Gene-20160128"]] TCGA-3M-AB46-01A-11R-A414-31 ...
[["STAD_RNASeqGene-20160128"]] TCGA-B7-5816-01A-21R-1602-13 ...
[["STAD_RPPAArray-20160128"]] TCGA-3M-AB47-01A-11-A43D-20 ...
...
<4 more elements>

Sizes of each ExperimentList element:

                                    assay  size.Mb
1                    STAD_CNASeq-20160128   0.9 Mb
2                    STAD_CNASNP-20160128  12.1 Mb
3                    STAD_CNVSNP-20160128   3.4 Mb
4          STAD_GISTIC_AllByGene-20160128    87 Mb
5              STAD_GISTIC_Peaks-20160128   0.4 Mb
6  STAD_GISTIC_ThresholdedByGene-20160128  86.8 Mb
7                  STAD_Mutation-20160128 161.5 Mb
8               STAD_RNASeq2Gene-20160128    73 Mb
9                STAD_RNASeqGene-20160128  57.4 Mb
10                STAD_RPPAArray-20160128   0.6 Mb
11             STAD_miRNASeqGene-20160128   3.7 Mb
12          STAD_RNASeq2GeneNorm-20160128  66.5 Mb
13     STAD_Methylation_methyl27-20160128   4.9 Mb
14    STAD_Methylation_methyl450-20160128  75.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   273 observations deleted due to missingness 
       n events median 0.95LCL 0.95UCL
[1,] 170    170  0.948   0.792    1.08


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   58.00   67.00   65.73   73.00   90.00       9 

vital_status:
  0   1 
268 175 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   194.0   346.0   423.7   553.5  2197.0     273 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   335.5   547.5   673.7   912.0  3720.0     177 

tumor_tissue_site:
stomach 
    443 




pathology_M_stage:
 m0  m1  mx 
391  30  22 

gender:
female   male 
   158    285 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1996    2010    2011    2010    2012    2013       6 

radiation_therapy:
  no  yes NA's 
 323   77   43 


residual_tumor:
  r0   r1   r2   rx NA's 
 350   18   19   25   31 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   5.635   8.000  57.000      51 

race:
                                    asian 
                                       89 
                black or african american 
                                       13 
native hawaiian or other pacific islander 
                                        1 
                                    white 
                                      278 
                                     NA's 
                                       62 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     5                    318                    120 

Including an additional 1390 columns
}}
\keyword{datasets}

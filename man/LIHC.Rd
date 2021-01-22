\name{ LIHC }
\alias{ LIHC }
\docType{data}
\title{ Liver hepatocellular carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( LIHC )
ExperimentList class object of length 11: 
 [1] LIHC_CNASNP-20160128: RaggedExperiment with 363628 rows and 760 columns 
 [2] LIHC_CNVSNP-20160128: RaggedExperiment with 93328 rows and 760 columns 
 [3] LIHC_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 370 columns 
 [4] LIHC_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 59 rows and 370 columns 
 [5] LIHC_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 370 columns 
 [6] LIHC_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 424 columns 
 [7] LIHC_Mutation-20160128: RaggedExperiment with 27892 rows and 198 columns 
 [8] LIHC_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 423 columns 
 [9] LIHC_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 26 columns 
 [10] LIHC_RPPAArray-20160128: SummarizedExperiment with 219 rows and 184 columns 
 [11] LIHC_Methylation-20160128: SummarizedExperiment with 485577 rows and 429 columns 

> rownames( LIHC )
CharacterList of length 11
[["LIHC_CNASNP-20160128"]] character(0)
[["LIHC_CNVSNP-20160128"]] character(0)
[["LIHC_GISTIC_AllByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["LIHC_GISTIC_Peaks-20160128"]] chr1:1-6847369 ... chr22:44205320-51304566
[["LIHC_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["LIHC_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["LIHC_Mutation-20160128"]] character(0)
[["LIHC_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
[["LIHC_RNASeqGene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["LIHC_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
...
<1 more element>

> colnames( LIHC )
CharacterList of length 11
[["LIHC_CNASNP-20160128"]] TCGA-2V-A95S-01A-11D-A36W-01 ...
[["LIHC_CNVSNP-20160128"]] TCGA-2V-A95S-01A-11D-A36W-01 ...
[["LIHC_GISTIC_AllByGene-20160128"]] TCGA-2V-A95S-01A-11D-A36W-01 ...
[["LIHC_GISTIC_Peaks-20160128"]] TCGA-2V-A95S-01A-11D-A36W-01 ...
[["LIHC_GISTIC_ThresholdedByGene-20160128"]] TCGA-2V-A95S-01A-11D-A36W-01 ...
[["LIHC_miRNASeqGene-20160128"]] TCGA-2V-A95S-01A-11R-A37G-13 ...
[["LIHC_Mutation-20160128"]] TCGA-BC-4073-01B-02D-A12Z-10 ...
[["LIHC_RNASeq2GeneNorm-20160128"]] TCGA-2V-A95S-01A-11R-A37K-07 ...
[["LIHC_RNASeqGene-20160128"]] TCGA-BC-4073-01B-02R-A131-07 ...
[["LIHC_RPPAArray-20160128"]] TCGA-BC-4072-01B-21-A40L-20 ...
...
<1 more element>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    LIHC_CNASNP-20160128  9.9 Mb
2                    LIHC_CNVSNP-20160128  2.7 Mb
3          LIHC_GISTIC_AllByGene-20160128  4.9 Mb
4              LIHC_GISTIC_Peaks-20160128  0.1 Mb
5  LIHC_GISTIC_ThresholdedByGene-20160128  4.9 Mb
6              LIHC_miRNASeqGene-20160128  0.1 Mb
7                  LIHC_Mutation-20160128 16.8 Mb
8           LIHC_RNASeq2GeneNorm-20160128  1.3 Mb
9                LIHC_RNASeqGene-20160128  1.3 Mb
10                LIHC_RPPAArray-20160128    0 Mb
11              LIHC_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   245 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
132.000 132.000   1.144   0.956   1.633 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  16.00   51.00   61.00   59.27   69.00   87.00       4 

vital_status:
  0   1 
245 132 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   194.8   417.5   672.1   837.0  3258.0     245 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   395.8   649.5   885.8  1222.0  3675.0     133 

tumor_tissue_site:
liver 
  377 



pathology_N_stage:
  n0   n1   nx NA's 
 257    4  115    1 

pathology_M_stage:
 m0  m1  mx 
272   4 101 

gender:
female   male 
   122    255 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1995    2008    2011    2010    2012    2013       3 

radiation_therapy:
  no  yes NA's 
 345    9   23 

histological_type:
         fibrolamellar carcinoma         hepatocellular carcinoma 
                               3                              367 
hepatocholangiocarcinoma (mixed) 
                               7 

residual_tumor:
  r0   r1   r2   rx NA's 
 330   17    1   22    7 

race:
american indian or alaska native                            asian 
                               2                              161 
       black or african american                            white 
                              17                              187 
                            NA's 
                              10 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    18                    340                     19 

Including an additional 1218 columns
}}
\seealso{\link{LIHC-v2.0.1}}
\keyword{datasets}

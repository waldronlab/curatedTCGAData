\name{PCPG-v2.1.0}
\alias{ PCPG-v2.1.0 }
\docType{data}
\title{ Pheochromocytoma and Paraganglioma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( PCPG )
ExperimentList class object of length 11:
 [1] PCPG_CNASNP-20160128: RaggedExperiment with 297329 rows and 360 columns
 [2] PCPG_CNVSNP-20160128: RaggedExperiment with 31256 rows and 346 columns
 [3] PCPG_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 162 columns
 [4] PCPG_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 27 rows and 162 columns
 [5] PCPG_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 162 columns
 [6] PCPG_Mutation-20160128: RaggedExperiment with 4662 rows and 184 columns
 [7] PCPG_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 187 columns
 [8] PCPG_RPPAArray-20160128: SummarizedExperiment with 192 rows and 82 columns
 [9] PCPG_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 187 columns
 [10] PCPG_RNASeq2GeneNorm-20160128: SummarizedExperiment with 17988 rows and 187 columns
 [11] PCPG_Methylation-20160128: SummarizedExperiment with 485577 rows and 187 columns

> rownames( PCPG )
CharacterList of length 11
[["PCPG_CNASNP-20160128"]] character(0)
[["PCPG_CNVSNP-20160128"]] character(0)
[["PCPG_GISTIC_AllByGene-20160128"]] character(0)
[["PCPG_GISTIC_Peaks-20160128"]] 7 1 8 9 10 11 2 12 ... 22 23 5 24 25 26 6 27
[["PCPG_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["PCPG_Mutation-20160128"]] character(0)
[["PCPG_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["PCPG_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
[["PCPG_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["PCPG_RNASeq2GeneNorm-20160128"]] A1BG A2BP1 A2LD1 ... ZZEF1 ZZZ3 psiTPTE22
...
<1 more element>

> colnames( PCPG )
CharacterList of length 11
[["PCPG_CNASNP-20160128"]] TCGA-P7-A5NX-10A-01D-A35A-01 ...
[["PCPG_CNVSNP-20160128"]] TCGA-P7-A5NX-10A-01D-A35A-01 ...
[["PCPG_GISTIC_AllByGene-20160128"]] TCGA-P7-A5NY-01A-12D-A35C-01 ...
[["PCPG_GISTIC_Peaks-20160128"]] TCGA-P7-A5NY-01A-12D-A35C-01 ...
[["PCPG_GISTIC_ThresholdedByGene-20160128"]] TCGA-P7-A5NY-01A-12D-A35C-01 ...
[["PCPG_Mutation-20160128"]] TCGA-P7-A5NX-01A-11D-A35D-08 ...
[["PCPG_RNASeq2Gene-20160128"]] TCGA-P7-A5NX-01A-11R-A35K-07 ...
[["PCPG_RPPAArray-20160128"]] TCGA-P7-A5NX-01A-21-A43B-20 ...
[["PCPG_miRNASeqGene-20160128"]] TCGA-P7-A5NX-01A-11R-A35M-13 ...
[["PCPG_RNASeq2GeneNorm-20160128"]] TCGA-P7-A5NX-01 ... TCGA-XG-A823-01
...
<1 more element>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    PCPG_CNASNP-20160128  8.1 Mb
2                    PCPG_CNVSNP-20160128  0.9 Mb
3          PCPG_GISTIC_AllByGene-20160128 34.1 Mb
4              PCPG_GISTIC_Peaks-20160128  0.1 Mb
5  PCPG_GISTIC_ThresholdedByGene-20160128   34 Mb
6                  PCPG_Mutation-20160128  8.4 Mb
7               PCPG_RNASeq2Gene-20160128 31.8 Mb
8                 PCPG_RPPAArray-20160128  0.2 Mb
9              PCPG_miRNASeqGene-20160128  1.7 Mb
10          PCPG_RNASeq2GeneNorm-20160128 27.9 Mb
11              PCPG_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   173 observations deleted due to missingness 
     n events median 0.95LCL 0.95UCL
[1,] 6      6   1.24    0.26      NA


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  19.00   35.00   46.00   47.33   58.50   83.00 

vital_status:
  0   1 
173   6 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   88.0   148.5   452.5   901.0   715.2  3563.0     173 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      2     352     766    1071    1301    9634       6 

tumor_tissue_site:
     adrenal gland extra-adrenal site 
               147                 32 

gender:
female   male 
   101     78 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1988    2009    2011    2010    2012    2013 

radiation_therapy:
  no  yes NA's 
 172    5    2 

karnofsky_performance_score:
  70   80   90  100 NA's 
   1    2   13   46  117 

histological_type:
                                paraganglioma 
                                           18 
paraganglioma; extra-adrenal pheochromocytoma 
                                           13 
                             pheochromocytoma 
                                          148 

number_of_lymph_nodes:
   0    1    2   13 NA's 
  16    3    1    1  158 

race:
american indian or alaska native                            asian 
                               1                                6 
       black or african american                            white 
                              20                              148 
                            NA's 
                               4 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     5                    138                     36 

Including an additional 894 columns
}}
\keyword{datasets}

\name{UCS-v2.0.0}
\alias{ UCS-v2.0.0 }
\docType{data}
\title{ Uterine Carcinosarcoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( UCS )
ExperimentList class object of length 11:
 [1] UCS_CNASNP-20160128: RaggedExperiment with 54944 rows and 111 columns
 [2] UCS_CNVSNP-20160128: RaggedExperiment with 19298 rows and 111 columns
 [3] UCS_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 56 columns
 [4] UCS_GISTIC_Peaks-20160128: SummarizedExperiment with 120 rows and 56 columns
 [5] UCS_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 56 columns
 [6] UCS_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 56 columns
 [7] UCS_Mutation-20160128: RaggedExperiment with 11339 rows and 57 columns
 [8] UCS_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 57 columns
 [9] UCS_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 57 columns
 [10] UCS_RPPAArray-20160128: SummarizedExperiment with 192 rows and 48 columns
 [11] UCS_Methylation-20160128: SummarizedExperiment with 485577 rows and 57 columns

> rownames( UCS )
CharacterList of length 11
[["UCS_CNASNP-20160128"]] character(0)
[["UCS_CNVSNP-20160128"]] character(0)
[["UCS_GISTIC_AllByGene-20160128"]] ACAP3 ...
[["UCS_GISTIC_Peaks-20160128"]] character(0)
[["UCS_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["UCS_miRNASeqGene-20160128"]] hsa-let-7a-1 ... hsa-mir-99b
[["UCS_Mutation-20160128"]] character(0)
[["UCS_RNASeq2Gene-20160128"]] A1BG A1CF ... psiTPTE22 tAKR
[["UCS_RNASeq2GeneNorm-20160128"]] A1BG A1CF ... tAKR
[["UCS_RPPAArray-20160128"]] 14-3-3_beta ...
...
<1 more element>

> colnames( UCS )
CharacterList of length 11
[["UCS_CNASNP-20160128"]] TCGA-N5-A4R8-10A-01D-A28T-01 ...
[["UCS_CNVSNP-20160128"]] TCGA-N5-A4R8-10A-01D-A28T-01 ...
[["UCS_GISTIC_AllByGene-20160128"]] TCGA-N5-A4RA-01A-11D-A2...
[["UCS_GISTIC_Peaks-20160128"]] TCGA-N5-A4RA-01A-11D-A28Q-0...
[["UCS_GISTIC_ThresholdedByGene-20160128"]] TCGA-N5-A4RA-01...
[["UCS_miRNASeqGene-20160128"]] TCGA-N5-A4R8-01A-11R-A28Z-1...
[["UCS_Mutation-20160128"]] TCGA-N5-A4R8-01A-11D-A28R-08 ...
[["UCS_RNASeq2Gene-20160128"]] TCGA-N5-A4R8-01A-11R-A28V-07...
[["UCS_RNASeq2GeneNorm-20160128"]] TCGA-N5-A4R8-01A-11R-A28...
[["UCS_RPPAArray-20160128"]] TCGA-N5-A4R8-01A-21-A41P-20 ...
...
<1 more element>

Sizes of each ExperimentList element:

                                   assay size.Mb
1                    UCS_CNASNP-20160128  1.5 Mb
2                    UCS_CNVSNP-20160128  0.6 Mb
3          UCS_GISTIC_AllByGene-20160128 15.5 Mb
4              UCS_GISTIC_Peaks-20160128  0.1 Mb
5  UCS_GISTIC_ThresholdedByGene-20160128 15.5 Mb
6              UCS_miRNASeqGene-20160128  0.6 Mb
7                  UCS_Mutation-20160128 19.7 Mb
8               UCS_RNASeq2Gene-20160128 11.5 Mb
9           UCS_RNASeq2GeneNorm-20160128 11.5 Mb
10                UCS_RPPAArray-20160128  0.1 Mb
11              UCS_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   22 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  35.00   35.00    1.43    1.04    1.96 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  51.00   62.00   68.00   69.72   76.00   90.00 

vital_status:
 0  1 
22 35 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   304.0   522.0   705.4   790.5  3115.0      22 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   591.2   828.0  1183.4  1647.5  4269.0      35 

tumor_tissue_site:
uterus 
    57 

gender:
female 
    57 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2002    2007    2009    2009    2011    2012 

radiation_therapy:
  no  yes NA's 
  29   25    3 

histological_type:
uterine carcinosarcoma/ malignant mixed mullerian tumor (mmmt): nos 
                                                                 24 
                    uterine carcinosarcoma/ mmmt: heterologous type 
                                                                 20 
                       uterine carcinosarcoma/mmmt: homologous type 
                                                                 13 

race:
                    asian black or african american 
                        3                         9 
                    white                      NA's 
                       44                         1 

ethnicity:
    hispanic or latino not hispanic or latino 
                     1                     43 
                  NA's 
                    13 

Including an additional 632 columns
}}
\keyword{datasets}

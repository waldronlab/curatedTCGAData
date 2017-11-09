\name{ UCS }
\alias{ UCS }
\docType{data}
\title{ Uterine Carcinosarcoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( UCS )
ExperimentList class object of length 9: 
 [1] CNASNP-20160128: RaggedExperiment with 54944 rows and 111 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 19298 rows and 111 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 56 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 56 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 57 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 56 columns 
 [7] Mutation-20160128: RaggedExperiment with 11339 rows and 57 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 57 columns 
 [9] RPPAArray-20160128: SummarizedExperiment with 192 rows and 48 columns 

> rownames( UCS )
CharacterList of length 9
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363

> colnames( UCS )
CharacterList of length 9
[["CNASNP-20160128"]] TCGA-N5-A4R8-10A-01D-A28T-01 ...
[["CNVSNP-20160128"]] TCGA-N5-A4R8-10A-01D-A28T-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-N5-A4RA-01A-11D-A28Q-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-N5-A4RA-01A-11D-A28Q-01 ...
[["Methylation-20160128"]] TCGA-N5-A4R8-01A-11D-A28S-05 ...
[["miRNASeqGene-20160128"]] TCGA-N5-A4R8-01A-11R-A28Z-13 ...
[["Mutation-20160128"]] TCGA-N5-A4R8-01A-11D-A28R-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-N5-A4R8-01A-11R-A28V-07 ...
[["RPPAArray-20160128"]] TCGA-N5-A4R8-01A-21-A41P-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  1.5 Mb
2                   CNVSNP-20160128  0.6 Mb
3         GISTIC_AllByGene-20160128  4.9 Mb
4 GISTIC_ThresholdedByGene-20160128  4.9 Mb
5              Methylation-20160128   75 Mb
6             miRNASeqGene-20160128  0.1 Mb
7                 Mutation-20160128 19.7 Mb
8          RNASeq2GeneNorm-20160128  1.3 Mb
9                RPPAArray-20160128    0 Mb

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
                    asian black or african american                     white 
                        3                         9                        44 
                     NA's 
                        1 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     1                     43                     13 

Including an additional 632 columns
}}
\keyword{datasets}

\name{DLBC-v2.1.0}
\alias{ DLBC-v2.1.0 }
\docType{data}
\title{ Lymphoid Neoplasm Diffuse Large B-cell Lymphoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( DLBC )
ExperimentList class object of length 11:
 [1] DLBC_CNASNP-20160128: RaggedExperiment with 44725 rows and 94 columns
 [2] DLBC_CNVSNP-20160128: RaggedExperiment with 9343 rows and 94 columns
 [3] DLBC_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 48 columns
 [4] DLBC_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 39 rows and 48 columns
 [5] DLBC_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 48 columns
 [6] DLBC_Mutation-20160128: RaggedExperiment with 16918 rows and 48 columns
 [7] DLBC_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 48 columns
 [8] DLBC_RPPAArray-20160128: SummarizedExperiment with 192 rows and 33 columns
 [9] DLBC_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 47 columns
 [10] DLBC_RNASeq2GeneNorm-20160128: SummarizedExperiment with 17665 rows and 48 columns
 [11] DLBC_Methylation-20160128: SummarizedExperiment with 485577 rows and 48 columns

> rownames( DLBC )
CharacterList of length 11
[["DLBC_CNASNP-20160128"]] character(0)
[["DLBC_CNVSNP-20160128"]] character(0)
[["DLBC_GISTIC_AllByGene-20160128"]] character(0)
[["DLBC_GISTIC_Peaks-20160128"]] 15 17 16 1 2 18 19 ... 36 37 12 38 39 13 40
[["DLBC_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["DLBC_Mutation-20160128"]] character(0)
[["DLBC_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["DLBC_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
[["DLBC_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["DLBC_RNASeq2GeneNorm-20160128"]] A1BG A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22
...
<1 more element>

> colnames( DLBC )
CharacterList of length 11
[["DLBC_CNASNP-20160128"]] TCGA-FA-8693-01A-11D-2389-01 ...
[["DLBC_CNVSNP-20160128"]] TCGA-FA-8693-01A-11D-2389-01 ...
[["DLBC_GISTIC_AllByGene-20160128"]] TCGA-FA-8693-01A-11D-2389-01 ...
[["DLBC_GISTIC_Peaks-20160128"]] TCGA-FA-8693-01A-11D-2389-01 ...
[["DLBC_GISTIC_ThresholdedByGene-20160128"]] TCGA-FA-8693-01A-11D-2389-01 ...
[["DLBC_Mutation-20160128"]] TCGA-FA-8693-01A-11D-2397-10 ...
[["DLBC_RNASeq2Gene-20160128"]] TCGA-FA-8693-01A-11R-2404-07 ...
[["DLBC_RPPAArray-20160128"]] TCGA-FA-8693-01A-21-A45K-20 ...
[["DLBC_miRNASeqGene-20160128"]] TCGA-FA-8693-01A-11R-2401-13 ...
[["DLBC_RNASeq2GeneNorm-20160128"]] TCGA-FA-8693-01 ... TCGA-VB-A8QN-01
...
<1 more element>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    DLBC_CNASNP-20160128  1.2 Mb
2                    DLBC_CNVSNP-20160128  0.3 Mb
3          DLBC_GISTIC_AllByGene-20160128 12.5 Mb
4              DLBC_GISTIC_Peaks-20160128  0.1 Mb
5  DLBC_GISTIC_ThresholdedByGene-20160128 12.4 Mb
6                  DLBC_Mutation-20160128 12.6 Mb
7               DLBC_RNASeq2Gene-20160128 10.1 Mb
8                 DLBC_RPPAArray-20160128  0.1 Mb
9              DLBC_miRNASeqGene-20160128  0.5 Mb
10          DLBC_RNASeq2GeneNorm-20160128  8.7 Mb
11              DLBC_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   39 observations deleted due to missingness 
     n events median 0.95LCL 0.95UCL
[1,] 9      9   1.63   0.858      NA


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.00   46.00   57.50   56.27   67.00   82.00 

vital_status:
 0  1 
39  9 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     58     313     595    1505    1252    6425      39 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     616     946    1328    1581    5980       9 


gender:
female   male 
    26     22 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1987    2009    2011    2009    2012    2013 

radiation_therapy:
  no  yes NA's 
  40    7    1 

histological_type:
diffuse large b-cell lymphoma (dlbcl) nos (any anatomic site nodal or extranodal) 
                                                                               41 
                                                         primary dlbcl of the cns 
                                                                                3 
                                               primary mediastinal (thymic) dlbcl 
                                                                                4 

race:
                    asian black or african american                     white 
                       18                         1                        29 

ethnicity:
    hispanic or latino not hispanic or latino 
                    12                     36 

Including an additional 607 columns
}}
\keyword{datasets}

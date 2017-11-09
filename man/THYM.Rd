\name{ THYM }
\alias{ THYM }
\docType{data}
\title{ Thymoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( THYM )
ExperimentList class object of length 9: 
 [1] CNASNP-20160128: RaggedExperiment with 105646 rows and 248 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 15571 rows and 248 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 123 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 123 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 126 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 126 columns 
 [7] Mutation-20160128: RaggedExperiment with 3064 rows and 123 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 122 columns 
 [9] RPPAArray-20160128: SummarizedExperiment with 192 rows and 90 columns 

> rownames( THYM )
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

> colnames( THYM )
CharacterList of length 9
[["CNASNP-20160128"]] TCGA-3G-AB0O-01A-22D-A422-01 ...
[["CNVSNP-20160128"]] TCGA-3G-AB0O-01A-22D-A422-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-3G-AB0O-01A-22D-A422-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-3G-AB0O-01A-22D-A422-01 ...
[["Methylation-20160128"]] TCGA-3G-AB0O-01A-22D-A424-05 ...
[["miRNASeqGene-20160128"]] TCGA-3G-AB0O-01A-22R-A42W-13 ...
[["Mutation-20160128"]] TCGA-3G-AB0O-01A-22D-A423-09 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-3G-AB0O-01A-22R-A42C-07 ...
[["RPPAArray-20160128"]] TCGA-3G-AB0O-01A-11-A45R-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  2.9 Mb
2                   CNVSNP-20160128  0.5 Mb
3         GISTIC_AllByGene-20160128  4.9 Mb
4 GISTIC_ThresholdedByGene-20160128  4.9 Mb
5              Methylation-20160128   75 Mb
6             miRNASeqGene-20160128  0.1 Mb
7                 Mutation-20160128  3.1 Mb
8          RNASeq2GeneNorm-20160128  1.3 Mb
9                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   115 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
   9.00    9.00    2.34    1.04      NA 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  17.00   49.50   60.00   58.15   68.50   84.00       1 

vital_status:
  0   1 
115   9 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    124     379     853    1423    2488    3488     115 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   725.2  1267.5  1486.3  1947.0  4575.0      10 

tumor_tissue_site:
anterior mediastinum               thymus 
                  27                   97 

gender:
female   male 
    60     64 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2000    2008    2010    2010    2012    2013       1 

radiation_therapy:
 no yes 
 81  43 


race:
                    asian black or african american                     white 
                       13                         6                       103 
                     NA's 
                        2 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    10                    100                     14 

Including an additional 685 columns
}}
\keyword{datasets}

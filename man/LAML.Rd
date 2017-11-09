\name{ LAML }
\alias{ LAML }
\docType{data}
\title{ Acute Myeloid Leukemia }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( LAML )
ExperimentList class object of length 7: 
 [1] CNASNP-20160128: RaggedExperiment with 874897 rows and 392 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 28324 rows and 380 columns 
 [3] Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 194 columns 
 [4] Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 194 columns 
 [5] Mutation-20160128: RaggedExperiment with 2585 rows and 197 columns 
 [6] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 173 columns 
 [7] RNASeqGene-20160128: SummarizedExperiment with 19990 rows and 179 columns 

> rownames( LAML )
CharacterList of length 7
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["Methylation_methyl27-20160128"]] cg00000292 cg00002426 ... cg27665659
[["Methylation_methyl450-20160128"]] cg00000029 cg00000108 ... rs9839873
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RNASeqGene-20160128"]] AADACL3 AADACL4 ABCA4 ABCB10 ... VCY XGPY2 XKRY2 ZFY

> colnames( LAML )
CharacterList of length 7
[["CNASNP-20160128"]] TCGA-AB-2802-03A-01D-0756-21 ...
[["CNVSNP-20160128"]] TCGA-AB-2803-03A-01D-0756-21 ...
[["Methylation_methyl27-20160128"]] TCGA-AB-2802-03A-01D-0741-05 ...
[["Methylation_methyl450-20160128"]] TCGA-AB-2802-03A-01D-0741-05 ...
[["Mutation-20160128"]] TCGA-AB-2802-03B-01W-0728-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-AB-2803-03A-01T-0734-13 ...
[["RNASeqGene-20160128"]] TCGA-AB-2803-03A-01T-0734-13 ...

Sizes of each ExperimentList element:

                           assay size.Mb
1                CNASNP-20160128 23.5 Mb
2                CNVSNP-20160128  0.9 Mb
3  Methylation_methyl27-20160128  4.9 Mb
4 Methylation_methyl450-20160128   75 Mb
5              Mutation-20160128  2.6 Mb
6       RNASeq2GeneNorm-20160128  1.3 Mb
7            RNASeqGene-20160128  1.3 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   80 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
120.000 120.000   0.748   0.586   0.918 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  18.00   44.75   57.00   55.02   67.00   88.00 

vital_status:
  0   1 
 67 133 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    91.5   273.0   355.7   489.0  1706.0      80 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   296.2   699.0   913.5  1506.8  2861.0     132 

tumor_tissue_site:
bone marrow        NA's 
        199           1 

gender:
female   male 
    91    109 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2001    2004    2006    2006    2008    2010 

race:
                    asian black or african american                     white 
                        2                        15                       181 
                     NA's 
                        2 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     3                    194                      3 

Including an additional 478 columns
}}
\keyword{datasets}

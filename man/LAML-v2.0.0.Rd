\name{LAML-v2.0.0}
\alias{ LAML-v2.0.0 }
\docType{data}
\title{ Acute Myeloid Leukemia }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( LAML )
ExperimentList class object of length 11:
 [1] LAML_CNASNP-20160128: RaggedExperiment with 874897 rows and 392 columns
 [2] LAML_CNVSNP-20160128: RaggedExperiment with 28324 rows and 380 columns
 [3] LAML_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 191 columns
 [4] LAML_GISTIC_Peaks-20160128: SummarizedExperiment with 32 rows and 191 columns
 [5] LAML_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 191 columns
 [6] LAML_Mutation-20160128: RaggedExperiment with 2585 rows and 197 columns
 [7] LAML_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 173 columns
 [8] LAML_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 173 columns
 [9] LAML_RNASeqGene-20160128: SummarizedExperiment with 19990 rows and 179 columns
 [10] LAML_Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 194 columns
 [11] LAML_Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 194 columns

> rownames( LAML )
CharacterList of length 11
[["LAML_CNASNP-20160128"]] character(0)
[["LAML_CNVSNP-20160128"]] character(0)
[["LAML_GISTIC_AllByGene-20160128"]] ACAP3 ...
[["LAML_GISTIC_Peaks-20160128"]] character(0)
[["LAML_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["LAML_Mutation-20160128"]] character(0)
[["LAML_RNASeq2Gene-20160128"]] A1BG A1CF ... psiTPTE22 tAKR
[["LAML_RNASeq2GeneNorm-20160128"]] A1BG A1CF ... tAKR
[["LAML_RNASeqGene-20160128"]] AADACL3 AADACL4 ... XKRY2 ZFY
[["LAML_Methylation_methyl27-20160128"]] cg00000292 ...
...
<1 more element>

> colnames( LAML )
CharacterList of length 11
[["LAML_CNASNP-20160128"]] TCGA-AB-2802-03A-01D-0756-21 ...
[["LAML_CNVSNP-20160128"]] TCGA-AB-2803-03A-01D-0756-21 ...
[["LAML_GISTIC_AllByGene-20160128"]] TCGA-AB-2803-03A-01D-0...
[["LAML_GISTIC_Peaks-20160128"]] TCGA-AB-2803-03A-01D-0756-...
[["LAML_GISTIC_ThresholdedByGene-20160128"]] TCGA-AB-2803-0...
[["LAML_Mutation-20160128"]] TCGA-AB-2802-03B-01W-0728-08 ...
[["LAML_RNASeq2Gene-20160128"]] TCGA-AB-2803-03A-01T-0734-1...
[["LAML_RNASeq2GeneNorm-20160128"]] TCGA-AB-2803-03A-01T-07...
[["LAML_RNASeqGene-20160128"]] TCGA-AB-2803-03A-01T-0734-13...
[["LAML_Methylation_methyl27-20160128"]] TCGA-AB-2802-03A-0...
...
<1 more element>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    LAML_CNASNP-20160128 23.5 Mb
2                    LAML_CNVSNP-20160128  0.9 Mb
3          LAML_GISTIC_AllByGene-20160128   41 Mb
4              LAML_GISTIC_Peaks-20160128  0.1 Mb
5  LAML_GISTIC_ThresholdedByGene-20160128   41 Mb
6                  LAML_Mutation-20160128  2.8 Mb
7               LAML_RNASeq2Gene-20160128 29.6 Mb
8           LAML_RNASeq2GeneNorm-20160128 29.6 Mb
9                LAML_RNASeqGene-20160128 29.8 Mb
10     LAML_Methylation_methyl27-20160128  4.9 Mb
11    LAML_Methylation_methyl450-20160128   75 Mb

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
                    asian black or african american 
                        2                        15 
                    white                      NA's 
                      181                         2 

ethnicity:
    hispanic or latino not hispanic or latino 
                     3                    194 
                  NA's 
                     3 

Including an additional 478 columns
}}
\keyword{datasets}

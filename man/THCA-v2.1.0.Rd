\name{THCA-v2.1.0}
\alias{ THCA-v2.1.0 }
\docType{data}
\title{ Thyroid carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( THCA )
ExperimentList class object of length 13:
 [1] THCA_CNASeq-20160128: RaggedExperiment with 5953 rows and 203 columns
 [2] THCA_CNASNP-20160128: RaggedExperiment with 389998 rows and 1013 columns
 [3] THCA_CNVSNP-20160128: RaggedExperiment with 55117 rows and 1013 columns
 [4] THCA_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 499 columns
 [5] THCA_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 38 rows and 499 columns
 [6] THCA_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 499 columns
 [7] THCA_Mutation-20160128: RaggedExperiment with 7458 rows and 405 columns
 [8] THCA_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 568 columns
 [9] THCA_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 4 columns
 [10] THCA_RPPAArray-20160128: SummarizedExperiment with 175 rows and 224 columns
 [11] THCA_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 569 columns
 [12] THCA_RNASeq2GeneNorm-20160128: SummarizedExperiment with 18003 rows and 568 columns
 [13] THCA_Methylation-20160128: SummarizedExperiment with 485577 rows and 567 columns

> rownames( THCA )
CharacterList of length 13
[["THCA_CNASeq-20160128"]] character(0)
[["THCA_CNASNP-20160128"]] character(0)
[["THCA_CNVSNP-20160128"]] character(0)
[["THCA_GISTIC_AllByGene-20160128"]] character(0)
[["THCA_GISTIC_Peaks-20160128"]] 1 10 11 12 2 13 3 14 ... 36 37 38 39 7 40 41
[["THCA_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["THCA_Mutation-20160128"]] character(0)
[["THCA_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["THCA_RNASeqGene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["THCA_RPPAArray-20160128"]] 14-3-3_epsilon 4E-BP1 ... p90RSK_pT359_S363
...
<3 more elements>

> colnames( THCA )
CharacterList of length 13
[["THCA_CNASeq-20160128"]] TCGA-BJ-A0Z2-01A-11D-A10R-02 ...
[["THCA_CNASNP-20160128"]] TCGA-4C-A93U-01A-11D-A396-01 ...
[["THCA_CNVSNP-20160128"]] TCGA-4C-A93U-01A-11D-A396-01 ...
[["THCA_GISTIC_AllByGene-20160128"]] TCGA-4C-A93U-01A-11D-A396-01 ...
[["THCA_GISTIC_Peaks-20160128"]] TCGA-4C-A93U-01A-11D-A396-01 ...
[["THCA_GISTIC_ThresholdedByGene-20160128"]] TCGA-4C-A93U-01A-11D-A396-01 ...
[["THCA_Mutation-20160128"]] TCGA-BJ-A0YZ-01A-11D-A10S-08 ...
[["THCA_RNASeq2Gene-20160128"]] TCGA-4C-A93U-01A-11R-A39I-07 ...
[["THCA_RNASeqGene-20160128"]] TCGA-DJ-A1QE-01A-21R-A14Y-07 ...
[["THCA_RPPAArray-20160128"]] TCGA-BJ-A0YZ-01A-21-A21L-20 ...
...
<3 more elements>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    THCA_CNASeq-20160128  0.2 Mb
2                    THCA_CNASNP-20160128 10.7 Mb
3                    THCA_CNVSNP-20160128  1.8 Mb
4          THCA_GISTIC_AllByGene-20160128 97.8 Mb
5              THCA_GISTIC_Peaks-20160128  0.3 Mb
6  THCA_GISTIC_ThresholdedByGene-20160128 97.7 Mb
7                  THCA_Mutation-20160128   14 Mb
8               THCA_RNASeq2Gene-20160128 91.5 Mb
9                THCA_RNASeqGene-20160128  3.2 Mb
10                THCA_RPPAArray-20160128  0.4 Mb
11             THCA_miRNASeqGene-20160128  4.8 Mb
12          THCA_RNASeq2GeneNorm-20160128 80.3 Mb
13              THCA_Methylation-20160128 75.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   487 observations deleted due to missingness 
      n events median 0.95LCL 0.95UCL
[1,] 16     16    2.8    2.23     4.8


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.00   35.00   46.00   47.26   58.00   89.00 

vital_status:
  0   1 
487  16 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    174     743    1021    1176    1631    2973     487 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     535     943    1220    1513    5423      16 

tumor_tissue_site:
thyroid 
    503 



pathology_N_stage:
 n0  n1 n1a n1b  nx 
227  58  93  75  50 

pathology_M_stage:
  m0   m1   mx NA's 
 280    9  213    1 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1993    2008    2010    2009    2011    2013 

radiation_therapy:
  no  yes NA's 
 181  306   16 

radiation_exposure:
  no  yes NA's 
 423   17   63 

extrathyroidal_extension:
           minimal (t3) moderate/advanced (t4a)                    none 
                    133                      18                     333 
    very advanced (t4b)                    NA's 
                      1                      18 

residual_tumor:
  r0   r1   r2   rx NA's 
 385   52    4   30   32 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   3.658   5.000  41.000     114 

multifocality:
multifocal   unifocal       NA's 
       227        266         10 

tumor_size:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.700   2.600   2.975   4.000   8.200     100 

Including an additional 1481 columns
}}
\keyword{datasets}

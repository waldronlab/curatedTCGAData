\name{LGG-v2.1.0}
\alias{ LGG-v2.1.0 }
\docType{data}
\title{ Brain Lower Grade Glioma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( LGG )
ExperimentList class object of length 13:
 [1] LGG_CNASeq-20160128: RaggedExperiment with 6360 rows and 104 columns
 [2] LGG_CNASNP-20160128: RaggedExperiment with 411918 rows and 1015 columns
 [3] LGG_CNVSNP-20160128: RaggedExperiment with 79791 rows and 1015 columns
 [4] LGG_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 513 columns
 [5] LGG_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 46 rows and 513 columns
 [6] LGG_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 513 columns
 [7] LGG_mRNAArray-20160128: SummarizedExperiment with 17814 rows and 27 columns
 [8] LGG_Mutation-20160128: RaggedExperiment with 9885 rows and 286 columns
 [9] LGG_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 530 columns
 [10] LGG_RPPAArray-20160128: SummarizedExperiment with 201 rows and 435 columns
 [11] LGG_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 526 columns
 [12] LGG_RNASeq2GeneNorm-20160128: SummarizedExperiment with 18311 rows and 530 columns
 [13] LGG_Methylation-20160128: SummarizedExperiment with 485577 rows and 530 columns

> rownames( LGG )
CharacterList of length 13
[["LGG_CNASeq-20160128"]] character(0)
[["LGG_CNASNP-20160128"]] character(0)
[["LGG_CNVSNP-20160128"]] character(0)
[["LGG_GISTIC_AllByGene-20160128"]] character(0)
[["LGG_GISTIC_Peaks-20160128"]] 21 22 1 2 23 3 24 25 ... 45 16 17 46 18 47 48
[["LGG_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["LGG_mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 PNMA1 ... SNRPD2 AQP7 CTSC
[["LGG_Mutation-20160128"]] character(0)
[["LGG_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["LGG_RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363
...
<3 more elements>

> colnames( LGG )
CharacterList of length 13
[["LGG_CNASeq-20160128"]] TCGA-CS-4938-01B-11D-1891-02 ...
[["LGG_CNASNP-20160128"]] TCGA-CS-4938-01B-11D-1892-01 ...
[["LGG_CNVSNP-20160128"]] TCGA-CS-4938-01B-11D-1892-01 ...
[["LGG_GISTIC_AllByGene-20160128"]] TCGA-CS-4938-01B-11D-1892-01 ...
[["LGG_GISTIC_Peaks-20160128"]] TCGA-CS-4938-01B-11D-1892-01 ...
[["LGG_GISTIC_ThresholdedByGene-20160128"]] TCGA-CS-4938-01B-11D-1892-01 ...
[["LGG_mRNAArray-20160128"]] TCGA-CS-4942-01A-01R-1470-07 ...
[["LGG_Mutation-20160128"]] TCGA-CS-4938-01B-11D-1893-08 ...
[["LGG_RNASeq2Gene-20160128"]] TCGA-CS-4938-01B-11R-1896-07 ...
[["LGG_RPPAArray-20160128"]] TCGA-CS-4938-01B-11-A300-20 ...
...
<3 more elements>

Sizes of each ExperimentList element:

                                   assay  size.Mb
1                    LGG_CNASeq-20160128   0.2 Mb
2                    LGG_CNASNP-20160128  11.3 Mb
3                    LGG_CNVSNP-20160128   2.4 Mb
4          LGG_GISTIC_AllByGene-20160128 100.5 Mb
5              LGG_GISTIC_Peaks-20160128   0.3 Mb
6  LGG_GISTIC_ThresholdedByGene-20160128 100.4 Mb
7                 LGG_mRNAArray-20160128   5.9 Mb
8                  LGG_Mutation-20160128   4.2 Mb
9               LGG_RNASeq2Gene-20160128  85.5 Mb
10                LGG_RPPAArray-20160128   0.8 Mb
11             LGG_miRNASeqGene-20160128   4.4 Mb
12          LGG_RNASeq2GeneNorm-20160128  76.4 Mb
13              LGG_Methylation-20160128  75.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   391 observations deleted due to missingness 
       n events median 0.95LCL 0.95UCL
[1,] 125    125   2.23    1.87    2.83


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   32.00   41.00   42.93   53.00   86.00       2 

vital_status:
   0    1 NA's 
 389  126    1 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      7     438     814    1219    1547    5166     391 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -1.0   384.0   629.0   880.1  1147.0  6423.0     127 

tumor_tissue_site:
central nervous system                   NA's 
                   515                      1 

gender:
female   male   NA's 
   230    285      1 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2008    2011    2009    2012    2013       1 

radiation_therapy:
  no  yes NA's 
 186  296   34 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   80.00   90.00   86.64  100.00  100.00     209 

histological_type:
      astrocytoma  oligoastrocytoma oligodendroglioma              NA's 
              194               130               191                 1 

race:
american indian or alaska native                            asian 
                               1                                8 
       black or african american                            white 
                              21                              475 
                            NA's 
                              11 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    32                    449                     35 

Including an additional 1764 columns
}}
\keyword{datasets}

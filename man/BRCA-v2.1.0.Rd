\name{BRCA-v2.1.0}
\alias{ BRCA-v2.1.0 }
\docType{data}
\title{ Breast invasive carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( BRCA )
ExperimentList class object of length 15:
 [1] BRCA_CNASeq-20160128: RaggedExperiment with 5298 rows and 38 columns
 [2] BRCA_CNASNP-20160128: RaggedExperiment with 1132786 rows and 2209 columns
 [3] BRCA_CNVSNP-20160128: RaggedExperiment with 284458 rows and 2199 columns
 [4] BRCA_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 1080 columns
 [5] BRCA_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 70 rows and 1080 columns
 [6] BRCA_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 1080 columns
 [7] BRCA_mRNAArray-20160128: SummarizedExperiment with 17814 rows and 590 columns
 [8] BRCA_Mutation-20160128: RaggedExperiment with 90490 rows and 993 columns
 [9] BRCA_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 1212 columns
 [10] BRCA_RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 878 columns
 [11] BRCA_RPPAArray-20160128: SummarizedExperiment with 226 rows and 937 columns
 [12] BRCA_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 849 columns
 [13] BRCA_RNASeq2GeneNorm-20160128: SummarizedExperiment with 18300 rows and 1212 columns
 [14] BRCA_Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 343 columns
 [15] BRCA_Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 885 columns

> rownames( BRCA )
CharacterList of length 15
[["BRCA_CNASeq-20160128"]] character(0)
[["BRCA_CNASNP-20160128"]] character(0)
[["BRCA_CNVSNP-20160128"]] character(0)
[["BRCA_GISTIC_AllByGene-20160128"]] character(0)
[["BRCA_GISTIC_Peaks-20160128"]] 29 30 1 2 31 3 32 4 ... 66 27 67 28 68 69 70
[["BRCA_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["BRCA_mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 PNMA1 ... SNRPD2 AQP7 CTSC
[["BRCA_Mutation-20160128"]] character(0)
[["BRCA_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["BRCA_RNASeqGene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
...
<5 more elements>

> colnames( BRCA )
CharacterList of length 15
[["BRCA_CNASeq-20160128"]] TCGA-A2-A0EU-01A-22D-A060-02 ...
[["BRCA_CNASNP-20160128"]] TCGA-3C-AAAU-01A-11D-A41E-01 ...
[["BRCA_CNVSNP-20160128"]] TCGA-3C-AAAU-01A-11D-A41E-01 ...
[["BRCA_GISTIC_AllByGene-20160128"]] TCGA-3C-AAAU-01A-11D-A41E-01 ...
[["BRCA_GISTIC_Peaks-20160128"]] TCGA-3C-AAAU-01A-11D-A41E-01 ...
[["BRCA_GISTIC_ThresholdedByGene-20160128"]] TCGA-3C-AAAU-01A-11D-A41E-01 ...
[["BRCA_mRNAArray-20160128"]] TCGA-A1-A0SD-01A-11R-A115-07 ...
[["BRCA_Mutation-20160128"]] TCGA-A1-A0SB-01A-11D-A142-09 ...
[["BRCA_RNASeq2Gene-20160128"]] TCGA-3C-AAAU-01A-11R-A41B-07 ...
[["BRCA_RNASeqGene-20160128"]] TCGA-A1-A0SB-01A-11R-A144-07 ...
...
<5 more elements>

Sizes of each ExperimentList element:

                                    assay  size.Mb
1                    BRCA_CNASeq-20160128   0.2 Mb
2                    BRCA_CNASNP-20160128  30.9 Mb
3                    BRCA_CNVSNP-20160128   8.2 Mb
4          BRCA_GISTIC_AllByGene-20160128 207.9 Mb
5              BRCA_GISTIC_Peaks-20160128   0.8 Mb
6  BRCA_GISTIC_ThresholdedByGene-20160128 207.7 Mb
7                 BRCA_mRNAArray-20160128  82.5 Mb
8                  BRCA_Mutation-20160128  67.4 Mb
9               BRCA_RNASeq2Gene-20160128 192.3 Mb
10               BRCA_RNASeqGene-20160128   140 Mb
11                BRCA_RPPAArray-20160128   1.8 Mb
12             BRCA_miRNASeqGene-20160128   7.1 Mb
13          BRCA_RNASeq2GeneNorm-20160128 171.7 Mb
14     BRCA_Methylation_methyl27-20160128   4.9 Mb
15    BRCA_Methylation_methyl450-20160128  75.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   947 observations deleted due to missingness 
       n events median 0.95LCL 0.95UCL
[1,] 151    151   3.48    2.83     4.5


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   26.0    49.0    59.0    58.6    68.0    90.0      16 

vital_status:
   0    1 NA's 
 945  152    1 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  116.0   700.5  1272.0  1644.7  2367.0  7455.0     947 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -7     440     761    1183    1572    8605     153 

tumor_tissue_site:
breast   NA's 
  1097      1 




pathology_M_stage:
cm0 (i+)       m0       m1       mx     NA's 
       6      906       22      163        1 

gender:
female   male   NA's 
  1085     12      1 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1988    2007    2009    2008    2010    2013       3 

days_to_last_known_alive:
 735 2576 NA's 
   1    1 1096 

radiation_therapy:
  no  yes NA's 
 446  556   96 


number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.363   2.000  35.000     169 

race:
american indian or alaska native                            asian 
                               1                               61 
       black or african american                            white 
                             183                              757 
                            NA's 
                              96 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    39                    884                    175 

Including an additional 2667 columns
}}
\keyword{datasets}

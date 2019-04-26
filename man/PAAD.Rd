\name{ PAAD }
\alias{ PAAD }
\docType{data}
\title{ Pancreatic adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( PAAD )
ExperimentList class object of length 10: 
 [1] PAAD_CNASNP-20160128: RaggedExperiment with 203871 rows and 368 columns 
 [2] PAAD_CNVSNP-20160128: RaggedExperiment with 34808 rows and 365 columns 
 [3] PAAD_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 184 columns 
 [4] PAAD_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 52 rows and 184 columns 
 [5] PAAD_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 184 columns 
 [6] PAAD_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 183 columns 
 [7] PAAD_Mutation-20160128: RaggedExperiment with 30357 rows and 150 columns 
 [8] PAAD_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 183 columns 
 [9] PAAD_RPPAArray-20160128: SummarizedExperiment with 195 rows and 123 columns 
 [10] PAAD_Methylation-20160128: SummarizedExperiment with 485577 rows and 195 columns 

> rownames( PAAD )
CharacterList of length 10
[["PAAD_CNASNP-20160128"]] character(0)
[["PAAD_CNVSNP-20160128"]] character(0)
[["PAAD_GISTIC_AllByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["PAAD_GISTIC_Peaks-20160128"]] chr1:26795113-27650365 ...
[["PAAD_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["PAAD_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["PAAD_Mutation-20160128"]] character(0)
[["PAAD_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
[["PAAD_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
[["PAAD_Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873

> colnames( PAAD )
CharacterList of length 10
[["PAAD_CNASNP-20160128"]] TCGA-2J-AAB1-01A-11D-A40V-01 ...
[["PAAD_CNVSNP-20160128"]] TCGA-2J-AAB1-01A-11D-A40V-01 ...
[["PAAD_GISTIC_AllByGene-20160128"]] TCGA-2J-AAB1-01A-11D-A40V-01 ...
[["PAAD_GISTIC_Peaks-20160128"]] TCGA-2J-AAB1-01A-11D-A40V-01 ...
[["PAAD_GISTIC_ThresholdedByGene-20160128"]] TCGA-2J-AAB1-01A-11D-A40V-01 ...
[["PAAD_miRNASeqGene-20160128"]] TCGA-2J-AAB1-01A-11R-A41G-13 ...
[["PAAD_Mutation-20160128"]] TCGA-2J-AAB1-01A-11D-A40W-08 ...
[["PAAD_RNASeq2GeneNorm-20160128"]] TCGA-2J-AAB1-01A-11R-A41B-07 ...
[["PAAD_RPPAArray-20160128"]] TCGA-2J-AAB4-01A-21-A43K-20 ...
[["PAAD_Methylation-20160128"]] TCGA-2J-AAB1-01A-11D-A40Y-05 ...

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    PAAD_CNASNP-20160128  5.6 Mb
2                    PAAD_CNVSNP-20160128    1 Mb
3          PAAD_GISTIC_AllByGene-20160128  4.9 Mb
4              PAAD_GISTIC_Peaks-20160128  0.1 Mb
5  PAAD_GISTIC_ThresholdedByGene-20160128  4.9 Mb
6              PAAD_miRNASeqGene-20160128  0.1 Mb
7                  PAAD_Mutation-20160128  111 Mb
8           PAAD_RNASeq2GeneNorm-20160128  1.3 Mb
9                 PAAD_RPPAArray-20160128    0 Mb
10              PAAD_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   85 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
100.000 100.000   1.079   0.967   1.315 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  35.00   57.00   65.00   64.86   73.00   88.00 

vital_status:
  0   1 
 85 100 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   228.5   394.0   459.5   596.5  2182.0      85 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   338.0   517.0   692.4   951.0  2741.0     100 

tumor_tissue_site:
pancreas 
     185 



pathology_N_stage:
  n0   n1  n1b   nx NA's 
  50  126    4    4    1 

pathology_M_stage:
m0 m1 mx 
85  5 95 

gender:
female   male 
    83    102 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2001    2010    2012    2011    2012    2013       1 

radiation_therapy:
  no  yes NA's 
 125   45   15 

histological_type:
             pancreas-adenocarcinoma ductal type 
                                             154 
           pancreas-adenocarcinoma-other subtype 
                                              25 
pancreas-colloid (mucinous non-cystic) carcinoma 
                                               4 
             pancreas-undifferentiated carcinoma 
                                               1 
                                            NA's 
                                               1 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.30   15.00   25.00   26.84   40.00   75.00     128 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1948    1960    1971    1971    1982    1993     138 

residual_tumor:
  r0   r1   r2   rx NA's 
 111   53    5    4   12 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   2.989   4.000  16.000       4 

race:
                    asian black or african american                     white 
                       11                         7                       162 
                     NA's 
                        5 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     5                    137                     43 

Including an additional 960 columns
}}
\keyword{datasets}

\name{ CHOL }
\alias{ CHOL }
\docType{data}
\title{ Cholangiocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( CHOL )
ExperimentList class object of length 9: 
 [1] CNASNP-20160128: RaggedExperiment with 39698 rows and 85 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 7570 rows and 85 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 36 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 36 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 45 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 45 columns 
 [7] Mutation-20160128: RaggedExperiment with 6755 rows and 35 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 45 columns 
 [9] RPPAArray-20160128: SummarizedExperiment with 192 rows and 30 columns 

> rownames( CHOL )
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

> colnames( CHOL )
CharacterList of length 9
[["CNASNP-20160128"]] TCGA-3X-AAV9-01A-72D-A416-01 ...
[["CNVSNP-20160128"]] TCGA-3X-AAV9-01A-72D-A416-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-3X-AAV9-01A-72D-A416-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-3X-AAV9-01A-72D-A416-01 ...
[["Methylation-20160128"]] TCGA-3X-AAV9-01A-72D-A418-05 ...
[["miRNASeqGene-20160128"]] TCGA-3X-AAV9-01A-72R-A41D-13 ...
[["Mutation-20160128"]] TCGA-3X-AAV9-01A-72D-A417-09 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-3X-AAV9-01A-72R-A41I-07 ...
[["RPPAArray-20160128"]] TCGA-3X-AAV9-01A-81-A45N-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  1.1 Mb
2                   CNVSNP-20160128  0.2 Mb
3         GISTIC_AllByGene-20160128  4.9 Mb
4 GISTIC_ThresholdedByGene-20160128  4.9 Mb
5              Methylation-20160128   75 Mb
6             miRNASeqGene-20160128  0.1 Mb
7                 Mutation-20160128  4.6 Mb
8          RNASeq2GeneNorm-20160128  1.3 Mb
9                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   18 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 18.000  18.000   1.370   0.742   2.532 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  29.00   56.50   66.50   63.03   72.00   82.00 

vital_status:
 0  1 
18 18 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0   195.2   500.0   592.9   731.0  1939.0      18 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   539.8   942.5   936.6  1394.0  1976.0      18 

tumor_tissue_site:
bile duct 
       36 


pathology_T_stage:
 t1  t2 t2a t2b  t3 
 19   6   2   4   5 

pathology_N_stage:
n0 n1 nx 
26  5  5 

pathology_M_stage:
m0 m1 mx 
28  5  3 

gender:
female   male 
    20     16 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2005    2010    2011    2010    2012    2013 

radiation_therapy:
  no NA's 
  35    1 

histological_type:
         cholangiocarcinoma; distal cholangiocarcinoma; hilar/perihilar 
                                  2                                   4 
   cholangiocarcinoma; intrahepatic 
                                 30 

residual_tumor:
r0 r1 rx 
28  5  3 

race:
                    asian black or african american                     white 
                        3                         2                        31 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     2                     33                      1 

Including an additional 622 columns
}}
\keyword{datasets}

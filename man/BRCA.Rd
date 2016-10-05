\name{ BRCA }
\alias{ BRCA }
\docType{data}
\title{ BRCA }

\details{
\preformatted{
> BRCA
A MultiAssayExperiment object of 8 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 8: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 878 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 1212 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 849 columns 
 [4] Methylation: SummarizedExperiment with 485577 rows and 885 columns 
 [5] mRNAArray: ExpressionSet with 17814 rows and 590 columns 
 [6] RPPAArray: ExpressionSet with 142 rows and 410 columns 
 [7] gistica: SummarizedExperiment with 24776 rows and 1080 columns 
 [8] gistict: SummarizedExperiment with 24776 rows and 1080 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( BRCA )
CharacterList of length 8
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_epsilon 4E-BP1 ... p70S6K_pT389 p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( BRCA )
CharacterList of length 8
[["RNASeqGene"]] TCGA-A1-A0SB-01A TCGA-A1-A0SD-01A ... TCGA-GM-A2DK-01A
[["RNASeq2GeneNorm"]] TCGA-3C-AAAU-01A TCGA-3C-AALI-01A ... TCGA-Z7-A8R6-01A
[["miRNASeqGene"]] TCGA-3C-AAAU-01A TCGA-3C-AALI-01A ... TCGA-Z7-A8R6-01A
[["Methylation"]] TCGA-3C-AAAU-01A-11D-A41Q-05 ... TCGA-Z7-A8R6-01A-11D-A41Q-05
[["mRNAArray"]] TCGA-A1-A0SD-01A TCGA-A1-A0SE-01A ... TCGA-E2-A1BD-01A
[["RPPAArray"]] TCGA-A1-A0SH-01A TCGA-A1-A0SJ-01A ... TCGA-E2-A1BD-01A
[["gistica"]] TCGA-3C-AAAU-01A-11D-A41E-01 ... TCGA-Z7-A8R6-01A-11D-A41E-01
[["gistict"]] TCGA-3C-AAAU-01A-11D-A41E-01 ... TCGA-Z7-A8R6-01A-11D-A41E-01

Sizes of each ExperimentList element:

            assay size.Mb
1      RNASeqGene  1.3 Mb
2 RNASeq2GeneNorm  1.3 Mb
3    miRNASeqGene  0.2 Mb
4     Methylation 67.5 Mb
5       mRNAArray  1.1 Mb
6       RPPAArray  0.1 Mb
7         gistica  4.4 Mb
8         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   947 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 151.00  151.00    3.48    2.83    4.50 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
 1098 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.00   49.00   59.00   58.58   68.00   90.00      15 

vital_status: 
  0   1 
946 152 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  116.0   700.5  1272.0  1645.0  2367.0  7455.0     947 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -31.0   434.2   761.5  1186.0  1583.0  8605.0     104 

tumor_tissue_site: 
breast 
  1098 




pathology_M_stage: 
cm0 (i+)       m0       m1       mx 
       6      907       22      163 

gender: 
female   male 
  1086     12 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1988    2007    2009    2008    2010    2013       2 

days_to_last_known_alive: 
 735 2576 NA's 
   1    1 1096 

radiation_therapy: 
  no  yes NA's 
 446  556   96 


number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    1.00    2.36    2.00   35.00     168 

race: 
american indian or alaska native                            asian 
                               1                               61 
       black or african american                            white 
                             183                              758 
                            NA's 
                              95 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    39                    885                    174 

}}
\keyword{datasets}

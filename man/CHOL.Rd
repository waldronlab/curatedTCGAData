\name{ CHOL }
\alias{ CHOL }
\docType{data}
\title{ CHOL }

\details{
\preformatted{
> CHOL
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 45 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 45 columns 
 [3] CNASNP: RangedRaggedAssay with 39698 rows and 85 columns 
 [4] CNVSNP: RangedRaggedAssay with 7570 rows and 85 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 45 columns 
 [6] RPPAArray: ExpressionSet with 192 rows and 30 columns 
 [7] Mutations: RangedRaggedAssay with 6755 rows and 35 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 36 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 36 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( CHOL )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 8 ... 39692 39693 39694 39695 39696 39697 39698
[["CNVSNP"]] 1 2 3 4 5 6 7 8 9 ... 7562 7563 7564 7565 7566 7567 7568 7569 7570
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] MAGEC1 GPS1 PI4KA DLL1 GREB1 CAV1 ... . . TBPL2 LILRB5 RABGAP1
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( CHOL )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-3X-AAV9-01A TCGA-3X-AAVA-01A ... TCGA-ZU-A8S4-01A
[["miRNASeqGene"]] TCGA-3X-AAV9-01A TCGA-3X-AAVA-01A ... TCGA-ZU-A8S4-01A
[["CNASNP"]] TCGA-3X-AAV9-01A-72D-A416-01 ... TCGA-ZU-A8S4-11A-11D-A416-01
[["CNVSNP"]] TCGA-3X-AAV9-01A-72D-A416-01 ... TCGA-ZU-A8S4-11A-11D-A416-01
[["Methylation"]] TCGA-3X-AAV9-01A-72D-A418-05 ... TCGA-ZU-A8S4-01A-11D-A418-05
[["RPPAArray"]] TCGA-3X-AAV9-01A TCGA-3X-AAVA-01A ... TCGA-ZH-A8Y8-01A
[["Mutations"]] TCGA-3X-AAV9-01A-72D-A417-09 ... TCGA-ZU-A8S4-01A-11D-A417-09
[["gistica"]] TCGA-3X-AAV9-01A-72D-A416-01 ... TCGA-ZU-A8S4-01A-11D-A416-01
[["gistict"]] TCGA-3X-AAV9-01A-72D-A416-01 ... TCGA-ZU-A8S4-01A-11D-A416-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.1 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP  3.1 Mb
4          CNVSNP  0.6 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7       Mutations  4.2 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   18 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 18.000  18.000   1.370   0.742   2.532 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
   36 

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
    0.0   196.5   534.0   616.0   976.8  1976.0      16 

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

}}
\keyword{datasets}

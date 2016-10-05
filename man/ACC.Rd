\name{ ACC }
\alias{ ACC }
\docType{data}
\title{ ACC }

\details{
\preformatted{
> ACC
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 79 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 80 columns 
 [3] CNASNP: RangedRaggedAssay with 79861 rows and 180 columns 
 [4] CNVSNP: RangedRaggedAssay with 21052 rows and 180 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 80 columns 
 [6] RPPAArray: ExpressionSet with 192 rows and 46 columns 
 [7] Mutations: RangedRaggedAssay with 20166 rows and 90 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 90 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 90 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( ACC )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 8 ... 79855 79856 79857 79858 79859 79860 79861
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 21046 21047 21048 21049 21050 21051 21052
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] PTCHD2 VPS13D PHC2 LCE1B LCE1A ... FZD1 AGPAT2 C16orf3 ZNF814
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( ACC )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-OR-A5J1-01A TCGA-OR-A5J2-01A ... TCGA-PK-A5HB-01A
[["miRNASeqGene"]] TCGA-OR-A5J1-01A TCGA-OR-A5J2-01A ... TCGA-PK-A5HB-01A
[["CNASNP"]] TCGA-OR-A5J1-01A-11D-A29H-01 ... TCGA-PK-A5HC-11A-11D-A309-01
[["CNVSNP"]] TCGA-OR-A5J1-01A-11D-A29H-01 ... TCGA-PK-A5HC-11A-11D-A309-01
[["Methylation"]] TCGA-OR-A5J1-01A-11D-A29J-05 ... TCGA-PK-A5HB-01A-11D-A29J-05
[["RPPAArray"]] TCGA-OR-A5J2-01A TCGA-OR-A5J3-01A ... TCGA-PK-A5HA-01A
[["Mutations"]] TCGA-OR-A5J1-01A-11D-A29I-10 ... TCGA-PK-A5HC-01A-11D-A30A-10
[["gistica"]] TCGA-OR-A5J1-01A-11D-A29H-01 ... TCGA-PK-A5HC-01A-11D-A309-01
[["gistict"]] TCGA-OR-A5J1-01A-11D-A29H-01 ... TCGA-PK-A5HC-01A-11D-A309-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.1 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP  6.2 Mb
4          CNVSNP  1.7 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7       Mutations 11.8 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   58 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  34.00   34.00    1.51    1.34    3.03 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
   92 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.00   35.50   48.50   47.16   60.00   83.00 

vital_status: 
 0  1 
58 34 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   399.0   551.5   854.6  1202.0  2405.0      58 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    253     885    1414    1738    2331    4673      31 

tumor_tissue_site: 
adrenal 
     92 

pathologic_stage: 
  stage i  stage ii stage iii  stage iv      NA's 
        9        44        19        18         2 

pathology_T_stage: 
  t1   t2   t3   t4 NA's 
   9   49   11   21    2 

pathology_N_stage: 
  n0   n1 NA's 
  80   10    2 

gender: 
female   male 
    60     32 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1997    2005    2008    2007    2011    2012 

radiation_therapy: 
  no  yes NA's 
  71   18    3 

histological_type: 
   adrenocortical carcinoma- myxoid type 
                                       1 
adrenocortical carcinoma- oncocytic type 
                                       4 
    adrenocortical carcinoma- usual type 
                                      87 

residual_tumor: 
  r0   r1   r2   rx NA's 
  64    7   12    6    3 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.548   0.500  52.000      61 

race: 
                    asian black or african american                     white 
                        2                         1                        78 
                     NA's 
                       11 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     8                     40                     44 

}}
\keyword{datasets}

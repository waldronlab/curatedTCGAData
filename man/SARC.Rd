\name{ SARC }
\alias{ SARC }
\docType{data}
\title{ SARC }

\details{
\preformatted{
> SARC
A MultiAssayExperiment object of 8 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 8: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 265 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 263 columns 
 [3] CNASNP: RangedRaggedAssay with 337377 rows and 516 columns 
 [4] CNVSNP: RangedRaggedAssay with 106739 rows and 513 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 269 columns 
 [6] RPPAArray: ExpressionSet with 192 rows and 226 columns 
 [7] gistica: SummarizedExperiment with 24776 rows and 256 columns 
 [8] gistict: SummarizedExperiment with 24776 rows and 256 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( SARC )
CharacterList of length 8
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 337371 337372 337373 337374 337375 337376 337377
[["CNVSNP"]] 1 2 3 4 5 6 7 ... 106733 106734 106735 106736 106737 106738 106739
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( SARC )
CharacterList of length 8
[["RNASeq2GeneNorm"]] TCGA-3B-A9HI-01A TCGA-3B-A9HJ-01A ... TCGA-Z4-AAPG-01A
[["miRNASeqGene"]] TCGA-3B-A9HI-01A TCGA-3B-A9HJ-01A ... TCGA-Z4-AAPG-01A
[["CNASNP"]] TCGA-3B-A9HI-01A-11D-A386-01 ... TCGA-Z4-AAPG-10A-01D-A38Y-01
[["CNVSNP"]] TCGA-3B-A9HI-01A-11D-A386-01 ... TCGA-Z4-AAPG-10A-01D-A38Y-01
[["Methylation"]] TCGA-3B-A9HI-01A-11D-A388-05 ... TCGA-Z4-AAPG-01A-11D-A39H-05
[["RPPAArray"]] TCGA-3B-A9HI-01A TCGA-3B-A9HJ-01A ... TCGA-Z4-AAPG-01A
[["gistica"]] TCGA-3B-A9HI-01A-11D-A386-01 ... TCGA-Z4-AAPG-01A-11D-A38Y-01
[["gistict"]] TCGA-3B-A9HI-01A-11D-A386-01 ... TCGA-Z4-AAPG-01A-11D-A38Y-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.2 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP 25.9 Mb
4          CNVSNP  8.3 Mb
5     Methylation 67.4 Mb
6       RPPAArray  0.1 Mb
7         gistica  4.3 Mb
8         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   165 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  96.00   96.00    1.83    1.51    2.46 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  261 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   53.00   61.00   60.88   71.00   90.00       1 

vital_status: 
  0   1 
165  96 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0   319.5   668.0   851.5  1167.0  2694.0     165 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      2     499     881    1197    1585    5723      76 


gender: 
female   male 
   142    119 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1994    2007    2010    2009    2012    2013       4 

radiation_therapy: 
  no  yes NA's 
 184   71    6 


residual_tumor: 
  r0   r1   r2   rx NA's 
 155   70    9   26    1 

race: 
                    asian black or african american                     white 
                        6                        18                       228 
                     NA's 
                        9 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     5                    223                     33 

}}
\keyword{datasets}

\name{ KIRC }
\alias{ KIRC }
\docType{data}
\title{ KIRC }

\details{
\preformatted{
> KIRC
A MultiAssayExperiment object of 11 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 11: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 537 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 606 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 326 columns 
 [4] CNASNP: RangedRaggedAssay with 488691 rows and 1059 columns 
 [5] CNVSNP: RangedRaggedAssay with 85036 rows and 1059 columns 
 [6] Methylation: SummarizedExperiment with 485577 rows and 480 columns 
 [7] mRNAArray: ExpressionSet with 17814 rows and 72 columns 
 [8] RPPAArray: ExpressionSet with 217 rows and 478 columns 
 [9] Mutations: RangedRaggedAssay with 26369 rows and 437 columns 
 [10] gistica: SummarizedExperiment with 24776 rows and 528 columns 
 [11] gistict: SummarizedExperiment with 24776 rows and 528 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( KIRC )
CharacterList of length 11
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 488685 488686 488687 488688 488689 488690 488691
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 85030 85031 85032 85033 85034 85035 85036
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] ADRA2B ALS2CR11 ANKRD55 ASB10 ... B4GALT5 FOXD1 HLA-J RAP1GAP
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
...
<1 more element>

> colnames( KIRC )
CharacterList of length 11
[["RNASeqGene"]] TCGA-A3-3306-01A TCGA-A3-3307-01A ... TCGA-DV-5576-01A
[["RNASeq2GeneNorm"]] TCGA-3Z-A93Z-01A TCGA-6D-AA2E-01A ... TCGA-T7-A92I-01A
[["miRNASeqGene"]] TCGA-3Z-A93Z-01A TCGA-6D-AA2E-01A ... TCGA-T7-A92I-01A
[["CNASNP"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ... TCGA-T7-A92I-10A-01D-A36Z-01
[["CNVSNP"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ... TCGA-T7-A92I-10A-01D-A36Z-01
[["Methylation"]] TCGA-3Z-A93Z-01A-11D-A36Y-05 ... TCGA-T7-A92I-01A-11D-A36Y-05
[["mRNAArray"]] TCGA-A3-3306-01A TCGA-A3-3307-01A ... TCGA-CJ-4644-01A
[["RPPAArray"]] TCGA-3Z-A93Z-01A TCGA-6D-AA2E-01A ... TCGA-MW-A4EC-01A
[["Mutations"]] TCGA-A3-3308-01A-01D-0966-08 ... TCGA-EU-5907-01A-11D-1669-08
[["gistica"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ... TCGA-T7-A92I-01A-11D-A36W-01
...
<1 more element>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.2 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 37.6 Mb
5           CNVSNP  6.8 Mb
6      Methylation 67.5 Mb
7        mRNAArray    1 Mb
8        RPPAArray  0.1 Mb
9        Mutations  7.9 Mb
10         gistica  4.4 Mb
11         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   360 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 177.00  177.00    2.24    1.77    2.61 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  537 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.00   52.00   61.00   60.57   70.00   90.00       1 

vital_status: 
  0   1 
360 177 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   333.0   819.0   961.2  1432.0  3615.0     360 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     662    1416    1494    2087    4537     152 

tumor_tissue_site: 
kidney 
   537 

pathologic_stage: 
  stage i  stage ii stage iii  stage iv 
      270        57       126        84 


pathology_N_stage: 
 n0  n1  nx 
240  17 280 

pathology_M_stage: 
  m0   m1   mx NA's 
 426   79   30    2 

gender: 
female   male 
   191    346 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1998    2004    2006    2006    2007    2013 

days_to_last_known_alive: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     191    1172    1117    1887    2799     510 

radiation_therapy: 
  no  yes NA's 
 142    2  393 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00   90.00   85.56  100.00  100.00     483 

histological_type: 
kidney clear cell renal carcinoma 
                              537 

number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   14.00   30.00   28.33   40.00   65.00     516 

year_of_tobacco_smoking_onset: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1946    1966    1978    1979    1996    2001     525 

race: 
                    asian black or african american                     white 
                        8                        56                       466 
                     NA's 
                        7 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    26                    359                    152 

}}
\keyword{datasets}

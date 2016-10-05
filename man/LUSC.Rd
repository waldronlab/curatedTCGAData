\name{ LUSC }
\alias{ LUSC }
\docType{data}
\title{ LUSC }

\details{
\preformatted{
> LUSC
A MultiAssayExperiment object of 12 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 12: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 240 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 552 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 387 columns 
 [4] CNASNP: RangedRaggedAssay with 543091 rows and 1035 columns 
 [5] CNVSNP: RangedRaggedAssay with 134864 rows and 1032 columns 
 [6] CNACGH: RangedRaggedAssay with 87417 rows and 407 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 412 columns 
 [8] mRNAArray: ExpressionSet with 17814 rows and 154 columns 
 [9] RPPAArray: ExpressionSet with 223 rows and 328 columns 
 [10] Mutations: RangedRaggedAssay with 65305 rows and 178 columns 
 [11] gistica: SummarizedExperiment with 24776 rows and 501 columns 
 [12] gistict: SummarizedExperiment with 24776 rows and 501 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( LUSC )
CharacterList of length 12
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 543085 543086 543087 543088 543089 543090 543091
[["CNVSNP"]] 1 2 3 4 5 6 7 ... 134858 134859 134860 134861 134862 134863 134864
[["CNACGH"]] 1 2 3 4 5 6 7 8 ... 87411 87412 87413 87414 87415 87416 87417
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] AGRN AGRN GLTPD1 ACTRT2 CASP9 ... CNGA2 USP9Y CD9 TUBGCP5
...
<2 more elements>

> colnames( LUSC )
CharacterList of length 12
[["RNASeqGene"]] TCGA-18-3406-01A TCGA-18-3407-01A ... TCGA-94-7033-01A
[["RNASeq2GeneNorm"]] TCGA-18-3406-01A TCGA-18-3407-01A ... TCGA-XC-AA0X-01A
[["miRNASeqGene"]] TCGA-18-5592-01A TCGA-18-5595-01A ... TCGA-XC-AA0X-01A
[["CNASNP"]] TCGA-18-3406-01A-01D-0978-01 ... TCGA-XC-AA0X-10A-01D-A400-01
[["CNVSNP"]] TCGA-18-3406-01A-01D-0978-01 ... TCGA-XC-AA0X-10A-01D-A400-01
[["CNACGH"]] TCGA-18-3406-01A-01D-1519-02 ... TCGA-85-6561-10A-01D-1815-02
[["Methylation"]] TCGA-18-3417-11A-01D-1440-05 ... TCGA-XC-AA0X-01A-32D-A408-05
[["mRNAArray"]] TCGA-18-3406-01A TCGA-18-3407-01A ... TCGA-66-2800-01A
[["RPPAArray"]] TCGA-18-4721-01A TCGA-18-5592-01A ... TCGA-O2-A5IB-01A
[["Mutations"]] TCGA-18-3406-01A-01D-0983-08 ... TCGA-85-6561-01A-11D-1817-08
...
<2 more elements>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.2 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 41.7 Mb
5           CNVSNP 10.6 Mb
6           CNACGH  6.8 Mb
7      Methylation 67.4 Mb
8        mRNAArray    1 Mb
9        RPPAArray  0.1 Mb
10       Mutations 77.1 Mb
11         gistica  4.4 Mb
12         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   288 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 216.00  216.00    1.51    1.25    1.90 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  504 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  39.00   62.00   68.00   67.26   73.00   90.00      10 

vital_status: 
  0   1 
284 220 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   282.0   551.0   871.9  1109.0  5287.0     288 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   272.8   663.5   945.0  1262.0  4765.0     112 

tumor_tissue_site: 
lung 
 504 



pathology_N_stage: 
 n0  n1  n2  n3  nx 
320 133  40   5   6 


gender: 
female   male 
   131    373 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2005    2009    2008    2011    2013      25 

days_to_last_known_alive: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   298.8   706.0   904.8  1052.0  3724.0     436 

radiation_therapy: 
  no  yes NA's 
 386   53   65 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    80.0    60.3    90.0   100.0     338 

histological_type: 
                      lung basaloid squamous cell carcinoma 
                                                         15 
                     lung papillary squamous cell caricnoma 
                                                          6 
                    lung small cell squamous cell carcinoma 
                                                          1 
lung squamous cell carcinoma- not otherwise specified (nos) 
                                                        482 

number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   31.12   50.00   52.91   64.50  240.00      77 

year_of_tobacco_smoking_onset: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1933    1952    1960    1960    1968    1997     183 

residual_tumor: 
  r0   r1   r2   rx NA's 
 401   12    4   23   64 

race: 
                    asian black or african american                     white 
                        9                        31                       351 
                     NA's 
                      113 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     8                    319                    177 

}}
\keyword{datasets}

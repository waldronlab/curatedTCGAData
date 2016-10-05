\name{ PRAD }
\alias{ PRAD }
\docType{data}
\title{ PRAD }

\details{
\preformatted{
> PRAD
A MultiAssayExperiment object of 10 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 10: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 550 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 547 columns 
 [3] CNASNP: RangedRaggedAssay with 573776 rows and 1029 columns 
 [4] CNVSNP: RangedRaggedAssay with 117345 rows and 1023 columns 
 [5] CNAseq: RangedRaggedAssay with 23398 rows and 230 columns 
 [6] Methylation: SummarizedExperiment with 485577 rows and 549 columns 
 [7] RPPAArray: ExpressionSet with 195 rows and 352 columns 
 [8] Mutations: RangedRaggedAssay with 12348 rows and 332 columns 
 [9] gistica: SummarizedExperiment with 24776 rows and 492 columns 
 [10] gistict: SummarizedExperiment with 24776 rows and 492 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( PRAD )
CharacterList of length 10
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 573770 573771 573772 573773 573774 573775 573776
[["CNVSNP"]] 1 2 3 4 5 6 7 ... 117339 117340 117341 117342 117343 117344 117345
[["CNAseq"]] 1 2 3 4 5 6 7 8 ... 23392 23393 23394 23395 23396 23397 23398
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] MYOM3 LPPR5 CD1C LBH C6 FRS3 ... DCAF4 RFWD3 FAM209A SUN2 WDR48
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( PRAD )
CharacterList of length 10
[["RNASeq2GeneNorm"]] TCGA-2A-A8VL-01A TCGA-2A-A8VO-01A ... TCGA-ZG-A9NI-01A
[["miRNASeqGene"]] TCGA-2A-A8VL-01A TCGA-2A-A8VO-01A ... TCGA-ZG-A9NI-01A
[["CNASNP"]] TCGA-2A-A8VL-01A-21D-A376-01 ... TCGA-ZG-A9NI-10A-01D-A41M-01
[["CNVSNP"]] TCGA-2A-A8VL-01A-21D-A376-01 ... TCGA-ZG-A9NI-10A-01D-A41M-01
[["CNAseq"]] TCGA-CH-5741-01A-11D-1572-02 ... TCGA-HI-7171-10A-01D-2111-02
[["Methylation"]] TCGA-2A-A8VL-01A-21D-A378-05 ... TCGA-ZG-A9NI-01A-11D-A41L-05
[["RPPAArray"]] TCGA-2A-A8VL-01A TCGA-2A-A8VO-01A ... TCGA-ZG-A9NI-01A
[["Mutations"]] TCGA-2A-A8VL-01A-21D-A377-08 ... TCGA-ZG-A8QZ-01A-11D-A377-08
[["gistica"]] TCGA-2A-A8VL-01A-21D-A376-01 ... TCGA-ZG-A9NI-01A-11D-A41J-01
[["gistict"]] TCGA-2A-A8VL-01A-21D-A376-01 ... TCGA-ZG-A9NI-01A-11D-A41J-01

Sizes of each ExperimentList element:

             assay size.Mb
1  RNASeq2GeneNorm  1.2 Mb
2     miRNASeqGene  0.2 Mb
3           CNASNP 44.1 Mb
4           CNVSNP  9.2 Mb
5           CNAseq  1.9 Mb
6      Methylation 67.5 Mb
7        RPPAArray  0.1 Mb
8        Mutations 20.2 Mb
9          gistica  4.4 Mb
10         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   488 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  10.00   10.00    3.02    1.99      NA 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  498 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  41.00   56.00   61.00   61.02   66.00   78.00      11 

vital_status: 
  0   1 
488  10 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  146.0   743.8  1102.0  1579.0  2316.0  3502.0     488 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   522.5   926.0  1080.0  1461.0  5024.0       8 

tumor_tissue_site: 
prostate 
     498 


pathology_N_stage: 
  n0   n1 NA's 
 346   79   73 

gender: 
male 
 498 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2000    2009    2011    2010    2012    2013      31 

radiation_therapy: 
  no  yes NA's 
 395   59   44 

histological_type: 
prostate adenocarcinoma  other subtype    prostate adenocarcinoma acinar type 
                                    15                                    483 

residual_tumor: 
  r0   r1   r2   rx NA's 
 316  147    5   15   15 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4447  0.0000 15.0000      91 

gleason_score: 
  6   7   8   9  10 
 45 248  64 137   4 

psa_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.030   0.100   1.742   0.110 323.000      57 

days_to_psa: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -164.0   191.0   512.0   685.6   926.0  3447.0      53 

race: 
                    asian black or african american                     white 
                        2                         7                       147 
                     NA's 
                      342 

ethnicity: 
not hispanic or latino                   NA's 
                   152                    346 

}}
\keyword{datasets}

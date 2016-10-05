\name{ UCEC }
\alias{ UCEC }
\docType{data}
\title{ UCEC }

\details{
\preformatted{
> UCEC
A MultiAssayExperiment object of 11 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 11: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 269 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 380 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 433 columns 
 [4] CNASNP: RangedRaggedAssay with 614636 rows and 1083 columns 
 [5] CNVSNP: RangedRaggedAssay with 126399 rows and 1078 columns 
 [6] CNAseq: RangedRaggedAssay with 36400 rows and 213 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 466 columns 
 [8] mRNAArray: ExpressionSet with 17814 rows and 54 columns 
 [9] RPPAArray: ExpressionSet with 208 rows and 440 columns 
 [10] gistica: SummarizedExperiment with 24776 rows and 539 columns 
 [11] gistict: SummarizedExperiment with 24776 rows and 539 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( UCEC )
CharacterList of length 11
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 619406 619407 619408 619409 619410 619411 619412
[["CNVSNP"]] 1 2 3 4 5 6 7 ... 127088 127089 127090 127091 127092 127093 127094
[["CNAseq"]] 1 2 3 4 5 6 7 8 ... 36394 36395 36396 36397 36398 36399 36400
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
...
<1 more element>

> colnames( UCEC )
CharacterList of length 11
[["RNASeqGene"]] TCGA-A5-A0G1-01A TCGA-A5-A0G2-01A ... TCGA-EY-A212-01A
[["RNASeq2GeneNorm"]] TCGA-A5-A0G1-01A TCGA-A5-A0G2-01A ... TCGA-FI-A2F9-01A
[["miRNASeqGene"]] TCGA-2E-A9G8-01A TCGA-4E-A92E-01A ... TCGA-SL-A6JA-01A
[["CNASNP"]] TCGA-2E-A9G8-01A-11D-A402-01 ... TCGA-SL-A6JA-10A-01D-A31T-01
[["CNVSNP"]] TCGA-2E-A9G8-01A-11D-A402-01 ... TCGA-SL-A6JA-10A-01D-A31T-01
[["CNAseq"]] TCGA-A5-A0G5-01A-11D-A043-02 ... TCGA-FI-A2D4-10A-01D-A17E-02
[["Methylation"]] TCGA-2E-A9G8-01A-11D-A409-05 ... TCGA-SL-A6JA-01A-11D-A31V-05
[["mRNAArray"]] TCGA-A5-A0G2-01A TCGA-A5-A0G3-01A ... TCGA-BK-A0CC-01A
[["RPPAArray"]] TCGA-4E-A92E-01A TCGA-5S-A9Q8-01A ... TCGA-SL-A6JA-01A
[["gistica"]] TCGA-2E-A9G8-01A-11D-A402-01 ... TCGA-SL-A6JA-01A-11D-A31T-01
...
<1 more element>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.2 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 47.2 Mb
5           CNVSNP   10 Mb
6           CNAseq  2.9 Mb
7      Methylation 67.5 Mb
8        mRNAArray    1 Mb
9        RPPAArray  0.1 Mb
10         gistica  4.4 Mb
11         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   458 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  90.00   90.00    1.95    1.62    2.51 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  548 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   57.00   64.00   63.94   71.00   90.00       4 

vital_status: 
  0   1 
458  90 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   364.0   711.5   885.6  1072.0  3423.0     458 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -6.0   491.5   902.5  1129.0  1600.0  6859.0      46 

tumor_tissue_site: 
   endometrial other  specify 
           547              1 

gender: 
female 
   548 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1995    2007    2009    2009    2010    2013       9 

days_to_last_known_alive: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    145     404     729    1405    2096    4144     535 

radiation_therapy: 
  no  yes NA's 
 295  228   25 

histological_type: 
endometrioid endometrial adenocarcinoma           mixed serous and endometrioid 
                                    411                                      22 
      serous endometrial adenocarcinoma 
                                    115 

residual_tumor: 
  r0   r1   r2   rx NA's 
 376   22   16   41   93 


ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    15                    377                    156 

}}
\keyword{datasets}

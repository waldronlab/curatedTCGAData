\name{ COAD }
\alias{ COAD }
\docType{data}
\title{ COAD }

\details{
\preformatted{
> COAD
A MultiAssayExperiment object of 12 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 12: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 10 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 191 columns 
 [3] miRNASeqGene: ExpressionSet with 705 rows and 221 columns 
 [4] CNASNP: RangedRaggedAssay with 457535 rows and 914 columns 
 [5] CNVSNP: RangedRaggedAssay with 90062 rows and 914 columns 
 [6] CNAseq: RangedRaggedAssay with 40530 rows and 136 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 333 columns 
 [8] mRNAArray: ExpressionSet with 17814 rows and 172 columns 
 [9] RPPAArray: ExpressionSet with 208 rows and 360 columns 
 [10] Mutations: RangedRaggedAssay with 62530 rows and 154 columns 
 [11] gistica: SummarizedExperiment with 24776 rows and 448 columns 
 [12] gistict: SummarizedExperiment with 24776 rows and 448 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( COAD )
CharacterList of length 12
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 459484 459485 459486 459487 459488 459489 459490
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 90528 90529 90530 90531 90532 90533 90534
[["CNAseq"]] 1 2 3 4 5 6 7 8 ... 40524 40525 40526 40527 40528 40529 40530
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] SIRPB1 ATF6 MUC17 ORC5L SRPK2 ... FRMPD1 KIAA2026 ODZ1 GSPT2
...
<2 more elements>

> colnames( COAD )
CharacterList of length 12
[["RNASeqGene"]] TCGA-AA-3841-01A TCGA-AA-3866-01A ... TCGA-AA-3980-01A
[["RNASeq2GeneNorm"]] TCGA-A6-2671-01A TCGA-A6-2672-01A ... TCGA-CM-5341-01A
[["miRNASeqGene"]] TCGA-A6-2675-01A TCGA-A6-4105-01A ... TCGA-G4-6628-01A
[["CNASNP"]] TCGA-3L-AA1B-01A-11D-A36W-01 ... TCGA-WS-AB45-10A-01D-A40O-01
[["CNVSNP"]] TCGA-3L-AA1B-01A-11D-A36W-01 ... TCGA-WS-AB45-10A-01D-A40O-01
[["CNAseq"]] TCGA-A6-2671-01A-01D-1405-02 ... TCGA-CM-4752-10A-01D-1405-02
[["Methylation"]] TCGA-3L-AA1B-01A-11D-A36Y-05 ... TCGA-WS-AB45-01A-11D-A40X-05
[["mRNAArray"]] TCGA-A6-2671-11A TCGA-A6-2672-01A ... TCGA-AY-4071-01A
[["RPPAArray"]] TCGA-3L-AA1B-01A TCGA-4N-A93T-01A ... TCGA-T9-A92H-01A
[["Mutations"]] TCGA-A6-2672-01A-01W-0833-10 ... TCGA-AY-4071-01A-01W-1073-09
...
<2 more elements>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.1 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 35.2 Mb
5           CNVSNP  7.1 Mb
6           CNAseq  3.1 Mb
7      Methylation 67.4 Mb
8        mRNAArray    1 Mb
9        RPPAArray  0.1 Mb
10       Mutations 22.5 Mb
11         gistica  4.4 Mb
12         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   355 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
102.000 102.000   1.107   0.915   1.504 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  457 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   58.00   68.00   66.95   77.00   90.00       2 

vital_status: 
  0   1 
355 102 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   176.8   404.0   683.0  1064.0  3042.0     355 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -9.0   379.5   669.0   855.6  1088.0  4502.0      46 

tumor_tissue_site: 
colon  NA's 
  456     1 





gender: 
female   male 
   216    241 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1998    2007    2009    2008    2010    2013 

days_to_last_known_alive: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   624.2  2170.0  2155.0  3746.0  3924.0     447 

radiation_therapy: 
  no  yes NA's 
 378    9   70 

histological_type: 
         colon adenocarcinoma colon mucinous adenocarcinoma 
                          390                            62 
                         NA's 
                            5 

residual_tumor: 
  r0   r1   r2   rx NA's 
 329    4   26   25   73 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.058   2.000  50.000      25 

race: 
american indian or alaska native                            asian 
                               1                               11 
       black or african american                            white 
                              59                              213 
                            NA's 
                             173 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     4                    270                    183 

}}
\keyword{datasets}

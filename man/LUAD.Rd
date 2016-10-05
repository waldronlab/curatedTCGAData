\name{ LUAD }
\alias{ LUAD }
\docType{data}
\title{ LUAD }

\details{
\preformatted{
> LUAD
A MultiAssayExperiment object of 12 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 12: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 162 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 576 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 498 columns 
 [4] CNASNP: RangedRaggedAssay with 501475 rows and 1095 columns 
 [5] CNVSNP: RangedRaggedAssay with 115836 rows and 1095 columns 
 [6] CNAseq: RangedRaggedAssay with 399592 rows and 249 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 492 columns 
 [8] mRNAArray: ExpressionSet with 17814 rows and 32 columns 
 [9] RPPAArray: ExpressionSet with 223 rows and 365 columns 
 [10] Mutations: RangedRaggedAssay with 72541 rows and 230 columns 
 [11] gistica: SummarizedExperiment with 24776 rows and 516 columns 
 [12] gistict: SummarizedExperiment with 24776 rows and 516 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( LUAD )
CharacterList of length 12
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 501469 501470 501471 501472 501473 501474 501475
[["CNVSNP"]] 1 2 3 4 5 6 7 ... 115830 115831 115832 115833 115834 115835 115836
[["CNAseq"]] 1 2 3 4 5 6 7 ... 399586 399587 399588 399589 399590 399591 399592
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] PLEKHN1 UBE2J2 Unknown C1orf200 ... C9orf131 FUBP3 FAM123B
...
<2 more elements>

> colnames( LUAD )
CharacterList of length 12
[["RNASeqGene"]] TCGA-05-4244-01A TCGA-05-4249-01A ... TCGA-95-7043-01A
[["RNASeq2GeneNorm"]] TCGA-05-4244-01A TCGA-05-4249-01A ... TCGA-S2-AA1A-01A
[["miRNASeqGene"]] TCGA-05-4384-01A TCGA-05-4390-01A ... TCGA-S2-AA1A-01A
[["CNASNP"]] TCGA-05-4244-01A-01D-1877-01 ... TCGA-S2-AA1A-01A-12D-A396-01
[["CNVSNP"]] TCGA-05-4244-01A-01D-1877-01 ... TCGA-S2-AA1A-01A-12D-A396-01
[["CNAseq"]] TCGA-05-4249-01A-01D-1103-02 ... TCGA-91-6836-11A-01D-1853-02
[["Methylation"]] TCGA-05-4384-01A-01D-1756-05 ... TCGA-S2-AA1A-01A-12D-A398-05
[["mRNAArray"]] TCGA-05-4244-01A TCGA-05-4249-01A ... TCGA-67-3774-01A
[["RPPAArray"]] TCGA-05-4244-01A TCGA-05-4249-01A ... TCGA-S2-AA1A-01A
[["Mutations"]] TCGA-05-4249-01A-01D-1105-08 ... TCGA-99-7458-01A-11D-2036-08
...
<2 more elements>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.2 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.2 Mb
4           CNASNP 38.6 Mb
5           CNVSNP  9.1 Mb
6           CNAseq 30.6 Mb
7      Methylation 67.5 Mb
8        mRNAArray    1 Mb
9        RPPAArray  0.1 Mb
10       Mutations 88.5 Mb
11         gistica  4.4 Mb
12         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   336 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 184.00  184.00    1.70    1.37    2.00 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  520 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  33.00   59.00   66.00   65.22   72.00   88.00      31 

vital_status: 
  0   1 
332 188 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   297.8   619.0   791.4  1120.0  4961.0     336 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   365.2   620.5   868.7  1068.0  7248.0     126 

tumor_tissue_site: 
lung 
 520 





gender: 
female   male 
   279    241 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1991    2007    2010    2008    2011    2013      19 

days_to_last_known_alive: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    96.5   141.0   327.6   386.0  1178.0     513 

radiation_therapy: 
  no  yes NA's 
 413   61   46 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   80.00   90.00   78.55  100.00  100.00     382 


number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   20.00   40.00   41.55   50.00  154.00     163 

year_of_tobacco_smoking_onset: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1930    1956    1965    1965    1972    1999     241 

residual_tumor: 
  r0   r1   r2   rx NA's 
 347   13    4   26  130 

race: 
american indian or alaska native                            asian 
                               1                                8 
       black or african american                            white 
                              53                              392 
                            NA's 
                              66 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     7                    388                    125 

}}
\keyword{datasets}

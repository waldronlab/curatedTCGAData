\name{ LGG }
\alias{ LGG }
\docType{data}
\title{ LGG }

\details{
\preformatted{
> LGG
A MultiAssayExperiment object of 11 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 11: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 529 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 525 columns 
 [3] CNASNP: RangedRaggedAssay with 411056 rows and 1013 columns 
 [4] CNVSNP: RangedRaggedAssay with 79618 rows and 1013 columns 
 [5] CNAseq: RangedRaggedAssay with 6360 rows and 104 columns 
 [6] Methylation: SummarizedExperiment with 485577 rows and 529 columns 
 [7] mRNAArray: ExpressionSet with 17814 rows and 27 columns 
 [8] RPPAArray: ExpressionSet with 201 rows and 434 columns 
 [9] Mutations: RangedRaggedAssay with 9885 rows and 286 columns 
 [10] gistica: SummarizedExperiment with 24776 rows and 512 columns 
 [11] gistict: SummarizedExperiment with 24776 rows and 512 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( LGG )
CharacterList of length 11
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 411912 411913 411914 411915 411916 411917 411918
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 79785 79786 79787 79788 79789 79790 79791
[["CNAseq"]] 1 2 3 4 5 6 7 8 9 ... 6352 6353 6354 6355 6356 6357 6358 6359 6360
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] GPR114 TP53 C3 KLK10 ANXA9 TPO ... KAT6B RBBP6 ZACN CIC CIC
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
...
<1 more element>

> colnames( LGG )
CharacterList of length 11
[["RNASeq2GeneNorm"]] TCGA-CS-4938-01B TCGA-CS-4941-01A ... TCGA-WY-A85E-01A
[["miRNASeqGene"]] TCGA-CS-4938-01B TCGA-CS-4941-01A ... TCGA-WY-A85E-01A
[["CNASNP"]] TCGA-CS-4938-01B-11D-1892-01 ... TCGA-WY-A85E-10A-01D-A366-01
[["CNVSNP"]] TCGA-CS-4938-01B-11D-1892-01 ... TCGA-WY-A85E-10A-01D-A366-01
[["CNAseq"]] TCGA-CS-4938-01B-11D-1891-02 ... TCGA-HW-7495-10A-01D-2022-02
[["Methylation"]] TCGA-CS-4938-01B-11D-1894-05 ... TCGA-WY-A85E-01A-11D-A368-05
[["mRNAArray"]] TCGA-CS-4942-01A TCGA-CS-4943-01A ... TCGA-E1-5322-01A
[["RPPAArray"]] TCGA-CS-4938-01B TCGA-CS-4942-01A ... TCGA-WY-A85D-01A
[["Mutations"]] TCGA-CS-4938-01B-11D-1893-08 ... TCGA-QH-A65Z-01A-11D-A29Q-08
[["gistica"]] TCGA-CS-4938-01B-11D-1892-01 ... TCGA-WY-A85E-01A-11D-A40C-01
...
<1 more element>

Sizes of each ExperimentList element:

             assay size.Mb
1  RNASeq2GeneNorm  1.2 Mb
2     miRNASeqGene  0.2 Mb
3           CNASNP 31.7 Mb
4           CNVSNP  6.4 Mb
5           CNAseq  0.5 Mb
6      Methylation 67.5 Mb
7        mRNAArray    1 Mb
8        RPPAArray  0.1 Mb
9        Mutations    4 Mb
10         gistica  4.4 Mb
11         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   390 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 125.00  125.00    2.23    1.87    2.83 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  515 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   32.00   41.00   42.93   53.00   86.00       1 

vital_status: 
  0   1 
389 126 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      7     438     814    1219    1547    5166     390 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -10.0   371.8   625.0   891.9  1151.0  6423.0      91 

tumor_tissue_site: 
central nervous system 
                   515 

gender: 
female   male 
   230    285 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1992    2008    2011    2009    2012    2013 

radiation_therapy: 
  no  yes NA's 
 186  296   33 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   80.00   90.00   86.64  100.00  100.00     208 

histological_type: 
      astrocytoma  oligoastrocytoma oligodendroglioma 
              194               130               191 

race: 
american indian or alaska native                            asian 
                               1                                8 
       black or african american                            white 
                              21                              475 
                            NA's 
                              10 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    32                    449                     34 

}}
\keyword{datasets}

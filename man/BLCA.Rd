\name{ BLCA }
\alias{ BLCA }
\docType{data}
\title{ BLCA }

\details{
\preformatted{
> BLCA
A MultiAssayExperiment object of 11 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 11: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 67 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 427 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 429 columns 
 [4] CNASNP: RangedRaggedAssay with 479978 rows and 806 columns 
 [5] CNVSNP: RangedRaggedAssay with 104349 rows and 797 columns 
 [6] CNAseq: RangedRaggedAssay with 35692 rows and 232 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 434 columns 
 [8] RPPAArray: ExpressionSet with 208 rows and 344 columns 
 [9] Mutations: RangedRaggedAssay with 39312 rows and 130 columns 
 [10] gistica: SummarizedExperiment with 24776 rows and 408 columns 
 [11] gistict: SummarizedExperiment with 24776 rows and 408 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( BLCA )
CharacterList of length 11
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 479972 479973 479974 479975 479976 479977 479978
[["CNVSNP"]] 1 2 3 4 5 6 7 ... 104343 104344 104345 104346 104347 104348 104349
[["CNAseq"]] 1 2 3 4 5 6 7 8 ... 35686 35687 35688 35689 35690 35691 35692
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] KIAA0090 COL9A2 SLC6A9 HSPB11 ... TBX22 RBM17 NTF3 RANGAP1
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
...
<1 more element>

> colnames( BLCA )
CharacterList of length 11
[["RNASeqGene"]] TCGA-BL-A0C8-01A TCGA-BL-A13I-01A ... TCGA-HQ-A2OE-01A
[["RNASeq2GeneNorm"]] TCGA-2F-A9KO-01A TCGA-2F-A9KP-01A ... TCGA-ZF-AA5P-01A
[["miRNASeqGene"]] TCGA-2F-A9KO-01A TCGA-2F-A9KP-01A ... TCGA-ZF-AA5P-01A
[["CNASNP"]] TCGA-2F-A9KO-01A-11D-A38F-01 ... TCGA-ZF-AA5P-10A-01D-A393-01
[["CNVSNP"]] TCGA-2F-A9KO-01A-11D-A38F-01 ... TCGA-ZF-AA5P-10A-01D-A393-01
[["CNAseq"]] TCGA-BL-A0C8-01A-11D-A10R-02 ... TCGA-K4-A3WV-10A-01D-A23Q-26
[["Methylation"]] TCGA-2F-A9KO-01A-11D-A38H-05 ... TCGA-ZF-AA5P-01A-11D-A392-05
[["RPPAArray"]] TCGA-2F-A9KO-01A TCGA-2F-A9KQ-01A ... TCGA-ZF-AA5P-01A
[["Mutations"]] TCGA-BL-A0C8-01A-11D-A10S-08 ... TCGA-K4-A3WV-01A-11D-A22Z-08
[["gistica"]] TCGA-2F-A9KO-01A-11D-A38F-01 ... TCGA-ZF-AA5P-01A-11D-A390-01
...
<1 more element>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.1 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 36.9 Mb
5           CNVSNP  8.2 Mb
6           CNAseq  2.8 Mb
7      Methylation 67.5 Mb
8        RPPAArray  0.1 Mb
9        Mutations 53.3 Mb
10         gistica  4.4 Mb
11         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   232 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
180.000 180.000   1.125   0.992   1.332 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  412 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  34.00   60.00   69.00   68.08   76.00   90.00       1 

vital_status: 
  0   1 
230 182 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   19.0   235.8   410.5   552.8   654.5  3183.0     232 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -64.0   288.5   525.0   854.4  1092.0  5050.0     109 

tumor_tissue_site: 
bladder 
    412 

pathologic_stage: 
  stage i  stage ii stage iii  stage iv      NA's 
        2       131       141       136         2 



pathology_M_stage: 
  m0   m1   mx NA's 
 196   11  203    2 

gender: 
female   male 
   108    304 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1999    2009    2011    2010    2012    2013      18 

radiation_therapy: 
  no  yes NA's 
 366   20   26 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   80.00   90.00   83.09   90.00  100.00     276 

histological_type: 
muscle invasive urothelial carcinoma (pt2 or above) 
                                                409 
                                               NA's 
                                                  3 

number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.15   20.00   30.00   39.04   50.00  730.00     188 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.088   2.000  97.000     115 

race: 
                    asian black or african american                     white 
                       44                        23                       327 
                     NA's 
                       18 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     9                    371                     32 

}}
\keyword{datasets}

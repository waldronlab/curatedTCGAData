\name{ STAD }
\alias{ STAD }
\docType{data}
\title{ STAD }

\details{
\preformatted{
> STAD
A MultiAssayExperiment object of 11 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 11: 
 [1] RNASeqGene: ExpressionSet with 26120 rows and 36 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 41 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 430 columns 
 [4] CNASNP: RangedRaggedAssay with 443042 rows and 906 columns 
 [5] CNVSNP: RangedRaggedAssay with 118389 rows and 904 columns 
 [6] CNAseq: RangedRaggedAssay with 31824 rows and 214 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 397 columns 
 [8] RPPAArray: ExpressionSet with 195 rows and 357 columns 
 [9] Mutations: RangedRaggedAssay with 148520 rows and 289 columns 
 [10] gistica: SummarizedExperiment with 24776 rows and 441 columns 
 [11] gistict: SummarizedExperiment with 24776 rows and 441 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( STAD )
CharacterList of length 11
[["RNASeqGene"]] AADACL3 AADACL4 AB007962 AB074166 AB1 ... VCY1B VCY XKRY ZFY
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 443036 443037 443038 443039 443040 443041 443042
[["CNVSNP"]] 1 2 3 4 5 6 7 ... 118383 118384 118385 118386 118387 118388 118389
[["CNAseq"]] 1 2 3 4 5 6 7 8 ... 31818 31819 31820 31821 31822 31823 31824
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] KLHL17 UBE2J2 TAS1R3 ATAD3C ... TMEM161B ADCY3 FOSL2 TMEM161B
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
...
<1 more element>

> colnames( STAD )
CharacterList of length 11
[["RNASeqGene"]] TCGA-BR-4191-01A TCGA-BR-4201-01A ... TCGA-CG-4477-01A
[["RNASeq2GeneNorm"]] TCGA-B7-5816-01A TCGA-BR-4256-01A ... TCGA-VQ-A94O-01A
[["miRNASeqGene"]] TCGA-3M-AB46-01A TCGA-3M-AB47-01A ... TCGA-ZQ-A9CR-01A
[["CNASNP"]] TCGA-3M-AB46-01A-11D-A40Z-01 ... TCGA-ZQ-A9CR-10A-01D-A399-01
[["CNVSNP"]] TCGA-3M-AB46-01A-11D-A40Z-01 ... TCGA-ZQ-A9CR-10A-01D-A399-01
[["CNAseq"]] TCGA-B7-5816-01A-21D-1598-02 ... TCGA-F1-6874-10A-01D-1880-02
[["Methylation"]] TCGA-3M-AB46-01A-11D-A411-05 ... TCGA-ZQ-A9CR-01A-11D-A398-05
[["RPPAArray"]] TCGA-3M-AB47-01A TCGA-B7-5816-01A ... TCGA-ZA-A8F6-01A
[["Mutations"]] TCGA-B7-5816-01A-21D-1600-08 ... TCGA-IP-7968-01A-11D-2201-08
[["gistica"]] TCGA-3M-AB46-01A-11D-A40Z-01 ... TCGA-ZQ-A9CR-01A-11D-A396-01
...
<1 more element>

Sizes of each ExperimentList element:

             assay  size.Mb
1       RNASeqGene   1.5 Mb
2  RNASeq2GeneNorm   1.1 Mb
3     miRNASeqGene   0.1 Mb
4           CNASNP  34.1 Mb
5           CNVSNP   9.3 Mb
6           CNAseq   2.5 Mb
7      Methylation  67.4 Mb
8        RPPAArray   0.1 Mb
9        Mutations 154.3 Mb
10         gistica   4.4 Mb
11         gistict   4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   272 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
171.000 169.000   0.953   0.792   1.090 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  443 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   58.00   67.00   65.73   73.00   90.00       9 

vital_status: 
  0   1 
270 173 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   196.0   348.0   423.7   553.0  2197.0     272 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    89.0   409.5   546.2   761.8  3720.0      87 

tumor_tissue_site: 
stomach 
    443 




pathology_M_stage: 
 m0  m1  mx 
391  30  22 

gender: 
female   male 
   158    285 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1996    2010    2011    2010    2012    2013       6 

radiation_therapy: 
  no  yes NA's 
 323   77   43 


residual_tumor: 
  r0   r1   r2   rx NA's 
 350   18   19   25   31 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   5.635   8.000  57.000      51 

race: 
                                    asian 
                                       89 
                black or african american 
                                       13 
native hawaiian or other pacific islander 
                                        1 
                                    white 
                                      278 
                                     NA's 
                                       62 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     5                    318                    120 

}}
\keyword{datasets}

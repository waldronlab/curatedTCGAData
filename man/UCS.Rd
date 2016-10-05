\name{ UCS }
\alias{ UCS }
\docType{data}
\title{ UCS }

\details{
\preformatted{
> UCS
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 57 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 56 columns 
 [3] CNASNP: RangedRaggedAssay with 54944 rows and 111 columns 
 [4] CNVSNP: RangedRaggedAssay with 19298 rows and 111 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 57 columns 
 [6] RPPAArray: ExpressionSet with 192 rows and 48 columns 
 [7] Mutations: RangedRaggedAssay with 11339 rows and 57 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 56 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 56 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( UCS )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 8 ... 54938 54939 54940 54941 54942 54943 54944
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 19292 19293 19294 19295 19296 19297 19298
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] IPO11 CALR AJAP1 RP1L1 LAMA5 ... ANKRD11 RP11-53I6.2 ATF5 BRSK1
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( UCS )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-N5-A4R8-01A TCGA-N5-A4RA-01A ... TCGA-QN-A5NN-01A
[["miRNASeqGene"]] TCGA-N5-A4R8-01A TCGA-N5-A4RA-01A ... TCGA-QN-A5NN-01A
[["CNASNP"]] TCGA-N5-A4R8-10A-01D-A28T-01 ... TCGA-QN-A5NN-10A-01D-A28T-01
[["CNVSNP"]] TCGA-N5-A4R8-10A-01D-A28T-01 ... TCGA-QN-A5NN-10A-01D-A28T-01
[["Methylation"]] TCGA-N5-A4R8-01A-11D-A28S-05 ... TCGA-QN-A5NN-01A-11D-A28S-05
[["RPPAArray"]] TCGA-N5-A4R8-01A TCGA-N5-A4RA-01A ... TCGA-QN-A5NN-01A
[["Mutations"]] TCGA-N5-A4R8-01A-11D-A28R-08 ... TCGA-QN-A5NN-01A-11D-A28R-08
[["gistica"]] TCGA-N5-A4RA-01A-11D-A28Q-01 ... TCGA-QN-A5NN-01A-11D-A28Q-01
[["gistict"]] TCGA-N5-A4RA-01A-11D-A28Q-01 ... TCGA-QN-A5NN-01A-11D-A28Q-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.1 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP  4.2 Mb
4          CNVSNP  1.5 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7       Mutations 18.6 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   22 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  35.00   35.00    1.43    1.04    1.96 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
   57 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  51.00   62.00   68.00   69.72   76.00   90.00 

vital_status: 
 0  1 
22 35 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   304.0   522.0   705.4   790.5  3115.0      22 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   575.8   828.0  1160.0  1648.0  4269.0      31 

tumor_tissue_site: 
uterus 
    57 

gender: 
female 
    57 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2002    2007    2009    2009    2011    2012 

radiation_therapy: 
  no  yes NA's 
  29   25    3 

histological_type: 
uterine carcinosarcoma/ malignant mixed mullerian tumor (mmmt): nos 
                                                                 24 
                    uterine carcinosarcoma/ mmmt: heterologous type 
                                                                 20 
                      uterine carcinosarcoma/ mmmt: homologous type 
                                                                 13 

race: 
                    asian black or african american                     white 
                        3                         9                        44 
                     NA's 
                        1 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     1                     43                     13 

}}
\keyword{datasets}

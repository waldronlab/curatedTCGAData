\name{ TGCT }
\alias{ TGCT }
\docType{data}
\title{ TGCT }

\details{
\preformatted{
> TGCT
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 139 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 139 columns 
 [3] CNASNP: RangedRaggedAssay with 123447 rows and 271 columns 
 [4] CNVSNP: RangedRaggedAssay with 22698 rows and 271 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 139 columns 
 [6] RPPAArray: ExpressionSet with 192 rows and 107 columns 
 [7] Mutations: RangedRaggedAssay with 13287 rows and 138 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 134 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 134 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( TGCT )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 137962 137963 137964 137965 137966 137967 137968
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 25473 25474 25475 25476 25477 25478 25479
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] PIK3CD PTPRF . ARHGAP30 TTC13 ... ND4 CYTB EIF2S3 BCAP31 BAHCC1
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( TGCT )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-2G-AAEW-01A TCGA-2G-AAEX-01A ... TCGA-ZM-AA0N-01A
[["miRNASeqGene"]] TCGA-2G-AAEW-01A TCGA-2G-AAEX-01A ... TCGA-ZM-AA0N-01A
[["CNASNP"]] TCGA-2G-AAEW-01A-11D-A42X-01 ... TCGA-ZM-AA0N-10A-01D-A437-01
[["CNVSNP"]] TCGA-2G-AAEW-01A-11D-A42X-01 ... TCGA-ZM-AA0N-10A-01D-A437-01
[["Methylation"]] TCGA-2G-AAEW-01A-11D-A42Z-05 ... TCGA-ZM-AA0N-01A-21D-A436-05
[["RPPAArray"]] TCGA-2G-AAEW-01A TCGA-2G-AAEX-01A ... TCGA-ZM-AA0E-01A
[["Mutations"]] TCGA-2G-AAEW-01A-11D-A42Y-10 ... TCGA-ZM-AA0N-01A-21D-A435-10
[["gistica"]] TCGA-2G-AAEW-01A-11D-A42X-01 ... TCGA-ZM-AA0N-01A-21D-A434-01
[["gistict"]] TCGA-2G-AAEW-01A-11D-A42X-01 ... TCGA-ZM-AA0N-01A-21D-A434-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.2 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP  9.5 Mb
4          CNVSNP  1.8 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7       Mutations  8.6 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   130 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 4.0000  4.0000  1.5493  0.0466      NA 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  134 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.00   26.00   31.00   31.99   37.00   67.00 

vital_status: 
  0   1 
130   4 

days_to_death: 
  17  513  618 6972 NA's 
   1    1    1    1  130 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   691.5  1268.0  2097.0  2796.0  7437.0       3 

tumor_tissue_site: 
testes 
   134 


pathology_T_stage: 
t1 t2 t3 tx 
76 51  6  1 

pathology_N_stage: 
  n0   n1   n2   nx NA's 
  46   11    2   65   10 

pathology_M_stage: 
  m0   m1  m1a  m1b NA's 
 115    2    1    1   15 

gender: 
male 
 134 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1992    2006    2009    2008    2012    2013 

radiation_therapy: 
  no  yes NA's 
 111   21    2 

karnofsky_performance_score: 
  80   90  100 NA's 
   5   41   56   32 

race: 
                    asian black or african american                     white 
                        4                         6                       119 
                     NA's 
                        5 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    12                    111                     11 

}}
\keyword{datasets}

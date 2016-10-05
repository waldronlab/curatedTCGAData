\name{ GBM }
\alias{ GBM }
\docType{data}
\title{ GBM }

\details{
\preformatted{
> GBM
A MultiAssayExperiment object of 8 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 8: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 165 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 0 columns 
 [3] Methylation: SummarizedExperiment with 27578 rows and 283 columns 
 [4] mRNAArray: ExpressionSet with 12042 rows and 525 columns 
 [5] miRNAArray: ExpressionSet with 534 rows and 563 columns 
 [6] RPPAArray: ExpressionSet with 208 rows and 243 columns 
 [7] gistica: SummarizedExperiment with 24776 rows and 573 columns 
 [8] gistict: SummarizedExperiment with 24776 rows and 573 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( GBM )
CharacterList of length 8
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["Methylation"]] cg00000292 cg00002426 cg00003994 ... cg27662877 cg27665659
[["mRNAArray"]] AACS FSTL1 ELMO2 CREB3L1 RPS11 ... SNRPD2 SLC39A6 CTSC AQP7
[["miRNAArray"]] ebv-miR-BART1-3p ebv-miR-BART1-5p ... kshv-miR-K12-9*
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( GBM )
CharacterList of length 8
[["RNASeq2GeneNorm"]] TCGA-02-0047-01A TCGA-02-0055-01A ... TCGA-76-4932-01A
[["miRNASeqGene"]] character(0)
[["Methylation"]] TCGA-02-0001-01C-01D-0186-05 ... TCGA-41-4097-01A-01D-1228-05
[["mRNAArray"]] TCGA-02-0001-01C TCGA-02-0003-01A ... TCGA-87-5896-01A
[["miRNAArray"]] TCGA-02-0001-01C TCGA-02-0003-01A ... TCGA-87-5896-01A
[["RPPAArray"]] TCGA-02-0003-01A TCGA-02-0004-01A ... TCGA-RR-A6KC-01A
[["gistica"]] TCGA-02-0001-01C-01D-0182-01 ... TCGA-RR-A6KC-01A-31D-A33S-01
[["gistict"]] TCGA-02-0001-01C-01D-0182-01 ... TCGA-RR-A6KC-01A-31D-A33S-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.2 Mb
2    miRNASeqGene  0.1 Mb
3     Methylation  4.4 Mb
4       mRNAArray  0.7 Mb
5      miRNAArray  0.1 Mb
6       RPPAArray  0.1 Mb
7         gistica  4.4 Mb
8         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   104 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
491.000 491.000   1.047   0.978   1.156 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  595 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  10.00   50.00   59.00   57.82   68.00   89.00 

vital_status: 
  0   1 
104 491 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   170.5   382.0   503.8   607.5  3881.0     104 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   142.2   314.5   440.3   541.0  3881.0      33 

tumor_tissue_site: 
brain 
  595 

gender: 
female   male 
   230    365 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1989    2002    2006    2005    2009    2013 

radiation_therapy: 
  no  yes NA's 
  78  489   28 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   70.00   80.00   77.04   80.00  100.00     153 

histological_type: 
  glioblastoma multiforme (gbm)             treated primary gbm 
                             31                              20 
untreated primary (de novo) gbm 
                            544 

race: 
                    asian black or african american                     white 
                       13                        51                       506 
                     NA's 
                       25 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    13                    489                     93 

}}
\keyword{datasets}

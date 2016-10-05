\name{ OV }
\alias{ OV }
\docType{data}
\title{ OV }

\details{
\preformatted{
> OV
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeqGene: ExpressionSet with 19990 rows and 299 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 307 columns 
 [3] miRNASeqGene: ExpressionSet with 705 rows and 461 columns 
 [4] Methylation: SummarizedExperiment with 27578 rows and 591 columns 
 [5] mRNAArray: ExpressionSet with 18632 rows and 575 columns 
 [6] miRNAArray: ExpressionSet with 821 rows and 573 columns 
 [7] RPPAArray: ExpressionSet with 208 rows and 427 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 573 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 573 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( OV )
CharacterList of length 9
[["RNASeqGene"]] AADACL3 AADACL4 ABCA4 ABCB10 ABCD3 ... UTY VCY XGPY2 XKRY2 ZFY
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["Methylation"]] cg00000292 cg00002426 cg00003994 ... cg27662877 cg27665659
[["mRNAArray"]] C9orf152 ELMO2 RPS11 CREB3L1 PNMA1 ... SNRPD2 SLC39A6 CTSC AQP7
[["miRNAArray"]] DarkCorner dmr_285 dmr_3 ... NegativeControl SCorner3
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( OV )
CharacterList of length 9
[["RNASeqGene"]] TCGA-04-1348-01A TCGA-04-1357-01A ... TCGA-61-2113-01A
[["RNASeq2GeneNorm"]] TCGA-04-1348-01A TCGA-04-1357-01A ... TCGA-WR-A838-01A
[["miRNASeqGene"]] TCGA-04-1331-01A TCGA-04-1332-01A ... TCGA-61-2113-01A
[["Methylation"]] TCGA-04-1331-01A-01D-0432-05 ... TCGA-72-4241-01A-01D-1011-05
[["mRNAArray"]] TCGA-04-1331-01A TCGA-04-1332-01A ... TCGA-61-2614-01A
[["miRNAArray"]] TCGA-04-1331-01A TCGA-04-1332-01A ... TCGA-61-2614-01A
[["RPPAArray"]] TCGA-04-1335-01A TCGA-04-1336-01A ... TCGA-WR-A838-01A
[["gistica"]] TCGA-04-1331-01A-01D-0428-01 ... TCGA-WR-A838-01A-12D-A402-01
[["gistict"]] TCGA-04-1331-01A-01D-0428-01 ... TCGA-WR-A838-01A-12D-A402-01

Sizes of each ExperimentList element:

            assay size.Mb
1      RNASeqGene  1.1 Mb
2 RNASeq2GeneNorm  1.2 Mb
3    miRNASeqGene  0.1 Mb
4     Methylation  4.4 Mb
5       mRNAArray  1.1 Mb
6      miRNAArray  0.2 Mb
7       RPPAArray  0.1 Mb
8         gistica  4.4 Mb
9         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   247 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 343.00  343.00    2.94    2.76    3.17 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  590 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.00   51.00   59.00   59.78   69.00   89.00      21 

vital_status: 
  0   1 
246 344 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   567.5  1073.0  1147.0  1557.0  4624.0     247 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     349     889    1094    1560    5481      75 

tumor_tissue_site: 
         omentum            ovary peritoneum ovary             NA's 
               3              575                2               10 

gender: 
female   NA's 
   580     10 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2001    2004    2004    2007    2013      10 

radiation_therapy: 
  no  yes NA's 
 556    5   29 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   60.00   80.00   75.83   80.00  100.00     506 

histological_type: 
serous cystadenocarcinoma                      NA's 
                      580                        10 

residual_tumor: 
  r0   r1   r2   rx NA's 
  15   31    5    3  536 


ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    11                    338                    241 

}}
\keyword{datasets}

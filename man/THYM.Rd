\name{ THYM }
\alias{ THYM }
\docType{data}
\title{ THYM }

\details{
\preformatted{
> THYM
A MultiAssayExperiment object of 8 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 8: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 122 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 126 columns 
 [3] CNASNP: RangedRaggedAssay with 105646 rows and 248 columns 
 [4] CNVSNP: RangedRaggedAssay with 15571 rows and 248 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 126 columns 
 [6] RPPAArray: ExpressionSet with 192 rows and 90 columns 
 [7] gistica: SummarizedExperiment with 24776 rows and 123 columns 
 [8] gistict: SummarizedExperiment with 24776 rows and 123 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( THYM )
CharacterList of length 8
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 105640 105641 105642 105643 105644 105645 105646
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 15565 15566 15567 15568 15569 15570 15571
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( THYM )
CharacterList of length 8
[["RNASeq2GeneNorm"]] TCGA-3G-AB0O-01A TCGA-3G-AB0Q-01A ... TCGA-ZT-A8OM-01A
[["miRNASeqGene"]] TCGA-3G-AB0O-01A TCGA-3G-AB0Q-01A ... TCGA-ZT-A8OM-01A
[["CNASNP"]] TCGA-3G-AB0O-01A-22D-A422-01 ... TCGA-ZT-A8OM-10A-01D-A42A-01
[["CNVSNP"]] TCGA-3G-AB0O-01A-22D-A422-01 ... TCGA-ZT-A8OM-10A-01D-A42A-01
[["Methylation"]] TCGA-3G-AB0O-01A-22D-A424-05 ... TCGA-ZT-A8OM-01A-11D-A429-05
[["RPPAArray"]] TCGA-3G-AB0O-01A TCGA-3G-AB0Q-01A ... TCGA-ZL-A9V6-01A
[["gistica"]] TCGA-3G-AB0O-01A-22D-A422-01 ... TCGA-ZT-A8OM-01A-11D-A427-01
[["gistict"]] TCGA-3G-AB0O-01A-22D-A422-01 ... TCGA-ZT-A8OM-01A-11D-A427-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.1 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP  8.1 Mb
4          CNVSNP  1.3 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7         gistica  4.3 Mb
8         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   115 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
   9.00    9.00    2.34    1.04      NA 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  124 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  17.00   49.50   60.00   58.15   68.50   84.00       1 

vital_status: 
  0   1 
115   9 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    124     379     853    1423    2488    3488     115 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      4     714    1253    1476    1960    4575       7 

tumor_tissue_site: 
anterior mediastinum               thymus 
                  27                   97 

gender: 
female   male 
    60     64 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2000    2008    2010    2010    2012    2013       1 

radiation_therapy: 
 no yes 
 81  43 


race: 
                    asian black or african american                     white 
                       13                         6                       103 
                     NA's 
                        2 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    10                    100                     14 

}}
\keyword{datasets}

\name{ LIHC }
\alias{ LIHC }
\docType{data}
\title{ LIHC }

\details{
\preformatted{
> LIHC
A MultiAssayExperiment object of 7 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 7: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 26 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 423 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 424 columns 
 [4] Methylation: SummarizedExperiment with 485577 rows and 429 columns 
 [5] RPPAArray: ExpressionSet with 193 rows and 0 columns 
 [6] gistica: SummarizedExperiment with 24776 rows and 370 columns 
 [7] gistict: SummarizedExperiment with 24776 rows and 370 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( LIHC )
CharacterList of length 7
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( LIHC )
CharacterList of length 7
[["RNASeqGene"]] TCGA-BC-4073-01B TCGA-BC-A10Q-11A ... TCGA-DD-A1EH-01A
[["RNASeq2GeneNorm"]] TCGA-2V-A95S-01A TCGA-2Y-A9GS-01A ... TCGA-ZS-A9CG-01A
[["miRNASeqGene"]] TCGA-2V-A95S-01A TCGA-2Y-A9GS-01A ... TCGA-ZS-A9CG-01A
[["Methylation"]] TCGA-2V-A95S-01A-11D-A36Y-05 ... TCGA-ZS-A9CG-01A-11D-A36Y-05
[["RPPAArray"]] character(0)
[["gistica"]] TCGA-2V-A95S-01A-11D-A36W-01 ... TCGA-ZS-A9CG-01A-11D-A36W-01
[["gistict"]] TCGA-2V-A95S-01A-11D-A36W-01 ... TCGA-ZS-A9CG-01A-11D-A36W-01

Sizes of each ExperimentList element:

            assay size.Mb
1      RNASeqGene  1.1 Mb
2 RNASeq2GeneNorm  1.2 Mb
3    miRNASeqGene  0.1 Mb
4     Methylation 67.5 Mb
5       RPPAArray    0 Mb
6         gistica  4.3 Mb
7         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   245 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
132.000 132.000   1.144   0.956   1.633 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  377 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  16.00   51.00   61.00   59.27   69.00   87.00       4 

vital_status: 
  0   1 
245 132 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   194.8   417.5   672.2   837.0  3258.0     245 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   328.0   579.0   783.8  1067.0  3675.0      92 

tumor_tissue_site: 
liver 
  377 



pathology_N_stage: 
  n0   n1   nx NA's 
 257    4  115    1 

pathology_M_stage: 
 m0  m1  mx 
272   4 101 

gender: 
female   male 
   122    255 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1995    2008    2011    2010    2012    2013       3 

radiation_therapy: 
  no  yes NA's 
 345    9   23 

histological_type: 
         fibrolamellar carcinoma         hepatocellular carcinoma 
                               3                              367 
hepatocholangiocarcinoma (mixed) 
                               7 

residual_tumor: 
  r0   r1   r2   rx NA's 
 330   17    1   22    7 

race: 
american indian or alaska native                            asian 
                               2                              161 
       black or african american                            white 
                              17                              187 
                            NA's 
                              10 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    18                    340                     19 

}}
\keyword{datasets}

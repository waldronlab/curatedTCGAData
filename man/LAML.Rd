\name{ LAML }
\alias{ LAML }
\docType{data}
\title{ LAML }

\details{
\preformatted{
> LAML
A MultiAssayExperiment object of 5 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 5: 
 [1] RNASeqGene: ExpressionSet with 19990 rows and 179 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 173 columns 
 [3] CNASNP: RangedRaggedAssay with 874897 rows and 392 columns 
 [4] CNVSNP: RangedRaggedAssay with 28324 rows and 380 columns 
 [5] Methylation: SummarizedExperiment with 27578 rows and 194 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( LAML )
CharacterList of length 5
[["RNASeqGene"]] AADACL3 AADACL4 ABCA4 ABCB10 ABCD3 ... UTY VCY XGPY2 XKRY2 ZFY
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["CNASNP"]] 1 2 3 4 5 6 7 ... 874891 874892 874893 874894 874895 874896 874897
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 28318 28319 28320 28321 28322 28323 28324
[["Methylation"]] cg00000292 cg00002426 cg00003994 ... cg27662877 cg27665659

> colnames( LAML )
CharacterList of length 5
[["RNASeqGene"]] TCGA-AB-2803-03A TCGA-AB-2805-03A ... TCGA-AB-3012-03A
[["RNASeq2GeneNorm"]] TCGA-AB-2803-03A TCGA-AB-2805-03A ... TCGA-AB-3012-03A
[["CNASNP"]] TCGA-AB-2802-03A-01D-0756-21 ... TCGA-AB-3012-11A-01D-0756-21
[["CNVSNP"]] TCGA-AB-2803-03A-01D-0756-21 ... TCGA-AB-3012-11A-01D-0756-21
[["Methylation"]] TCGA-AB-2802-03A-01D-0741-05 ... TCGA-AB-3012-03A-01D-0741-05

Sizes of each ExperimentList element:

            assay size.Mb
1      RNASeqGene  1.1 Mb
2 RNASeq2GeneNorm  1.2 Mb
3          CNASNP 66.9 Mb
4          CNVSNP  2.3 Mb
5     Methylation  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   80 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
120.000 120.000   0.748   0.586   0.918 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  200 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  18.00   44.75   57.00   55.02   67.00   88.00 

vital_status: 
  0   1 
 67 133 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    91.5   273.0   355.7   489.0  1706.0      80 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   296.2   699.0   913.5  1507.0  2861.0     132 

tumor_tissue_site: 
bone marrow        NA's 
        199           1 

gender: 
female   male 
    91    109 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2001    2004    2006    2006    2008    2010 

race: 
                    asian black or african american                     white 
                        2                        15                       181 
                     NA's 
                        2 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     3                    194                      3 

}}
\keyword{datasets}

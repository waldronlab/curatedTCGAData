\name{ STAD }
\alias{ STAD }
\docType{data}
\title{ Stomach adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( STAD )
ExperimentList class object of length 12: 
 [1] CNASeq-20160128: RaggedExperiment with 31824 rows and 214 columns 
 [2] CNASNP-20160128: RaggedExperiment with 443042 rows and 906 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 118389 rows and 904 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 441 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 441 columns 
 [6] Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 73 columns 
 [7] Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 397 columns 
 [8] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 430 columns 
 [9] Mutation-20160128: RaggedExperiment with 148520 rows and 289 columns 
 [10] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 450 columns 
 [11] RNASeqGene-20160128: SummarizedExperiment with 26120 rows and 36 columns 
 [12] RPPAArray-20160128: SummarizedExperiment with 195 rows and 357 columns 

> rownames( STAD )
CharacterList of length 12
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation_methyl27-20160128"]] cg00000292 cg00002426 ... cg27665659
[["Methylation_methyl450-20160128"]] cg00000029 cg00000108 ... rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
...
<2 more elements>

> colnames( STAD )
CharacterList of length 12
[["CNASeq-20160128"]] TCGA-B7-5816-01A-21D-1598-02 ...
[["CNASNP-20160128"]] TCGA-3M-AB46-01A-11D-A40Z-01 ...
[["CNVSNP-20160128"]] TCGA-3M-AB46-01A-11D-A40Z-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-3M-AB46-01A-11D-A40Z-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-3M-AB46-01A-11D-A40Z-01 ...
[["Methylation_methyl27-20160128"]] TCGA-BR-4183-11A-01D-1129-05 ...
[["Methylation_methyl450-20160128"]] TCGA-3M-AB46-01A-11D-A411-05 ...
[["miRNASeqGene-20160128"]] TCGA-3M-AB46-01A-11R-A415-13 ...
[["Mutation-20160128"]] TCGA-B7-5816-01A-21D-1600-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-3M-AB46-01A-11R-A414-31 ...
...
<2 more elements>

Sizes of each ExperimentList element:

                               assay  size.Mb
1                    CNASeq-20160128   0.9 Mb
2                    CNASNP-20160128  12.1 Mb
3                    CNVSNP-20160128   3.4 Mb
4          GISTIC_AllByGene-20160128   4.9 Mb
5  GISTIC_ThresholdedByGene-20160128   4.9 Mb
6      Methylation_methyl27-20160128   4.9 Mb
7     Methylation_methyl450-20160128    75 Mb
8              miRNASeqGene-20160128   0.1 Mb
9                  Mutation-20160128 161.5 Mb
10          RNASeq2GeneNorm-20160128   1.3 Mb
11               RNASeqGene-20160128   1.7 Mb
12                RPPAArray-20160128     0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   273 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
170.000 170.000   0.948   0.792   1.085 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   58.00   67.00   65.73   73.00   90.00       9 

vital_status:
  0   1 
268 175 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   194.0   346.0   423.7   553.5  2197.0     273 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   335.5   547.5   673.7   912.0  3720.0     177 

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

Including an additional 1390 columns
}}
\keyword{datasets}

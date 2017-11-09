\name{ HNSC }
\alias{ HNSC }
\docType{data}
\title{ Head and Neck squamous cell carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( HNSC )
ExperimentList class object of length 11: 
 [1] CNASeq-20160128: RaggedExperiment with 32905 rows and 225 columns 
 [2] CNASNP-20160128: RaggedExperiment with 499142 rows and 1090 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 110289 rows and 1089 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 522 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 522 columns 
 [6] Methylation-20160128: SummarizedExperiment with 485577 rows and 580 columns 
 [7] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 532 columns 
 [8] Mutation-20160128: RaggedExperiment with 51799 rows and 279 columns 
 [9] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 566 columns 
 [10] RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 294 columns 
 [11] RPPAArray-20160128: SummarizedExperiment with 160 rows and 212 columns 

> rownames( HNSC )
CharacterList of length 11
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RNASeqGene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
...
<1 more element>

> colnames( HNSC )
CharacterList of length 11
[["CNASeq-20160128"]] TCGA-BA-4074-01A-01D-1431-02 ...
[["CNASNP-20160128"]] TCGA-4P-AA8J-01A-11D-A390-01 ...
[["CNVSNP-20160128"]] TCGA-4P-AA8J-01A-11D-A390-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-4P-AA8J-01A-11D-A390-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-4P-AA8J-01A-11D-A390-01 ...
[["Methylation-20160128"]] TCGA-4P-AA8J-01A-11D-A392-05 ...
[["miRNASeqGene-20160128"]] TCGA-4P-AA8J-01A-11R-A39B-13 ...
[["Mutation-20160128"]] TCGA-BA-4074-01A-01D-1434-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-4P-AA8J-01A-11R-A39I-07 ...
[["RNASeqGene-20160128"]] TCGA-BA-4074-01A-01R-1436-07 ...
...
<1 more element>

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128    1 Mb
2                    CNASNP-20160128 13.6 Mb
3                    CNVSNP-20160128  3.3 Mb
4          GISTIC_AllByGene-20160128  4.9 Mb
5  GISTIC_ThresholdedByGene-20160128  4.9 Mb
6               Methylation-20160128 75.1 Mb
7              miRNASeqGene-20160128  0.1 Mb
8                  Mutation-20160128 68.9 Mb
9           RNASeq2GeneNorm-20160128  1.3 Mb
10               RNASeqGene-20160128  1.3 Mb
11                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   305 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 223.00  223.00    1.18    1.06    1.35 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  19.00   53.00   61.00   60.91   69.00   89.00       1 

vital_status:
  0   1 
304 224 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   260.0   430.0   740.0   814.5  6417.0     305 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   529.5   851.0  1042.9  1404.0  5480.0     225 

tumor_tissue_site:
head and neck 
          528 




pathology_M_stage:
  m0   m1   mx NA's 
 191    1   65  271 

gender:
female   male 
   142    386 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2007    2010    2008    2011    2013       1 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   98.0   483.0   848.5  1226.9  1647.5  3930.0     506 

radiation_therapy:
  no  yes NA's 
 163  303   62 

histological_type:
                      head & neck squamous cell carcinoma 
                                                      517 
        head & neck squamous cell carcinoma basaloid type 
                                                       10 
head & neck squamous cell carcinoma, spindle cell variant 
                                                        1 

number_pack_years_smoked:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
  0.01685  25.00000  40.00000  45.75496  60.00000 300.00000       230 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1936    1959    1968    1967    1975    2001     246 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.186   3.000  44.000     115 

race:
american indian or alaska native                            asian 
                               2                               11 
       black or african american                            white 
                              48                              452 
                            NA's 
                              15 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    26                    465                     37 

Including an additional 1426 columns
}}
\keyword{datasets}

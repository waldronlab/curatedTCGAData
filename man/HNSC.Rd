\name{ HNSC }
\alias{ HNSC }
\docType{data}
\title{ HNSC }

\details{
\preformatted{
> HNSC
A MultiAssayExperiment object of 11 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 11: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 294 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 566 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 532 columns 
 [4] CNASNP: RangedRaggedAssay with 499142 rows and 1090 columns 
 [5] CNVSNP: RangedRaggedAssay with 110289 rows and 1089 columns 
 [6] CNAseq: RangedRaggedAssay with 32905 rows and 225 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 580 columns 
 [8] RPPAArray: ExpressionSet with 160 rows and 212 columns 
 [9] Mutations: RangedRaggedAssay with 51799 rows and 279 columns 
 [10] gistica: SummarizedExperiment with 24776 rows and 522 columns 
 [11] gistict: SummarizedExperiment with 24776 rows and 522 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( HNSC )
CharacterList of length 11
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 499136 499137 499138 499139 499140 499141 499142
[["CNVSNP"]] 1 2 3 4 5 6 7 ... 110283 110284 110285 110286 110287 110288 110289
[["CNAseq"]] 1 2 3 4 5 6 7 8 ... 32899 32900 32901 32902 32903 32904 32905
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_epsilon 4E-BP1 ... p70S6K_pT389 p90RSK_pT359_S363
[["Mutations"]] SLC2A7 VPS13D KIAA0467 BEND5 DIO1 ... FLNA ABCC3 DNAH8 AKAP9
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
...
<1 more element>

> colnames( HNSC )
CharacterList of length 11
[["RNASeqGene"]] TCGA-BA-4074-01A TCGA-BA-4075-01A ... TCGA-IQ-7632-01A
[["RNASeq2GeneNorm"]] TCGA-4P-AA8J-01A TCGA-BA-4074-01A ... TCGA-WA-A7H4-01A
[["miRNASeqGene"]] TCGA-4P-AA8J-01A TCGA-BA-5149-01A ... TCGA-WA-A7H4-01A
[["CNASNP"]] TCGA-4P-AA8J-01A-11D-A390-01 ... TCGA-WA-A7H4-10A-01D-A34L-01
[["CNVSNP"]] TCGA-4P-AA8J-01A-11D-A390-01 ... TCGA-WA-A7H4-10A-01D-A34L-01
[["CNAseq"]] TCGA-BA-4074-01A-01D-1431-02 ... TCGA-IQ-7632-10A-01D-2317-26
[["Methylation"]] TCGA-4P-AA8J-01A-11D-A392-05 ... TCGA-WA-A7H4-01A-21D-A34K-05
[["RPPAArray"]] TCGA-BA-4074-01A TCGA-BA-4078-01A ... TCGA-IQ-7632-01A
[["Mutations"]] TCGA-BA-4074-01A-01D-1434-08 ... TCGA-IQ-7632-01A-11D-2078-08
[["gistica"]] TCGA-4P-AA8J-01A-11D-A390-01 ... TCGA-WA-A7H4-01A-21D-A34I-01
...
<1 more element>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.2 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.2 Mb
4           CNASNP 38.4 Mb
5           CNVSNP  8.7 Mb
6           CNAseq  2.6 Mb
7      Methylation 67.5 Mb
8        RPPAArray  0.1 Mb
9        Mutations 65.4 Mb
10         gistica  4.4 Mb
11         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   305 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 223.00  223.00    1.18    1.06    1.35 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  528 

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
   -2.0   414.5   723.5   957.6  1278.0  5480.0     162 

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
   98.0   483.0   848.5  1227.0  1648.0  3930.0     506 

radiation_therapy: 
  no  yes NA's 
 163  301   64 

histological_type: 
                      head and neck squamous cell carcinoma 
                                                        517 
head and neck squamous cell carcinoma  spindle cell variant 
                                                          1 
        head and neck squamous cell carcinoma basaloid type 
                                                         10 

number_pack_years_smoked: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
  0.01685  25.00000  40.00000  45.75000  60.00000 300.00000       230 

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

}}
\keyword{datasets}

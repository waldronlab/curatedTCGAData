\name{ KICH }
\alias{ KICH }
\docType{data}
\title{ KICH }

\details{
\preformatted{
> KICH
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 91 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 91 columns 
 [3] CNASNP: RangedRaggedAssay with 57729 rows and 132 columns 
 [4] CNVSNP: RangedRaggedAssay with 10164 rows and 132 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 66 columns 
 [6] RPPAArray: ExpressionSet with 193 rows and 63 columns 
 [7] Mutations: RangedRaggedAssay with 7559 rows and 66 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 66 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 66 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( KICH )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 8 ... 57723 57724 57725 57726 57727 57728 57729
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 10158 10159 10160 10161 10162 10163 10164
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] CUBN PTEN RASAL2 OBSCN MESDC1 ... LPP CATSPER3 EBF2 SLC52A2
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( KICH )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-KL-8323-01A TCGA-KL-8324-11A ... TCGA-KO-8417-01A
[["miRNASeqGene"]] TCGA-KL-8323-01A TCGA-KL-8324-11A ... TCGA-KO-8417-01A
[["CNASNP"]] TCGA-KL-8323-01A-21D-2308-01 ... TCGA-KO-8417-11A-01D-2309-01
[["CNVSNP"]] TCGA-KL-8323-01A-21D-2308-01 ... TCGA-KO-8417-11A-01D-2309-01
[["Methylation"]] TCGA-KL-8323-01A-21D-2312-05 ... TCGA-KO-8417-01A-11D-2312-05
[["RPPAArray"]] TCGA-KL-8324-01A TCGA-KL-8325-01A ... TCGA-KO-8417-01A
[["Mutations"]] TCGA-KL-8323-01A-21D-2310-10 ... TCGA-KO-8417-01A-11D-2310-10
[["gistica"]] TCGA-KL-8323-01A-21D-2308-01 ... TCGA-KO-8417-01A-11D-2308-01
[["gistict"]] TCGA-KL-8323-01A-21D-2308-01 ... TCGA-KO-8417-01A-11D-2308-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.1 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP  4.5 Mb
4          CNVSNP  0.8 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7       Mutations  2.5 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   57 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
   9.00    9.00    2.34    1.99      NA 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
   66 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  17.00   42.00   50.00   51.52   61.75   86.00 

vital_status: 
 0  1 
56 10 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    325     725     855    1001    1158    2172      57 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     30    1366    2674    2311    3136    4676       9 

tumor_tissue_site: 
kidney 
    66 

pathologic_stage: 
  stage i  stage ii stage iii  stage iv 
       21        25        14         6 


pathology_N_stage: 
n0 n1 n2 nx 
40  3  2 21 

pathology_M_stage: 
  m0   m1   mx NA's 
  34    2    9   21 

gender: 
female   male 
    27     39 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2000    2004    2005    2005    2007    2011 

radiation_therapy: 
no 
66 

karnofsky_performance_score: 
  90  100 NA's 
   3   10   53 

histological_type: 
kidney chromophobe 
                66 

number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   24.00   25.09   31.00   75.00      55 

year_of_tobacco_smoking_onset: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1949    1964    1974    1974    1984    1997      58 

race: 
                    asian black or african american                     white 
                        2                         4                        58 
                     NA's 
                        2 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     4                     32                     30 

}}
\keyword{datasets}

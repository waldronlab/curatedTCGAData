\name{ESCA-v2.1.0}
\alias{ ESCA-v2.1.0 }
\docType{data}
\title{ Esophageal carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( ESCA )
ExperimentList class object of length 13:
 [1] ESCA_CNASeq-20160128: RaggedExperiment with 17059 rows and 104 columns
 [2] ESCA_CNASNP-20160128: RaggedExperiment with 203186 rows and 373 columns
 [3] ESCA_CNVSNP-20160128: RaggedExperiment with 60803 rows and 373 columns
 [4] ESCA_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 184 columns
 [5] ESCA_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 77 rows and 184 columns
 [6] ESCA_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 184 columns
 [7] ESCA_Mutation-20160128: RaggedExperiment with 58602 rows and 185 columns
 [8] ESCA_RNASeq2Gene-20160128: SummarizedExperiment with 20501 rows and 196 columns
 [9] ESCA_RNASeqGene-20160128: SummarizedExperiment with 26120 rows and 198 columns
 [10] ESCA_RPPAArray-20160128: SummarizedExperiment with 192 rows and 126 columns
 [11] ESCA_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 198 columns
 [12] ESCA_RNASeq2GeneNorm-20160128: SummarizedExperiment with 18730 rows and 196 columns
 [13] ESCA_Methylation-20160128: SummarizedExperiment with 485577 rows and 202 columns

> rownames( ESCA )
CharacterList of length 13
[["ESCA_CNASeq-20160128"]] character(0)
[["ESCA_CNASNP-20160128"]] character(0)
[["ESCA_CNVSNP-20160128"]] character(0)
[["ESCA_GISTIC_AllByGene-20160128"]] character(0)
[["ESCA_GISTIC_Peaks-20160128"]] 31 1 32 2 3 33 4 34 ... 73 74 29 75 76 77 78
[["ESCA_GISTIC_ThresholdedByGene-20160128"]] character(0)
[["ESCA_Mutation-20160128"]] character(0)
[["ESCA_RNASeq2Gene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["ESCA_RNASeqGene-20160128"]] AADACL3 AADACL4 AB007962 ... VCY XKRY ZFY
[["ESCA_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
...
<3 more elements>

> colnames( ESCA )
CharacterList of length 13
[["ESCA_CNASeq-20160128"]] TCGA-IG-A3I8-01A-11D-A248-26 ...
[["ESCA_CNASNP-20160128"]] TCGA-2H-A9GF-01A-11D-A37B-01 ...
[["ESCA_CNVSNP-20160128"]] TCGA-2H-A9GF-01A-11D-A37B-01 ...
[["ESCA_GISTIC_AllByGene-20160128"]] TCGA-2H-A9GF-01A-11D-A37B-01 ...
[["ESCA_GISTIC_Peaks-20160128"]] TCGA-2H-A9GF-01A-11D-A37B-01 ...
[["ESCA_GISTIC_ThresholdedByGene-20160128"]] TCGA-2H-A9GF-01A-11D-A37B-01 ...
[["ESCA_Mutation-20160128"]] TCGA-2H-A9GF-01A-11D-A37C-09 ...
[["ESCA_RNASeq2Gene-20160128"]] TCGA-2H-A9GF-01A-11R-A37I-31 ...
[["ESCA_RNASeqGene-20160128"]] TCGA-2H-A9GF-01A-11R-A37I-31 ...
[["ESCA_RPPAArray-20160128"]] TCGA-2H-A9GF-01A-21-A41Y-20 ...
...
<3 more elements>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    ESCA_CNASeq-20160128  0.5 Mb
2                    ESCA_CNASNP-20160128  5.5 Mb
3                    ESCA_CNVSNP-20160128  1.7 Mb
4          ESCA_GISTIC_AllByGene-20160128 38.3 Mb
5              ESCA_GISTIC_Peaks-20160128  0.2 Mb
6  ESCA_GISTIC_ThresholdedByGene-20160128 38.1 Mb
7                  ESCA_Mutation-20160128   45 Mb
8               ESCA_RNASeq2Gene-20160128 33.2 Mb
9                ESCA_RNASeqGene-20160128 42.8 Mb
10                ESCA_RPPAArray-20160128  0.2 Mb
11             ESCA_miRNASeqGene-20160128  1.8 Mb
12          ESCA_RNASeq2GeneNorm-20160128 30.4 Mb
13              ESCA_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   108 observations deleted due to missingness 
      n events median 0.95LCL 0.95UCL
[1,] 77     77  0.962   0.677    1.35


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  27.00   54.00   61.00   62.46   72.00   90.00 

vital_status:
  0   1 
108  77 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   180.0   351.0   495.2   650.0  2532.0     108 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   336.5   402.5   570.1   696.8  3714.0      77 

tumor_tissue_site:
esophagus 
      185 




pathology_M_stage:
  m0   m1  m1a   mx NA's 
 136    4    5   18   22 

gender:
female   male 
    27    158 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1998    2007    2011    2009    2012    2013       7 

radiation_therapy:
  no  yes NA's 
 124   43   18 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   60.00   80.00   73.82   90.00  100.00     117 

histological_type:
    esophagus adenocarcinoma, nos esophagus squamous cell carcinoma 
                               89                                96 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   17.62   30.00   34.48   47.25  102.00      87 

residual_tumor:
  r0   r1   r2   rx NA's 
 137   13    2    7   26 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.526   4.000  21.000      50 

race:
                    asian black or african american                     white 
                       46                         5                       114 
                     NA's 
                       20 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     6                     88                     91 

Including an additional 940 columns
}}
\keyword{datasets}

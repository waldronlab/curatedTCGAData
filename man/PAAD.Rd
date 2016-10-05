\name{ PAAD }
\alias{ PAAD }
\docType{data}
\title{ PAAD }

\details{
\preformatted{
> PAAD
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 183 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 183 columns 
 [3] CNASNP: RangedRaggedAssay with 203871 rows and 368 columns 
 [4] CNVSNP: RangedRaggedAssay with 34808 rows and 365 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 195 columns 
 [6] RPPAArray: ExpressionSet with 195 rows and 123 columns 
 [7] Mutations: RangedRaggedAssay with 37705 rows and 146 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 184 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 184 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( PAAD )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 203865 203866 203867 203868 203869 203870 203871
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 34802 34803 34804 34805 34806 34807 34808
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] SMPD1 EXPH5 MBD6 LCE2A ... ERLIN2 KCNMA1 RP11-812E19.9 FAT1
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( PAAD )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-2J-AAB1-01A TCGA-2J-AAB4-01A ... TCGA-Z5-AAPL-01A
[["miRNASeqGene"]] TCGA-2J-AAB1-01A TCGA-2J-AAB4-01A ... TCGA-Z5-AAPL-01A
[["CNASNP"]] TCGA-2J-AAB1-01A-11D-A40V-01 ... TCGA-Z5-AAPL-10A-01D-A40V-01
[["CNVSNP"]] TCGA-2J-AAB1-01A-11D-A40V-01 ... TCGA-Z5-AAPL-10A-01D-A40V-01
[["Methylation"]] TCGA-2J-AAB1-01A-11D-A40Y-05 ... TCGA-Z5-AAPL-01A-12D-A40Y-05
[["RPPAArray"]] TCGA-2J-AAB4-01A TCGA-2J-AAB8-01A ... TCGA-Z5-AAPL-01A
[["Mutations"]] TCGA-2J-AAB1-01A-11D-A40W-08 ... TCGA-Z5-AAPL-01A-12D-A40W-08
[["gistica"]] TCGA-2J-AAB1-01A-11D-A40V-01 ... TCGA-Z5-AAPL-01A-12D-A40V-01
[["gistict"]] TCGA-2J-AAB1-01A-11D-A40V-01 ... TCGA-Z5-AAPL-01A-12D-A40V-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.2 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP 15.7 Mb
4          CNVSNP  2.8 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7       Mutations 52.1 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   85 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
100.000 100.000   1.079   0.967   1.315 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  185 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  35.00   57.00   65.00   64.86   73.00   88.00 

vital_status: 
  0   1 
 85 100 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   228.5   394.0   459.5   596.5  2182.0      85 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   186.5   416.0   553.9   718.0  2741.0      66 

tumor_tissue_site: 
pancreas 
     185 



pathology_N_stage: 
  n0   n1  n1b   nx NA's 
  50  126    4    4    1 

pathology_M_stage: 
m0 m1 mx 
85  5 95 

gender: 
female   male 
    83    102 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2001    2010    2012    2011    2012    2013       1 

radiation_therapy: 
  no  yes NA's 
 125   45   15 

histological_type: 
             pancreas-adenocarcinoma ductal type 
                                             154 
           pancreas-adenocarcinoma-other subtype 
                                              25 
pancreas-colloid (mucinous non-cystic) carcinoma 
                                               4 
             pancreas-undifferentiated carcinoma 
                                               1 
                                            NA's 
                                               1 

number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.30   15.00   25.00   26.84   40.00   75.00     128 

year_of_tobacco_smoking_onset: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1948    1960    1971    1971    1982    1993     138 

residual_tumor: 
  r0   r1   r2   rx NA's 
 111   53    5    4   12 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   2.989   4.000  16.000       4 

race: 
                    asian black or african american                     white 
                       11                         7                       162 
                     NA's 
                        5 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     5                    137                     43 

}}
\keyword{datasets}

\name{ MESO }
\alias{ MESO }
\docType{data}
\title{ MESO }

\details{
\preformatted{
> MESO
A MultiAssayExperiment object of 8 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 8: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 87 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 87 columns 
 [3] CNASNP: RangedRaggedAssay with 90003 rows and 173 columns 
 [4] CNVSNP: RangedRaggedAssay with 18335 rows and 172 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 87 columns 
 [6] RPPAArray: ExpressionSet with 193 rows and 63 columns 
 [7] gistica: SummarizedExperiment with 24776 rows and 87 columns 
 [8] gistict: SummarizedExperiment with 24776 rows and 87 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( MESO )
CharacterList of length 8
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 8 ... 89997 89998 89999 90000 90001 90002 90003
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 18329 18330 18331 18332 18333 18334 18335
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( MESO )
CharacterList of length 8
[["RNASeq2GeneNorm"]] TCGA-3H-AB3K-01A TCGA-3H-AB3L-01A ... TCGA-ZN-A9VW-01A
[["miRNASeqGene"]] TCGA-3H-AB3K-01A TCGA-3H-AB3L-01A ... TCGA-ZN-A9VW-01A
[["CNASNP"]] TCGA-3H-AB3K-01A-11D-A39Q-01 ... TCGA-ZN-A9VW-10A-01D-A39T-01
[["CNVSNP"]] TCGA-3H-AB3K-01A-11D-A39Q-01 ... TCGA-ZN-A9VW-10A-01D-A39T-01
[["Methylation"]] TCGA-3H-AB3K-01A-11D-A39S-05 ... TCGA-ZN-A9VW-01A-11D-A39S-05
[["RPPAArray"]] TCGA-3H-AB3K-01A TCGA-3H-AB3L-01A ... TCGA-ZN-A9VW-01A
[["gistica"]] TCGA-3H-AB3K-01A-11D-A39Q-01 ... TCGA-ZN-A9VW-01A-11D-A39Q-01
[["gistict"]] TCGA-3H-AB3K-01A-11D-A39Q-01 ... TCGA-ZN-A9VW-01A-11D-A39Q-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.1 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP  6.9 Mb
4          CNVSNP  1.5 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7         gistica  4.3 Mb
8         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   16 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  71.00   71.00    1.23    1.05    1.57 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
   87 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  28.00   57.00   64.00   62.99   69.00   81.00 

vital_status: 
 0  1 
16 71 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   243.5   449.0   567.2   749.0  2790.0      16 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -8.0   256.0   501.0   690.1   977.0  2024.0      58 

tumor_tissue_site: 
pleura 
    87 



pathology_N_stage: 
n0 n1 n2 n3 nx 
44 10 26  3  4 

pathology_M_stage: 
m0 m1 mx 
57  3 27 

gender: 
female   male 
    16     71 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1999    2008    2010    2009    2012    2013 

radiation_therapy: 
  no  yes NA's 
  59   24    4 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   80.00   90.00   77.65   90.00  100.00      70 

histological_type: 
               biphasic mesothelioma diffuse malignant mesothelioma - nos 
                                  23                                    5 
            epithelioid mesothelioma             sarcomatoid mesothelioma 
                                  57                                    2 

residual_tumor: 
  r0   r1   r2   rx NA's 
  17    3   15   11   41 

race: 
                    asian black or african american                     white 
                        1                         1                        85 

ethnicity: 
not hispanic or latino                   NA's 
                    73                     14 

}}
\keyword{datasets}

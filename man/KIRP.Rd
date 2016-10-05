\name{ KIRP }
\alias{ KIRP }
\docType{data}
\title{ KIRP }

\details{
\preformatted{
> KIRP
A MultiAssayExperiment object of 11 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 11: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 14 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 323 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 326 columns 
 [4] CNASNP: RangedRaggedAssay with 300681 rows and 593 columns 
 [5] CNVSNP: RangedRaggedAssay with 46914 rows and 590 columns 
 [6] Methylation: SummarizedExperiment with 485577 rows and 321 columns 
 [7] mRNAArray: ExpressionSet with 17814 rows and 16 columns 
 [8] RPPAArray: ExpressionSet with 195 rows and 216 columns 
 [9] Mutations: RangedRaggedAssay with 15585 rows and 161 columns 
 [10] gistica: SummarizedExperiment with 24776 rows and 288 columns 
 [11] gistict: SummarizedExperiment with 24776 rows and 288 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( KIRP )
CharacterList of length 11
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 300675 300676 300677 300678 300679 300680 300681
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 46908 46909 46910 46911 46912 46913 46914
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] WDR64 PCDH15 DOCK1 CCKBR LRP4 ... GABRQ CUX2 GPR112 TGM7 RERE
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
...
<1 more element>

> colnames( KIRP )
CharacterList of length 11
[["RNASeqGene"]] TCGA-AL-3466-01A TCGA-AL-3467-01A ... TCGA-B9-4617-01A
[["RNASeq2GeneNorm"]] TCGA-2K-A9WE-01A TCGA-2Z-A9J1-01A ... TCGA-Y8-A8S1-01A
[["miRNASeqGene"]] TCGA-2K-A9WE-01A TCGA-2Z-A9J1-01A ... TCGA-Y8-A8S1-01A
[["CNASNP"]] TCGA-2K-A9WE-01A-11D-A381-01 ... TCGA-Y8-A8S1-11A-11D-A36Z-01
[["CNVSNP"]] TCGA-2K-A9WE-01A-11D-A381-01 ... TCGA-Y8-A8S1-11A-11D-A36Z-01
[["Methylation"]] TCGA-2K-A9WE-01A-11D-A383-05 ... TCGA-Y8-A8S1-01A-11D-A36Y-05
[["mRNAArray"]] TCGA-AL-3466-01A TCGA-AL-3467-01A ... TCGA-B9-4617-01A
[["RPPAArray"]] TCGA-2K-A9WE-01A TCGA-2Z-A9J1-01A ... TCGA-Y8-A8S1-01A
[["Mutations"]] TCGA-A4-7286-01A-11D-2136-08 ... TCGA-Q2-A5QZ-01A-11D-A28G-10
[["gistica"]] TCGA-2K-A9WE-01A-11D-A381-01 ... TCGA-Y8-A8S1-01A-11D-A36W-01
...
<1 more element>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.1 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 23.1 Mb
5           CNVSNP  3.8 Mb
6      Methylation 67.4 Mb
7        mRNAArray    1 Mb
8        RPPAArray  0.1 Mb
9        Mutations  9.9 Mb
10         gistica  4.3 Mb
11         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   247 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  44.00   44.00    1.76    1.35    3.60 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  291 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  28.00   53.25   61.50   61.52   71.00   88.00       5 

vital_status: 
  0   1 
247  44 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  122.0   341.2   641.0   989.8  1498.0  2941.0     247 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     406     766    1042    1500    5925      33 

tumor_tissue_site: 
kidney 
   291 

pathologic_stage: 
  stage i  stage ii stage iii  stage iv      NA's 
      173        23        52        15        28 


pathology_N_stage: 
  n0   n1   n2   nx NA's 
  50   24    4  212    1 

pathology_M_stage: 
  m0   m1   mx NA's 
  95    9  172   15 

gender: 
female   male 
    77    214 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1996    2008    2011    2010    2012    2013      25 

days_to_last_known_alive: 
  34 NA's 
   1  290 

radiation_therapy: 
  no  yes NA's 
 209    1   81 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00   90.00   87.66  100.00  100.00     214 

histological_type: 
kidney papillary renal cell carcinoma 
                                  291 

number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   15.00   24.50   31.73   41.25  185.00     215 

year_of_tobacco_smoking_onset: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1941    1961    1970    1972    1984    2010     235 

race: 
american indian or alaska native                            asian 
                               2                                6 
       black or african american                            white 
                              61                              207 
                            NA's 
                              15 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    12                    243                     36 

}}
\keyword{datasets}

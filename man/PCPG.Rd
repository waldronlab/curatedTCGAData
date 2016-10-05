\name{ PCPG }
\alias{ PCPG }
\docType{data}
\title{ PCPG }

\details{
\preformatted{
> PCPG
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 187 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 187 columns 
 [3] CNASNP: RangedRaggedAssay with 297329 rows and 360 columns 
 [4] CNVSNP: RangedRaggedAssay with 31256 rows and 346 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 187 columns 
 [6] RPPAArray: ExpressionSet with 192 rows and 82 columns 
 [7] Mutations: RangedRaggedAssay with 4662 rows and 184 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 162 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 162 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( PCPG )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 297323 297324 297325 297326 297327 297328 297329
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 31250 31251 31252 31253 31254 31255 31256
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] NUP205 TTN UGT2B17 GATAD1 ... GGT3P KMT2D LOC100294341 CDC42BPG
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( PCPG )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-P7-A5NX-01A TCGA-P7-A5NY-05A ... TCGA-XG-A823-01A
[["miRNASeqGene"]] TCGA-P7-A5NX-01A TCGA-P7-A5NY-05A ... TCGA-XG-A823-01A
[["CNASNP"]] TCGA-P7-A5NX-10A-01D-A35A-01 ... TCGA-XG-A823-10A-01D-A35F-01
[["CNVSNP"]] TCGA-P7-A5NX-10A-01D-A35A-01 ... TCGA-XG-A823-10A-01D-A35F-01
[["Methylation"]] TCGA-P7-A5NX-01A-11D-A35E-05 ... TCGA-XG-A823-01A-11D-A35J-05
[["RPPAArray"]] TCGA-P7-A5NX-01A TCGA-P8-A5KC-01A ... TCGA-WB-A81W-01A
[["Mutations"]] TCGA-P7-A5NX-01A-11D-A35D-08 ... TCGA-XG-A823-01A-11D-A35I-08
[["gistica"]] TCGA-P7-A5NY-01A-12D-A35C-01 ... TCGA-XG-A823-01A-11D-A35H-01
[["gistict"]] TCGA-P7-A5NY-01A-12D-A35C-01 ... TCGA-XG-A823-01A-11D-A35H-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.2 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP 22.8 Mb
4          CNVSNP  2.5 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7       Mutations  7.9 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   173 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
   6.00    6.00    1.24    0.26      NA 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  179 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  19.00   35.00   46.00   47.33   58.50   83.00 

vital_status: 
  0   1 
173   6 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   88.0   148.5   452.5   901.0   715.2  3563.0     173 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     352     766    1069    1301    9634       6 

tumor_tissue_site: 
     adrenal gland extra-adrenal site 
               147                 32 

gender: 
female   male 
   101     78 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1988    2009    2011    2010    2012    2013 

radiation_therapy: 
  no  yes NA's 
 172    5    2 

karnofsky_performance_score: 
  70   80   90  100 NA's 
   1    2   13   46  117 

histological_type: 
                                paraganglioma 
                                           18 
paraganglioma; extra-adrenal pheochromocytoma 
                                           13 
                             pheochromocytoma 
                                          148 

number_of_lymph_nodes: 
   0    1    2   13 NA's 
  16    3    1    1  158 

race: 
american indian or alaska native                            asian 
                               1                                6 
       black or african american                            white 
                              20                              148 
                            NA's 
                               4 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     5                    138                     36 

}}
\keyword{datasets}

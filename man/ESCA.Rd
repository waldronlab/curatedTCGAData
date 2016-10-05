\name{ ESCA }
\alias{ ESCA }
\docType{data}
\title{ ESCA }

\details{
\preformatted{
> ESCA
A MultiAssayExperiment object of 10 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 10: 
 [1] RNASeqGene: ExpressionSet with 26120 rows and 198 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 196 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 198 columns 
 [4] CNASNP: RangedRaggedAssay with 203186 rows and 373 columns 
 [5] CNVSNP: RangedRaggedAssay with 60803 rows and 373 columns 
 [6] CNAseq: RangedRaggedAssay with 17059 rows and 104 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 202 columns 
 [8] RPPAArray: ExpressionSet with 192 rows and 126 columns 
 [9] gistica: SummarizedExperiment with 24776 rows and 184 columns 
 [10] gistict: SummarizedExperiment with 24776 rows and 184 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( ESCA )
CharacterList of length 10
[["RNASeqGene"]] AADACL3 AADACL4 AB007962 AB074166 AB1 ... VCY1B VCY XKRY ZFY
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 203180 203181 203182 203183 203184 203185 203186
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 60797 60798 60799 60800 60801 60802 60803
[["CNAseq"]] 1 2 3 4 5 6 7 8 ... 17053 17054 17055 17056 17057 17058 17059
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( ESCA )
CharacterList of length 10
[["RNASeqGene"]] TCGA-2H-A9GF-01A TCGA-2H-A9GG-01A ... TCGA-ZR-A9CJ-01B
[["RNASeq2GeneNorm"]] TCGA-2H-A9GF-01A TCGA-2H-A9GG-01A ... TCGA-ZR-A9CJ-01B
[["miRNASeqGene"]] TCGA-2H-A9GF-01A TCGA-2H-A9GG-01A ... TCGA-ZR-A9CJ-01B
[["CNASNP"]] TCGA-2H-A9GF-01A-11D-A37B-01 ... TCGA-ZR-A9CJ-10A-01D-A389-01
[["CNVSNP"]] TCGA-2H-A9GF-01A-11D-A37B-01 ... TCGA-ZR-A9CJ-10A-01D-A389-01
[["CNAseq"]] TCGA-IG-A3I8-01A-11D-A248-26 ... TCGA-LN-A5U5-10A-01D-A28P-26
[["Methylation"]] TCGA-2H-A9GF-01A-11D-A37D-05 ... TCGA-ZR-A9CJ-01B-11D-A388-05
[["RPPAArray"]] TCGA-2H-A9GF-01A TCGA-2H-A9GI-01A ... TCGA-Z6-AAPN-01A
[["gistica"]] TCGA-2H-A9GF-01A-11D-A37B-01 ... TCGA-ZR-A9CJ-01B-11D-A386-01
[["gistict"]] TCGA-2H-A9GF-01A-11D-A37B-01 ... TCGA-ZR-A9CJ-01B-11D-A386-01

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.5 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 15.6 Mb
5           CNVSNP  4.8 Mb
6           CNAseq  1.3 Mb
7      Methylation 67.4 Mb
8        RPPAArray    0 Mb
9          gistica  4.3 Mb
10         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   108 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 77.000  77.000   0.962   0.677   1.353 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  185 

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
    0.0   223.2   391.0   513.3   633.8  3714.0      57 

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
    esophagus adenocarcinoma  nos esophagus squamous cell carcinoma 
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

}}
\keyword{datasets}

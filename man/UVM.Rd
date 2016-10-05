\name{ UVM }
\alias{ UVM }
\docType{data}
\title{ UVM }

\details{
\preformatted{
> UVM
A MultiAssayExperiment object of 10 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 10: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 80 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 80 columns 
 [3] CNASNP: RangedRaggedAssay with 69487 rows and 160 columns 
 [4] CNVSNP: RangedRaggedAssay with 12973 rows and 160 columns 
 [5] CNAseq: RangedRaggedAssay with 6496 rows and 102 columns 
 [6] Methylation: SummarizedExperiment with 485577 rows and 80 columns 
 [7] RPPAArray: ExpressionSet with 192 rows and 12 columns 
 [8] Mutations: RangedRaggedAssay with 2174 rows and 80 columns 
 [9] gistica: SummarizedExperiment with 24776 rows and 80 columns 
 [10] gistict: SummarizedExperiment with 24776 rows and 80 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( UVM )
CharacterList of length 10
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 8 ... 69481 69482 69483 69484 69485 69486 69487
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 12967 12968 12969 12970 12971 12972 12973
[["CNAseq"]] 1 2 3 4 5 6 7 8 9 ... 6488 6489 6490 6491 6492 6493 6494 6495 6496
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] PPP4R1 POU4F1 DNAH8 LILRA2 MTOR ... AGAP2 UBR2 SF3A2 SCAF1 BAP1
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( UVM )
CharacterList of length 10
[["RNASeq2GeneNorm"]] TCGA-RZ-AB0B-01A TCGA-V3-A9ZX-01A ... TCGA-YZ-A985-01A
[["miRNASeqGene"]] TCGA-RZ-AB0B-01A TCGA-V3-A9ZX-01A ... TCGA-YZ-A985-01A
[["CNASNP"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ... TCGA-YZ-A985-10A-01D-A39Y-01
[["CNVSNP"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ... TCGA-YZ-A985-10A-01D-A39Y-01
[["CNAseq"]] TCGA-RZ-AB0B-01A-11D-A40D-26 ... TCGA-VD-AA8P-10A-01D-A40E-26
[["Methylation"]] TCGA-RZ-AB0B-01A-11D-A39X-05 ... TCGA-YZ-A985-01A-11D-A39X-05
[["RPPAArray"]] TCGA-V3-A9ZX-01A TCGA-V4-A9E9-01A ... TCGA-WC-AA9E-01A
[["Mutations"]] TCGA-RZ-AB0B-01A-11D-A39W-08 ... TCGA-YZ-A985-01A-11D-A39W-08
[["gistica"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ... TCGA-YZ-A985-01A-11D-A39V-01
[["gistict"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ... TCGA-YZ-A985-01A-11D-A39V-01

Sizes of each ExperimentList element:

             assay size.Mb
1  RNASeq2GeneNorm  1.1 Mb
2     miRNASeqGene  0.1 Mb
3           CNASNP  5.4 Mb
4           CNVSNP    1 Mb
5           CNAseq  0.5 Mb
6      Methylation 67.4 Mb
7        RPPAArray    0 Mb
8        Mutations 11.3 Mb
9          gistica  4.3 Mb
10         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   66 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  14.00   14.00    1.11    0.66    3.82 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
   80 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  22.00   51.00   61.50   61.65   74.25   86.00 

vital_status: 
 0  1 
66 14 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   172.0   406.5   517.9   661.2  1581.0      66 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   299.5   618.0   636.1   889.0  2600.0      13 

tumor_tissue_site: 
choroid 
     80 



pathology_N_stage: 
  n0   nx NA's 
  52   27    1 

pathology_M_stage: 
  m0   m1  m1b   mx NA's 
  51    2    2   23    2 

gender: 
female   male 
    35     45 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2007    2011    2012    2012    2013    2013 

radiation_therapy: 
  no  yes NA's 
  76    3    1 

race: 
white  NA's 
   55    25 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     1                     52                     27 

}}
\keyword{datasets}

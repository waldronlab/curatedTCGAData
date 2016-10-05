\name{ THCA }
\alias{ THCA }
\docType{data}
\title{ THCA }

\details{
\preformatted{
> THCA
A MultiAssayExperiment object of 11 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 11: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 4 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 568 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 569 columns 
 [4] CNASNP: RangedRaggedAssay with 389998 rows and 1013 columns 
 [5] CNVSNP: RangedRaggedAssay with 55117 rows and 1013 columns 
 [6] CNAseq: RangedRaggedAssay with 5953 rows and 203 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 567 columns 
 [8] RPPAArray: ExpressionSet with 175 rows and 224 columns 
 [9] Mutations: RangedRaggedAssay with 7458 rows and 405 columns 
 [10] gistica: SummarizedExperiment with 24776 rows and 499 columns 
 [11] gistict: SummarizedExperiment with 24776 rows and 499 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( THCA )
CharacterList of length 11
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 389992 389993 389994 389995 389996 389997 389998
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 55111 55112 55113 55114 55115 55116 55117
[["CNAseq"]] 1 2 3 4 5 6 7 8 9 ... 5945 5946 5947 5948 5949 5950 5951 5952 5953
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_epsilon 4E-BP1 ... p70S6K_pT389 p90RSK_pT359_S363
[["Mutations"]] HTR3C TEP1 C18orf62 SSFA2 VN1R1 ... CSMD1 BRAF TGFBI RSPH6A
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
...
<1 more element>

> colnames( THCA )
CharacterList of length 11
[["RNASeqGene"]] TCGA-DJ-A1QE-01A TCGA-DJ-A1QL-01A TCGA-DO-A1JZ-11A TCGA-EM-A1YB-01A
[["RNASeq2GeneNorm"]] TCGA-4C-A93U-01A TCGA-BJ-A0YZ-01A ... TCGA-QD-A8IV-01A
[["miRNASeqGene"]] TCGA-4C-A93U-01A TCGA-BJ-A0YZ-01A ... TCGA-QD-A8IV-01A
[["CNASNP"]] TCGA-4C-A93U-01A-11D-A396-01 ... TCGA-QD-A8IV-10A-01D-A399-01
[["CNVSNP"]] TCGA-4C-A93U-01A-11D-A396-01 ... TCGA-QD-A8IV-10A-01D-A399-01
[["CNAseq"]] TCGA-BJ-A0Z2-01A-11D-A10R-02 ... TCGA-J8-A3YH-10A-01D-A23Q-26
[["Methylation"]] TCGA-4C-A93U-01A-11D-A398-05 ... TCGA-QD-A8IV-01A-11D-A398-05
[["RPPAArray"]] TCGA-BJ-A0YZ-01A TCGA-BJ-A0Z0-01A ... TCGA-J8-A3O1-01A
[["Mutations"]] TCGA-BJ-A0YZ-01A-11D-A10S-08 ... TCGA-MK-A4N9-01A-11D-A257-08
[["gistica"]] TCGA-4C-A93U-01A-11D-A396-01 ... TCGA-QD-A8IV-01A-11D-A396-01
...
<1 more element>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.1 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.2 Mb
4           CNASNP 30.1 Mb
5           CNVSNP  4.5 Mb
6           CNAseq  0.5 Mb
7      Methylation 67.5 Mb
8        RPPAArray  0.1 Mb
9        Mutations 13.2 Mb
10         gistica  4.4 Mb
11         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   487 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  16.00   16.00    2.80    2.23    4.80 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  503 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.00   35.00   46.00   47.26   58.00   89.00 

vital_status: 
  0   1 
487  16 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    174     743    1021    1176    1631    2973     487 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     519     931    1207    1512    5423      14 

tumor_tissue_site: 
thyroid 
    503 



pathology_N_stage: 
 n0  n1 n1a n1b  nx 
227  58  93  75  50 

pathology_M_stage: 
  m0   m1   mx NA's 
 280    9  213    1 

gender: 
female   male 
   368    135 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1993    2008    2010    2009    2011    2013 

radiation_therapy: 
  no  yes NA's 
 181  306   16 

histological_type: 
                                                        other  specify 
                                                                     7 
                         thyroid papillary carcinoma - classical/usual 
                                                                   358 
thyroid papillary carcinoma - follicular (>= 99% follicular patterned) 
                                                                   102 
   thyroid papillary carcinoma - tall cell (>= 50% tall cell features) 
                                                                    36 

radiation_exposure: 
  no  yes NA's 
 423   17   63 

extrathyroidal_extension: 
           minimal (t3) moderate/advanced (t4a)                    none 
                    133                      18                     333 
    very advanced (t4b)                    NA's 
                      1                      18 

residual_tumor: 
  r0   r1   r2   rx NA's 
 385   52    4   30   32 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   3.658   5.000  41.000     114 

multifocality: 
multifocal   unifocal       NA's 
       227        266         10 

tumor_size: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.700   2.600   2.975   4.000   8.200     100 

race: 
american indian or alaska native                            asian 
                               1                               52 
       black or african american                            white 
                              27                              331 
                            NA's 
                              92 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    38                    362                    103 

}}
\keyword{datasets}
